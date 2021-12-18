open! Bigarray
open Unix
open Lwt.Syntax
open Lwt.Infix
include Irmin_fuse_intf

let default_stats = LargeFile.stat "."

module Make
    (Store : Irmin.Generic_key.S)
    (C : Conf with type contents := Store.contents and type step := Store.step) =
struct
  let store =
    let* repo = Store.Repo.v C.config in
    let* branches = Store.Branch.list repo in
    Store.of_branch repo (List.hd branches)

  let do_getattr p =
    Lwt_main.run
      (let path = Irmin.Type.of_string Store.Path.t p |> Result.get_ok in
       let* store in
       let* is_file = Store.mem store path in
       let* is_dir = Store.mem_tree store path in
       if is_file then
         let* c = Store.last_modified store path >|= List.hd in
         let+ contents = Store.get store path >|= C.string_of_contents in
         let length = String.length contents in
         {
           default_stats with
           st_nlink = 1;
           st_kind = S_REG;
           st_perm = 0o444;
           st_size = Int64.of_int length;
           st_mtime = Store.Info.date (Store.Commit.info c) |> Int64.to_float;
         }
       else if is_dir then
         Lwt.return
           { default_stats with st_kind = S_DIR; st_mtime = Unix.time () }
       else raise (Unix_error (ENOENT, "stat", p)))

  let do_readdir p _ =
    Lwt_main.run
      (let* store in
       let path = Irmin.Type.of_string Store.Path.t p |> Result.get_ok in
       let* is_dir = Store.mem_tree store path in
       if is_dir then
         let+ files = Store.list store path in
         let files =
           List.map (fun (a, _) -> Irmin.Type.to_string Store.step_t a) files
         in
         let files = "." :: ".." :: files in
         files
       else raise (Unix_error (ENOENT, "readdir", p)))

  let do_fopen p _flags =
    Lwt_main.run
      (let* store in
       let path = Irmin.Type.of_string Store.Path.t p |> Result.get_ok in
       let+ v = Store.mem store path in
       if v then None else raise (Unix_error (ENOENT, "open", p)))

  let do_read p buf ofs _ =
    Lwt_main.run
      (let* store in
       let path = Irmin.Type.of_string Store.Path.t p |> Result.get_ok in
       let* v = Store.find store path in
       match v with
       | None -> raise (Unix_error (ENOENT, "read", p))
       | Some x ->
           if ofs > Int64.of_int max_int then Lwt.return 0
           else
             let x = C.string_of_contents x in
             let ofs = Int64.to_int ofs in
             let len = String.length x - ofs in
             let () =
               Bigstringaf.blit_from_string ~src_off:ofs x buf ~dst_off:0 ~len
             in
             Lwt.return len)

  let main args : unit =
    Fuse.(
      main args
        {
          default_operations with
          getattr = do_getattr;
          readdir = do_readdir;
          fopen = do_fopen;
          read = do_read;
        })
end
