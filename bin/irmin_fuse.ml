open Cmdliner

let main ~readonly ~root ~store ~contents ~hash ~config_path ~mount_path =
  let store', config =
    Irmin_unix.Resolver.load_config ?root ?config_path ?store ?hash ?contents ()
  in
  let (module Store : Irmin.Generic_key.S) =
    Irmin_unix.Resolver.Store.generic_keyed store'
  in
  let config =
    if readonly then
      Irmin.Backend.Conf.add config Irmin_pack.Conf.Key.readonly readonly
    else config
  in
  let string_of_contents =
    match (contents, store) with
    | _, Some "tezos" -> fun x -> Bytes.unsafe_to_string (Obj.magic x)
    | None, _ -> fun x -> Obj.magic x
    | Some "string", _ -> fun x -> Obj.magic x
    | Some _, _ -> fun x -> Irmin.Type.to_string Store.contents_t x
  in
  let module F =
    Irmin_fuse.Make
      (Store)
      (struct
        let config = config
        let string_of_contents = string_of_contents
        let string_of_step (s : Store.step) = Obj.magic s
      end)
  in
  F.main [| "irmin-fuse"; mount_path |]

let main readonly root (store, hash, contents) config_path mount_path () =
  main ~readonly ~root ~store ~contents ~hash ~config_path ~mount_path

let config_path : string option Cmdliner.Term.t =
  let doc = Arg.info ~docv:"PATH" ~doc:"Config path" [ "config" ] in
  Arg.(value & opt (some string) None & doc)

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let root =
  let doc = Arg.info ~doc:"Irmin store path" [ "r"; "root" ] in
  Arg.(value @@ opt (some string) None doc)

let mount_path =
  let doc = Arg.info ~doc:"Mount path" [] in
  Arg.(required @@ pos 0 (some string) None doc)

let readonly =
  let doc =
    Arg.info
      ~doc:
        "Open in read-only mode. This only has an effect when using irmin-pack"
      [ "readonly" ]
  in
  Arg.(value @@ flag doc)

let term =
  Term.(
    const main $ readonly $ root
    $ Irmin_unix.Resolver.Store.term ()
    $ config_path $ mount_path $ setup_log)

let () =
  let info = Term.info "irmin-fuse" in
  Term.exit @@ Term.eval (term, info)
