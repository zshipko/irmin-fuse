module type Conf = sig
  type contents
  type step

  val config : Irmin.config
  val string_of_contents : contents -> string
  val string_of_step : step -> string
  (*val contents_of_string : string -> Store.contents*)
end

module type S = sig
  val main : string array -> unit
end

module type Sigs = sig
  module type Conf = Conf
  module type S = S

  module Make
      (Store : Irmin.Generic_key.S)
      (C : Conf with type contents := Store.contents and type step := Store.step) :
    S
end
