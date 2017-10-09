open Core.Std

type t =
  {
    project: string;
    start : Time.t;
    stop : Time.t option;
  }

val make : string -> Time.t -> Time.t option -> t
val read_from_file : string -> t list
val save_to_file : t list -> string -> unit
