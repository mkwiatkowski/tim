open Core.Std

type t

val start : t -> Time.t
val stop : t -> Time.t
val read_from_file : string -> t list
