open Core

val today : Date.t
val to_date : Time.t -> Date.t
val is_today : Time.t -> bool
val is_this_week : Time.t -> bool
val is_this_month : Time.t -> bool
val is_last_month : Time.t -> bool
val format_time : Time.t -> string
val local_tz : Time.Zone.t
val this_month_days : Date.t list
val this_month_work_days_number : int
val this_month_work_days_so_far : int
val time_of_float : float -> Time.t
val time_of_string_after : Time.t -> string -> Time.t option
val time_of_string_before : Time.t -> string -> Time.t option
val time_to_float : Time.t -> float
