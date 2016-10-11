open Core.Std

val to_date : Time.t -> Date.t
val is_today : Time.t -> bool
val is_this_week : Time.t -> bool
val is_this_month : Time.t -> bool
val is_last_month : Time.t -> bool
val this_month_days : Date.t list
val this_month_work_days_number : int
val this_month_work_days_so_far : int
