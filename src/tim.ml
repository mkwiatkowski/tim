open Core.Std

let report records _ goal =
  printf "%s" (TimSummary.summary records goal)

let start records file time =
  let open TimRecord in
  match records with
  | [] | {start = _; stop = Some _} :: _ ->
     TimRecord.save_to_file ((TimRecord.make time None)::records) file;
     printf "Timer started at %s.\n" (TimDate.format_time time)
  | {start = start; stop = None} :: _ ->
     Printf.exitf "Timer already started at %s." (TimDate.format_time start) ()

let stop records file time =
  let open TimRecord in
  match records with
  | [] | {start = _; stop = Some _} :: _ ->
     Printf.exitf "Timer hasn't been started yet." ()
  | {start = start; stop = None} :: rest ->
     let updated = TimRecord.make start (Some time) in
     TimRecord.save_to_file (updated::rest) file;
     printf "Timer stopped at %s.\n" (TimDate.format_time time)

let time_arg =
  Command.Spec.Arg_type.create
    (fun str ->
      match TimDate.time_of_string_after TimDate.today str with
      | Some time -> time
      | None -> Printf.exitf "Invalid time format." ()
    )

let time_spec =
  Command.Spec.(
    empty
    +> anon (maybe_with_default (Time.now ()) ("time" %: time_arg))
  )

let command summary common_args func =
  Command.basic
    ~summary:summary
    Command.Spec.(
      empty
      +> flag "-f" (required file) ~doc:"file Specify path to the storage file"
      ++ common_args
    )
    (fun file argval () ->
      let records = TimRecord.read_from_file file in
      func records file argval
    )

let reportCommand =
  command "Show report of worked time"
          Command.Spec.(empty +> flag "-g" (required int) ~doc:"goal Daily hours goal (used to calculate completion percentage)")
          report

let startCommand =
  command "Start tracking time" time_spec start

let stopCommand =
  command "Stop tracking time" time_spec stop

let commandGroup =
  Command.group
    ~summary:"Track work time"
    ["report", reportCommand ; "start", startCommand; "stop", stopCommand]

let () = Command.run commandGroup
