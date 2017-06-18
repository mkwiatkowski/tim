open Core.Std

let parse_time_or_now parse timeStrOpt =
  match timeStrOpt with
  | Some timeStr -> (match parse timeStr with
                     | Some time -> time
                     | None -> Printf.exitf "Invalid time format." ())
  | None -> Time.now ()

let report records _ goal =
  printf "%s" (TimSummary.summary records goal)

let start records file timeStrOpt =
  let open TimRecord in
  let time =
    parse_time_or_now (TimDate.time_of_string_before (Time.now ())) timeStrOpt in
  let save () =
    TimRecord.save_to_file ((TimRecord.make time None)::records) file;
    printf "Timer started at %s.\n" (TimDate.format_time time) in
  match records with
  | {start = start; stop = None} :: _ ->
     Printf.exitf "Timer already started at %s." (TimDate.format_time start) ()
  | {start = _; stop = Some stop} :: _ ->
     if time < stop then
       Printf.exitf "Timer can't be started before the time it was last stopped at %s." (TimDate.format_time stop) ()
     else
       save ()
  | [] ->
     save ()

let stop records file (timeStrOpt: string option) =
  let open TimRecord in
  match records with
  | [] | {start = _; stop = Some _} :: _ ->
     Printf.exitf "Timer hasn't been started yet." ()
  | {start = start; stop = None} :: rest ->
     let time = parse_time_or_now (TimDate.time_of_string_after start) timeStrOpt in
     let updated = TimRecord.make start (Some time) in
     TimRecord.save_to_file (updated::rest) file;
     printf "Timer stopped at %s.\n" (TimDate.format_time time)

let time_spec =
  Command.Spec.(
    empty
    +> anon (maybe ("time" %: string))
  )

let defaultJsonLocation =
  let home = match Sys.getenv "HOME" with
    | Some p -> p
    | None -> "." in
  Printf.sprintf "%s/.tim.json" home

let command summary additional_args func =
  Command.basic
    ~summary:summary
    Command.Spec.(
      empty
      +> flag "-f" (optional_with_default defaultJsonLocation file) ~doc:"file Specify path to the storage file"
      ++ additional_args
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
