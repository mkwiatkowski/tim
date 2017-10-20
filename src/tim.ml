open Core

let parse_time_or_now parse time_str_opt =
  match time_str_opt with
  | Some time_str -> (match parse time_str with
                     | Some time -> time
                     | None -> Printf.exitf "Invalid time format." ())
  | None -> Time.now ()

let default_project =
  "default"

let report records _ _ goal =
  printf "%s" (TimSummary.summary records goal)

let start records file project time_str_opt =
  let open TimRecord in
  let time =
    parse_time_or_now (TimDate.time_of_string_before (Time.now ())) time_str_opt in
  let save () =
    TimRecord.save_to_file ((TimRecord.make project time None)::records) file;
    printf "Timer started at %s.\n" (TimDate.format_time time) in
  match records with
  | {project = _; start = start; stop = None} :: _ ->
     Printf.exitf "Timer already started at %s." (TimDate.format_time start) ()
  | {project = _; start = _; stop = Some stop} :: _ ->
     if time < stop then
       Printf.exitf "Timer can't be started before the time it was last stopped at %s." (TimDate.format_time stop) ()
     else
       save ()
  | [] ->
     save ()

let stop records file project (time_str_opt: string option) =
  let open TimRecord in
  match records with
  | [] | {project = _; start = _; stop = Some _} :: _ ->
     Printf.exitf "Timer hasn't been started yet." ()
  | {project = oldProject; start = start; stop = None} :: rest ->
     let time = parse_time_or_now (TimDate.time_of_string_after start) time_str_opt in
     let newProject = if project = default_project then oldProject else project in
     let updated = TimRecord.make newProject start (Some time) in
     TimRecord.save_to_file (updated::rest) file;
     printf "Timer stopped at %s.\n" (TimDate.format_time time)

let time_spec =
  Command.Spec.(
    empty
    +> anon (maybe ("time" %: string))
  )

let default_daily_goal =
  match Sys.getenv "TIM_DAILY_GOAL" with
  | Some s -> int_of_string s
  | None -> 6

let default_json_location =
  match Sys.getenv "TIM_JSON_PATH" with
  | Some path ->
     path
  | None ->
     let home = match Sys.getenv "HOME" with
       | Some p -> p
       | None -> "." in
     Printf.sprintf "%s/.tim.json" home

let readme =
  String.concat
    ~sep:"\n"
    [ "Specify daily hours goal by setting TIM_DAILY_GOAL."
    ; "It is used to calculate completion percentage."
    ; "Specify path to tim data file with TIM_JSON_PATH."
    ]

let command summary additional_args func =
  Command.basic
    ~summary:summary
    ~readme:(fun () -> readme)
    Command.Spec.(
      empty
      +> flag "-f" (optional_with_default default_json_location file) ~doc:"file Specify path to the storage file"
      +> flag "-p" (optional_with_default default_project string) ~doc:"project Specify name of the project"
      ++ additional_args
    )
    (fun file project argval () ->
      let records = TimRecord.read_from_file file in
      func records file project argval
    )

let report_command =
  command "Show report of worked time"
          Command.Spec.(empty +> flag "-g" (optional_with_default default_daily_goal int)
                                      ~doc:"goal Daily hours goal (used to calculate completion percentage)")
          report

let start_command =
  command "Start tracking time" time_spec start

let stop_command =
  command "Stop tracking time" time_spec stop

let command_group =
  Command.group
    ~summary:"Track work time"
    ["report", report_command ; "start", start_command; "stop", stop_command]

let add_default_command_if_empty args =
  if List.length args <= 1 then
    args @ ["report"]
  else
    args

let () = Command.run ~argv:(add_default_command_if_empty(Array.to_list Sys.argv)) command_group
