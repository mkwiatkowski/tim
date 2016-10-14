open Core.Std

let perror fmt = ksprintf (fun s -> print_string s; exit 1) fmt

let report records goal =
  printf "%s" (TimSummary.summary records goal)

let start records file =
  let open TimRecord in
  match records with
  | [] | {start = _; stop = Some _} :: _ ->
     let now = Time.now () in
     TimRecord.save_to_file ((TimRecord.make now None)::records) file;
     printf "Timer started at %s.\n" (TimDate.format_time now)
  | {start = start; stop = None} :: _ ->
     perror "Timer already started at %s.\n" (TimDate.format_time start)

let stop records file =
  let open TimRecord in
  match records with
  | [] | {start = _; stop = Some _} :: _ ->
     perror "Timer hasn't been started yet.\n"
  | {start = start; stop = None} :: rest ->
     let now = Time.now () in
     let updated = TimRecord.make start (Some (Time.now ())) in
     TimRecord.save_to_file (updated::rest) file;
     printf "Timer stopped at %s.\n" (TimDate.format_time now)

let command =
  Command.basic
    ~summary:"Track work time"
    Command.Spec.(
      empty
      +> anon (maybe_with_default "report" ("command" %: string))
      +> flag "-f" (required file) ~doc:"file Specify path to the storage file"
      +> flag "-g" (required int) ~doc:"goal Daily hours goal (used to calculate completion percentage)"
    )
    (fun command file goal () ->
      let records = TimRecord.read_from_file file in
      match command with
      | "report" -> report records goal
      | "start" -> start records file
      | "stop" -> stop records file
      | _ -> ()
    )

let () = Command.run command
