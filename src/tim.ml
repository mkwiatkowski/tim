open Core.Std

let perror message =
  print_string message;
  exit 1

let is_timer_running = function
  | [] ->
     false
  | hd :: _ ->
     match TimRecord.stop hd with
     | Some _ -> false
     | None -> true

let report records goal =
  printf "%s" (TimSummary.summary records goal)

let start records file =
  if is_timer_running records then
    let head :: _ = records in
    printf "Timer already started at %s.\n" (TimDate.format_time (TimRecord.start head))
  else
    let now = Time.now () in
    TimRecord.save_to_file ((TimRecord.make now None)::records) file;
    printf "Timer started at %s.\n" (TimDate.format_time now)

let stop records file =
  if not (is_timer_running records) then
    perror "Timer hasn't been started yet.\n"
  else
    let now = Time.now () in
    let head :: rest = records in
    let updated = TimRecord.make (TimRecord.start head) (Some (Time.now ())) in
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
