open Core.Std

let command =
  Command.basic
    ~summary:"Track work time"
    Command.Spec.(
      empty
      +> flag "-f" (required file) ~doc:"file Specify path to the storage file"
      +> flag "-g" (required int) ~doc:"goal Daily hours goal (used to calculate completion percentage)"
    )
    (fun file goal () ->
      let records = TimRecord.read_from_file file in
      printf "%s" (TimSummary.summary records goal)
    )

let () = Command.run command
