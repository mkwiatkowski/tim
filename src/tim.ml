open Core.Std

let () =
  let records = TimRecord.read_from_file TimConfig.tim_file_path in
  printf "%s" (TimSummary.summary records)
