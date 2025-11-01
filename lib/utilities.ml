open Batteries

let read_input fname =
  File.lines_of fname |> List.of_enum
