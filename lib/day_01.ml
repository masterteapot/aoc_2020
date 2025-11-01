open Batteries
open Utilities
open Printf


let parse_p1 ls =
  let rec aux v next_ls = function
    | [] when List.length next_ls <= 1 -> failwith "no match"
    | [] -> 
        let ls = List.rev next_ls in
        aux (List.hd ls) (List.tl ls) (List.tl ls)
    | hd :: _ when hd + v = 2020 -> hd * v
    | _ :: tl -> aux v next_ls tl
  in
  assert (List.length ls >= 2);
  aux (List.hd ls) (List.tl ls) (List.tl ls)
;;

let parse_p2 ls =
  let rec aux v next_ls = function
    | [] when List.length next_ls <= 1 -> failwith "no match"
    | [] -> 
        let ls = List.rev next_ls in
        aux (List.hd ls) (List.tl ls) (List.tl ls)
    | hd :: _ when hd + v = 2020 -> hd * v
    | _ :: tl -> aux v next_ls tl
  in
  assert (List.length ls >= 2);
  aux (List.hd ls) (List.tl ls) (List.tl ls)
;;

let part_1 () =
  let input = read_input "inputs/day_01.txt" in
  let ls = List.map Int.of_string input in
  sprintf "%d" (parse_p1 ls)

let part_2 () =
  let input = read_input "examples/day_01.txt" in
  let ls = List.map Int.of_string input in
  sprintf "%d" (parse_p1 ls)
