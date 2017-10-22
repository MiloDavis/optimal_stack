type token = Id of string
           | Lparen
           | Rparen
           | Colon

type values = Var of string | Pair of values * values

(* This is a total hack *)
let split =
  Str.full_split (Str.regexp "[A-Za-z0-9]+\\|(\\|)\\|:")

let rec to_tokens =
  let open Str in
  function
  | [] -> []
  | Delim d :: tl ->
      (match d with
       | "(" -> Lparen
       | ")" -> Rparen
       | ":" -> Colon
       | id -> Id id) :: (to_tokens tl)
  | Text _ :: tl -> to_tokens tl

let lex s =
  to_tokens @@ split s

let rec parse_item = function
  | Id x :: tl -> (Var x, tl)
  | Lparen :: tl ->
      let (first, tl) = parse_item tl in
      let (second, tl) = parse_item tl in
      begin match tl with
        | Rparen :: tl -> (Pair (first, second), tl)
        | _ -> failwith "Pair with too many elements"
      end
  | Rparen :: _ -> failwith "Unmatched rparen"
  | Colon :: _ -> failwith "Expected stack item, but found seperator"
  | [] -> failwith "expected stack item"

let rec parse_stack = function
  | [] -> []
  | Colon :: tl -> failwith "Colon should not begin a stack"
  | l ->
      let (item, rest) = parse_item l in
      begin match rest with
        | Colon :: ((_ :: _) as tl) -> item :: (parse_stack tl)
        | Colon :: [] -> failwith "Colon should not end stack"
        | [] -> [ item ]
        | _ -> failwith "Expected seperator, but did not find one"
      end

let parse str =
  parse_stack @@ to_tokens @@ split str

let rec pp_item formatter = function
  | Var x -> Format.fprintf formatter "%s" x
  | Pair (x, y) ->
      Format.fprintf formatter "(%a %a)"
        pp_item x
        pp_item y

let rec pp formatter = function
  | [] -> ()
  | item :: [] -> pp_item formatter item
  | item :: ((_ :: _) as tl) ->
      Format.fprintf formatter "%a : %a" pp_item item pp tl
