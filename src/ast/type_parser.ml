open Types

let starts_with prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

let parse_s1_type s =
  if starts_with "S1." s && String.length s > 3 then
    try
      let n_str = String.sub s 3 (String.length s - 3) in
      let n = int_of_string (String.trim n_str) in
      Some (TPrimitive (BitWidth n))
    with Failure _ -> None
  else
    None

let rec parse_type_string s =
  let s = String.trim s in
  match s with
  | "" | "_" -> TInferred

  | "0" -> TPrimitive Bit

  | "S0" -> TPrimitive SignBit

  | "i" -> TNumeric A10

  | "b" -> TPrimitive Bit

  | "f" -> TNumeric A12

  | "c" -> TNumeric A13

  | "A8" -> TNumeric A8
  | "A9" -> TNumeric A9
  | "A10" -> TNumeric A10
  | "A11" -> TNumeric A11
  | "A12" -> TNumeric A12
  | "A13" -> TNumeric A13

  | s when starts_with "S1." s ->
    (match parse_s1_type s with
     | Some t -> t
     | None -> TInferred)

  | s when String.contains s '.' -> parse_array_type s

  | s when String.contains s '*' -> parse_multi_array_type s

  | s when String.length s > 0 && s.[0] = '(' -> parse_tuple_type s

  | _ -> TInferred

and parse_array_type s =
  try
    let dot_pos = String.index s '.' in
    let size_str = String.sub s 0 dot_pos in
    let elem_str = String.sub s (dot_pos + 1) (String.length s - dot_pos - 1) in
    let size = int_of_string (String.trim size_str) in
    let elem_type = parse_type_string elem_str in
    TArray (size, elem_type)
  with
  | Not_found | Failure _ | Invalid_argument _ -> TInferred

and parse_multi_array_type s =
  try
    let parts = String.split_on_char '*' s in
    let rec parse_parts dims = function
      | [] -> TInferred
      | [elem_type_str] ->
        let elem_type = parse_type_string (String.trim elem_type_str) in
        (match List.rev dims with
         | [] -> elem_type
         | [n] -> TArray (n, elem_type)
         | ns -> TMultiArray (ns, elem_type))
      | dim_str :: rest ->
        let dim = int_of_string (String.trim dim_str) in
        parse_parts (dim :: dims) rest
    in
    parse_parts [] parts
  with
  | Failure _ | Invalid_argument _ -> TInferred

and parse_tuple_type s =
  try
    let len = String.length s in
    if len < 2 || s.[0] <> '(' || s.[len - 1] <> ')' then
      TInferred
    else
      let inner = String.sub s 1 (len - 2) in
      let parts = split_tuple_parts inner in
      let types = List.map parse_type_string parts in
      TTuple types
  with
  | Invalid_argument _ -> TInferred

and split_tuple_parts s =
  let rec loop acc current depth i =
    if i >= String.length s then
      List.rev (String.trim current :: acc)
    else
      let c = s.[i] in
      match c with
      | '(' -> loop acc (current ^ String.make 1 c) (depth + 1) (i + 1)
      | ')' -> loop acc (current ^ String.make 1 c) (depth - 1) (i + 1)
      | ',' when depth = 0 ->
        loop (String.trim current :: acc) "" 0 (i + 1)
      | _ -> loop acc (current ^ String.make 1 c) depth (i + 1)
  in
  loop [] "" 0 0

let parse_type_annotation lexeme =
  let s = String.trim lexeme in
  let len = String.length s in
  if len >= 3 && s.[0] = '[' && s.[1] = ':' && s.[len - 1] = ']' then
    let inner = String.sub s 2 (len - 3) in
    parse_type_string inner
  else if len >= 2 && s.[0] = '[' && s.[len - 1] = ']' then
    let inner = String.sub s 1 (len - 2) in
    parse_type_string inner
  else
    parse_type_string s
