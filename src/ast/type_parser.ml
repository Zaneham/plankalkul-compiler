(** Type Annotation Parser for Plankalkul

    Parses type annotations from two formats:
    1. Linear notation: [:i], [:S0], [:8.0]
    2. S row (2D) notation: i, 0, S0, 8.0

    Type syntax:
    - "i" or "A10" -> Integer (signed)
    - "0" -> Single bit
    - "S0" -> Sign bit
    - "A8" through "A13" -> Numeric types
    - "n.t" -> Array of n elements of type t (e.g., "8.0" = 8 bits)
    - "(t1,t2,...)" -> Tuple type
*)

open Types

(** Check if string starts with prefix *)
let starts_with prefix s =
  let plen = String.length prefix in
  String.length s >= plen && String.sub s 0 plen = prefix

(** Parse S1.n notation - historical n-bit integer format *)
let parse_s1_type s =
  (* Format: S1.n where n is bit width *)
  if starts_with "S1." s && String.length s > 3 then
    try
      let n_str = String.sub s 3 (String.length s - 3) in
      let n = int_of_string (String.trim n_str) in
      Some (TPrimitive (BitWidth n))
    with Failure _ -> None
  else
    None

(** Parse a type string (without [:...] wrapper) *)
let rec parse_type_string s =
  let s = String.trim s in
  match s with
  (* Empty or placeholder *)
  | "" | "_" -> TInferred

  (* Single bit *)
  | "0" -> TPrimitive Bit

  (* Sign bit *)
  | "S0" -> TPrimitive SignBit

  (* Integer shorthand *)
  | "i" -> TNumeric A10

  (* Boolean shorthand *)
  | "b" -> TPrimitive Bit

  (* Float shorthand *)
  | "f" -> TNumeric A12

  (* Complex shorthand *)
  | "c" -> TNumeric A13

  (* Explicit numeric types A8-A13 *)
  | "A8" -> TNumeric A8
  | "A9" -> TNumeric A9
  | "A10" -> TNumeric A10
  | "A11" -> TNumeric A11
  | "A12" -> TNumeric A12
  | "A13" -> TNumeric A13

  (* S1.n notation - n-bit integer (historical Zuse format) *)
  | s when starts_with "S1." s ->
    (match parse_s1_type s with
     | Some t -> t
     | None -> TInferred)

  (* Array type: "n.t" where n is size and t is element type *)
  | s when String.contains s '.' -> parse_array_type s

  (* Multi-dimensional array: "m*n*t" format (historical) *)
  | s when String.contains s '*' -> parse_multi_array_type s

  (* Tuple type: "(t1,t2,...)" *)
  | s when String.length s > 0 && s.[0] = '(' -> parse_tuple_type s

  (* Unknown - default to inferred *)
  | _ -> TInferred

(** Parse array type notation: "8.0" -> TArray(8, Bit) *)
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

(** Parse multi-dimensional array: "8*8*i" -> TMultiArray([8;8], A10)
    Historical Zuse format for arrays like chess boards *)
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

(** Parse tuple type notation: "(i,0)" -> TTuple [TNumeric A10; TPrimitive Bit] *)
and parse_tuple_type s =
  try
    (* Strip outer parentheses *)
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

(** Split tuple parts by comma, respecting nested parentheses *)
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

(** Parse a type annotation token (with [:...] wrapper) *)
let parse_type_annotation lexeme =
  let s = String.trim lexeme in
  let len = String.length s in
  (* Check for [:...] wrapper from linear notation *)
  if len >= 3 && s.[0] = '[' && s.[1] = ':' && s.[len - 1] = ']' then
    let inner = String.sub s 2 (len - 3) in
    parse_type_string inner
  (* Check for [...] wrapper (older format) *)
  else if len >= 2 && s.[0] = '[' && s.[len - 1] = ']' then
    let inner = String.sub s 1 (len - 2) in
    parse_type_string inner
  (* Raw type string (from S row in 2D notation) *)
  else
    parse_type_string s
