open Tokens

type lexer_state = {
  source: string;
  mutable pos: int;
  mutable line: int;
  mutable column: int;
  file: string;
}

let create ?(file="<input>") source =
  let normalised_source = Tokens.normalise_unicode source in
  {
    source = normalised_source;
    pos = 0;
    line = 1;
    column = 1;
    file;
  }

let current_char state =
  if state.pos >= String.length state.source then None
  else Some (String.get state.source state.pos)

let peek state offset =
  let pos = state.pos + offset in
  if pos >= String.length state.source then None
  else Some (String.get state.source pos)

let advance state =
  match current_char state with
  | None -> None
  | Some c ->
    state.pos <- state.pos + 1;
    if c = '\n' then begin
      state.line <- state.line + 1;
      state.column <- 1
    end else
      state.column <- state.column + 1;
    Some c

let current_loc state = {
  file = state.file;
  line = state.line;
  column = state.column;
}

let rec skip_whitespace state =
  match current_char state with
  | Some (' ' | '\t' | '\r') -> ignore (advance state); skip_whitespace state
  | _ -> ()

let rec skip_line_comment state =
  match current_char state with
  | None | Some '\n' -> ()
  | _ -> ignore (advance state); skip_line_comment state

let rec skip_block_comment state depth =
  match current_char state, peek state 1 with
  | Some '*', Some ')' ->
    ignore (advance state); ignore (advance state);
    if depth > 1 then skip_block_comment state (depth - 1)
  | Some '(', Some '*' ->
    ignore (advance state); ignore (advance state);
    skip_block_comment state (depth + 1)
  | None, _ -> ()
  | _, _ -> ignore (advance state); skip_block_comment state depth

let read_number state =
  let loc = current_loc state in
  let buf = Buffer.create 16 in
  let rec collect_digits () =
    match current_char state with
    | Some c when c >= '0' && c <= '9' ->
      Buffer.add_char buf c;
      ignore (advance state);
      collect_digits ()
    | Some '.' when peek state 1 <> Some '.' ->
      Buffer.add_char buf '.';
      ignore (advance state);
      collect_digits ()
    | _ -> ()
  in
  collect_digits ();
  let lexeme = Buffer.contents buf in
  if String.contains lexeme '.' then
    make_token (FLOAT (float_of_string lexeme)) lexeme loc
  else
    make_token (INTEGER (int_of_string lexeme)) lexeme loc

let is_var_start c = c = 'V' || c = 'v' || c = 'Z' || c = 'z' || c = 'R' || c = 'r'

let read_identifier state =
  let loc = current_loc state in
  let buf = Buffer.create 16 in
  let rec collect () =
    let len = Buffer.length buf in
    match current_char state with
    | Some c when len = 1 &&
        (Buffer.contents buf = "u" || Buffer.contents buf = "U") &&
        is_var_start c ->
      ()
    | Some c when len = 1 &&
        (Buffer.contents buf = "e" || Buffer.contents buf = "E") &&
        is_var_start c ->
      ()
    | Some c when len = 1 &&
        (Buffer.contents buf = "p" || Buffer.contents buf = "P") &&
        (c >= '0' && c <= '9') ->
      ()
    | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
               || (c >= '0' && c <= '9') || c = '_' ->
      Buffer.add_char buf c;
      ignore (advance state);
      collect ()
    | _ -> ()
  in
  collect ();
  let lexeme = Buffer.contents buf in
  let upper = String.uppercase_ascii lexeme in

  let typ = match upper with
    | "END" -> END
    | "FIN" -> FIN
    | "TRUE" -> BOOLEAN true
    | "FALSE" -> BOOLEAN false
    | "W" -> W
    | "W0" -> W0
    | "W1" -> W1
    | "W2" -> W2
    | "W3" -> W3
    | "W4" -> W4
    | "W5" -> W5
    | "W6" -> W6
    | "P" -> P
    | "TABLE" -> TABLE   (* Truth tables come from ZIA-0368 pp.20-21 *)
    | "DELTA" -> DELTA
    | "SIGMA" -> SIGMA
    | "PHI" -> PHI
    | "N" when current_char state = Some '(' -> CARDINALITY
    | "U" when (match current_char state with
                | Some c -> is_var_start c
                | None -> false) -> MU
    | _ ->
      if String.length lexeme >= 2 then
        let first = String.get lexeme 0 in
        let rest = String.sub lexeme 1 (String.length lexeme - 1) in
        if (first = 'V' || first = 'v') &&
           String.for_all (fun c -> c >= '0' && c <= '9') rest then
          VAR_V (int_of_string rest)
        else if (first = 'Z' || first = 'z') &&
                String.for_all (fun c -> c >= '0' && c <= '9') rest then
          VAR_Z (int_of_string rest)
        else if (first = 'R' || first = 'r') &&
                String.for_all (fun c -> c >= '0' && c <= '9') rest then
          VAR_R (int_of_string rest)
        else
          IDENT lexeme
      else
        IDENT lexeme
  in
  make_token typ lexeme loc

let read_type_annotation state =
  let loc = current_loc state in
  let buf = Buffer.create 16 in
  Buffer.add_char buf '[';
  ignore (advance state);
  let depth = ref 1 in
  while !depth > 0 do
    match current_char state with
    | Some '[' ->
      Buffer.add_char buf '[';
      ignore (advance state);
      incr depth
    | Some ']' ->
      Buffer.add_char buf ']';
      ignore (advance state);
      decr depth
    | Some c ->
      Buffer.add_char buf c;
      ignore (advance state)
    | None ->
      depth := 0
  done;
  let lexeme = Buffer.contents buf in
  make_token (TYPE_ANNOT lexeme) lexeme loc

let rec next_token state =
  skip_whitespace state;
  let loc = current_loc state in

  match current_char state with
  | None -> make_token EOF "" loc

  | Some ';' ->
    skip_line_comment state;
    make_token NEWLINE ";" loc

  | Some '(' when peek state 1 = Some '*' ->
    ignore (advance state); ignore (advance state);
    skip_block_comment state 1;
    next_token state

  | Some '\n' ->
    ignore (advance state);
    make_token NEWLINE "\n" loc

  | Some c when c >= '0' && c <= '9' ->
    read_number state

  | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
    read_identifier state

  | Some '[' when peek state 1 = Some ':' ->
    read_type_annotation state

  | Some '=' when peek state 1 = Some '>' ->
    ignore (advance state); ignore (advance state);
    make_token ARROW "=>" loc

  | Some '-' when peek state 1 = Some '>' ->
    ignore (advance state); ignore (advance state);
    make_token COND_ARROW "->" loc

  | Some '!' when peek state 1 = Some '=' ->
    ignore (advance state); ignore (advance state);
    make_token NEQ "!=" loc

  | Some '/' when peek state 1 = Some '=' ->
    ignore (advance state); ignore (advance state);
    make_token NEQ "/=" loc

  | Some '<' when peek state 1 = Some '=' ->
    ignore (advance state); ignore (advance state);
    make_token LEQ "<=" loc

  | Some '>' when peek state 1 = Some '=' ->
    ignore (advance state); ignore (advance state);
    make_token GEQ ">=" loc

  | Some '<' when peek state 1 = Some '-' && peek state 2 = Some '>' ->
    ignore (advance state); ignore (advance state); ignore (advance state);
    make_token EQUIV "<->" loc

  | Some '^' when peek state 1 = Some '^' ->
    ignore (advance state); ignore (advance state);
    make_token FILTER_SEQ "^^" loc

  | Some '\\' ->
    ignore (advance state);
    make_token LAMBDA "\\" loc

  | Some '+' -> ignore (advance state); make_token PLUS "+" loc
  | Some '-' -> ignore (advance state); make_token MINUS "-" loc
  | Some '*' -> ignore (advance state); make_token MULTIPLY "*" loc
  | Some '/' -> ignore (advance state); make_token DIVIDE "/" loc
  | Some '=' -> ignore (advance state); make_token EQ "=" loc
  | Some '<' -> ignore (advance state); make_token LT "<" loc
  | Some '>' -> ignore (advance state); make_token GT ">" loc
  | Some '&' -> ignore (advance state); make_token AND "&" loc
  | Some '|' -> ignore (advance state); make_token OR "|" loc
  | Some '!' -> ignore (advance state); make_token NOT "!" loc
  | Some '~' -> ignore (advance state); make_token NOT "~" loc
  | Some '(' -> ignore (advance state); make_token LPAREN "(" loc
  | Some ')' -> ignore (advance state); make_token RPAREN ")" loc
  | Some '[' -> ignore (advance state); make_token LBRACKET "[" loc
  | Some ']' -> ignore (advance state); make_token RBRACKET "]" loc
  | Some '{' -> ignore (advance state); make_token LBRACE "{" loc
  | Some '}' -> ignore (advance state); make_token RBRACE "}" loc
  | Some ',' -> ignore (advance state); make_token COMMA "," loc
  | Some ':' -> ignore (advance state); make_token COLON ":" loc
  | Some '.' -> ignore (advance state); make_token DOT "." loc
  | Some '^' -> ignore (advance state); make_token FILTER_SET "^" loc
  | Some '\'' -> ignore (advance state); make_token FIND_UNIQUE "'" loc

  | Some c ->
    let msg = Printf.sprintf "Unexpected character '%c' at line %d, column %d"
                c state.line state.column in
    ignore (advance state);
    make_token (ERROR msg) (String.make 1 c) loc

let tokenise state =
  let rec collect acc =
    let tok = next_token state in
    match tok.typ with
    | EOF -> List.rev (tok :: acc)
    | _ -> collect (tok :: acc)
  in
  collect []

let tokenise_no_newlines state =
  tokenise state
  |> List.filter (fun tok -> tok.typ <> NEWLINE)

let tokenise_string ?(file="<string>") source =
  let state = create ~file source in
  tokenise state
