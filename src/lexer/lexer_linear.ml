(** Linear Notation Lexer for Plankalkül

    Tokenises the linearised (horizontal) notation used in modern
    implementations. The original 2D notation is handled separately.

    Ported from plankalkul.py (Zane Hambly, 2025) with adaptations
    for OCaml idioms and the compiler context.

    Attribution: Original lexer patterns from Zane Hambly's interpreter,
    building on Rojas et al., FU Berlin (2000).
*)

open Tokens

(** Lexer state *)
type lexer_state = {
  source: string;
  mutable pos: int;
  mutable line: int;
  mutable column: int;
  file: string;
}

(** Create a new lexer for the given source *)
let create ?(file="<input>") source =
  (* Normalise Unicode characters to ASCII equivalents *)
  let normalised_source = Tokens.normalise_unicode source in
  {
    source = normalised_source;
    pos = 0;
    line = 1;
    column = 1;
    file;
  }

(** Get current character, or None if at end *)
let current_char state =
  if state.pos >= String.length state.source then None
  else Some (String.get state.source state.pos)

(** Peek ahead by offset characters *)
let peek state offset =
  let pos = state.pos + offset in
  if pos >= String.length state.source then None
  else Some (String.get state.source pos)

(** Advance one character and return it *)
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

(** Current location *)
let current_loc state = {
  file = state.file;
  line = state.line;
  column = state.column;
}

(** Skip whitespace (but not newlines - they can be significant) *)
let rec skip_whitespace state =
  match current_char state with
  | Some (' ' | '\t' | '\r') -> ignore (advance state); skip_whitespace state
  | _ -> ()

(** Skip comment (from ; to end of line, or (* ... *) style) *)
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
  | None, _ -> () (* Unclosed comment - could error here *)
  | _, _ -> ignore (advance state); skip_block_comment state depth

(** Read an integer or float *)
let read_number state =
  let loc = current_loc state in
  let buf = Buffer.create 16 in
  let rec collect_digits () =
    match current_char state with
    | Some c when c >= '0' && c <= '9' ->
      Buffer.add_char buf c;
      ignore (advance state);
      collect_digits ()
    | Some '.' when peek state 1 <> Some '.' -> (* Avoid consuming .. *)
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

(** Check if character starts a variable (V, Z, R) *)
let is_var_start c = c = 'V' || c = 'v' || c = 'Z' || c = 'z' || c = 'R' || c = 'r'

(** Read an identifier or keyword *)
let read_identifier state =
  let loc = current_loc state in
  let buf = Buffer.create 16 in
  let rec collect () =
    let len = Buffer.length buf in
    match current_char state with
    (* Special case: 'u' or 'U' followed by variable letter = mu iterator *)
    | Some c when len = 1 &&
        (Buffer.contents buf = "u" || Buffer.contents buf = "U") &&
        is_var_start c ->
      ()  (* Stop here - 'u' becomes MU token, variable follows *)
    (* Special case: 'E' or 'e' followed by variable letter = exists quantifier *)
    | Some c when len = 1 &&
        (Buffer.contents buf = "e" || Buffer.contents buf = "E") &&
        is_var_start c ->
      ()  (* Stop here - 'E' becomes IDENT "E", variable follows *)
    (* Special case: 'P' or 'p' followed by digit = plan reference *)
    | Some c when len = 1 &&
        (Buffer.contents buf = "p" || Buffer.contents buf = "P") &&
        (c >= '0' && c <= '9') ->
      ()  (* Stop here - 'P' becomes P token, number follows *)
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

  (* Check for keywords and special identifiers *)
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
    | "TABLE" -> TABLE   (* Truth table declaration (ZIA-0368 pp.20-21) *)
    (* Greek letter plan group prefixes - from Zuse's manuscripts *)
    | "DELTA" -> DELTA   (* PΔ300 notation for chess plans *)
    | "SIGMA" -> SIGMA   (* PΣ notation *)
    | "PHI" -> PHI       (* PΦ notation *)
    (* Cardinality: N followed by ( *)
    | "N" when current_char state = Some '(' -> CARDINALITY
    (* Mu iterator: u followed by variable - checked in collect() above *)
    | "U" when (match current_char state with
                | Some c -> is_var_start c
                | None -> false) -> MU
    | _ ->
      (* Check for variable: V0, Z1, R2, etc. *)
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

(** Read a type annotation like [:8.0] or [:m*X] *)
let read_type_annotation state =
  let loc = current_loc state in
  let buf = Buffer.create 16 in
  Buffer.add_char buf '[';
  ignore (advance state); (* consume [ *)
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
      depth := 0 (* Unterminated - will error later *)
  done;
  let lexeme = Buffer.contents buf in
  make_token (TYPE_ANNOT lexeme) lexeme loc

(** Read the next token *)
let rec next_token state =
  skip_whitespace state;
  let loc = current_loc state in

  match current_char state with
  | None -> make_token EOF "" loc

  (* Comments *)
  | Some ';' ->
    skip_line_comment state;
    make_token NEWLINE ";" loc (* Treat comment end as newline *)

  | Some '(' when peek state 1 = Some '*' ->
    ignore (advance state); ignore (advance state);
    skip_block_comment state 1;
    next_token state (* Skip and get next real token *)

  (* Newlines *)
  | Some '\n' ->
    ignore (advance state);
    make_token NEWLINE "\n" loc

  (* Numbers *)
  | Some c when c >= '0' && c <= '9' ->
    read_number state

  (* Identifiers and keywords *)
  | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
    read_identifier state

  (* Type annotations: [: *)
  | Some '[' when peek state 1 = Some ':' ->
    read_type_annotation state

  (* Two-character operators *)
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

  (* Backslash for lambda iterator *)
  | Some '\\' ->
    ignore (advance state);
    make_token LAMBDA "\\" loc

  (* Single-character tokens *)
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

  (* Unknown character *)
  | Some c ->
    let msg = Printf.sprintf "Unexpected character '%c' at line %d, column %d"
                c state.line state.column in
    ignore (advance state);
    make_token (ERROR msg) (String.make 1 c) loc

(** Tokenise entire source, returning list of tokens *)
let tokenise state =
  let rec collect acc =
    let tok = next_token state in
    match tok.typ with
    | EOF -> List.rev (tok :: acc)
    | _ -> collect (tok :: acc)
  in
  collect []

(** Tokenise, filtering out newlines (for parsers that don't care) *)
let tokenise_no_newlines state =
  tokenise state
  |> List.filter (fun tok -> tok.typ <> NEWLINE)

(** Convenience: tokenise a string directly *)
let tokenise_string ?(file="<string>") source =
  let state = create ~file source in
  tokenise state
