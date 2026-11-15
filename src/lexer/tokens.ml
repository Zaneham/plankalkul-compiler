type location = {
  file: string;
  line: int;
  column: int;
}
[@@deriving show, eq]

let dummy_loc = { file = "<unknown>"; line = 0; column = 0 }

type token_type =
  | INTEGER of int
  | FLOAT of float
  | BOOLEAN of bool

  | VAR_V of int
  | VAR_Z of int
  | VAR_R of int

  | IDENT of string
  | TYPE_ANNOT of string

  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE

  | EQ
  | NEQ
  | LT
  | GT
  | LEQ
  | GEQ

  | AND
  | OR
  | NOT
  | IMPL
  | EQUIV

  | ARROW
  | COND_ARROW

  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | SEMICOLON
  | DOT
  | PIPE

  | P
  | TABLE
  | DELTA
  | SIGMA
  | PHI
  | W
  | W0
  | W1
  | W2
  | W3
  | W4
  | W5
  | W6
  | FIN
  | END

  | ROW_V
  | ROW_K
  | ROW_S
  | ROW_HASH
  | NEWLINE

  | CARDINALITY
  | FIND_UNIQUE
  | FILTER_SET
  | FILTER_SEQ
  | MU
  | LAMBDA

  | FORALL
  | EXISTS

  | IN

  | EOF
  | ERROR of string
[@@deriving show, eq]

type token = {
  typ: token_type;
  lexeme: string;
  loc: location;
}
[@@deriving show]

let make_token typ lexeme loc = { typ; lexeme; loc }

let unicode_map = [
  ("→", "->");
  ("⇒", "=>");

  ("×", "*");
  ("÷", "/");

  ("≠", "!=");
  ("≤", "<=");
  ("≥", ">=");

  ("∧", "&");
  ("∨", "|");
  ("¬", "!");
  ("↔", "<->");

  ("∀", "(x)");
  ("∃", "(Ex)");

  ("∈", " in ");
  ("μ", "u");
  ("λ", "\\");

  ("η", "eta");
  ("ξ", "xi");
  ("ν", "nu");
  ("Π", "Pi");
  ("α", "alpha");
  ("β", "beta");
  ("γ", "gamma");

  ("Δ", " Delta ");
  ("Σ", " Sigma ");
  ("Φ", " Phi ");
]

let normalise_unicode s =
  let replace_all ~pattern ~replacement s =
    let pattern_len = String.length pattern in
    let buf = Buffer.create (String.length s) in
    let rec loop i =
      if i >= String.length s then ()
      else if i + pattern_len <= String.length s &&
              String.sub s i pattern_len = pattern then begin
        Buffer.add_string buf replacement;
        loop (i + pattern_len)
      end else begin
        Buffer.add_char buf (String.get s i);
        loop (i + 1)
      end
    in
    loop 0;
    Buffer.contents buf
  in
  List.fold_left (fun acc (unicode, ascii) ->
    replace_all ~pattern:unicode ~replacement:ascii acc
  ) s unicode_map

let is_loop_keyword = function
  | W | W0 | W1 | W2 | W3 | W4 | W5 | W6 -> true
  | _ -> false

let is_variable = function
  | VAR_V _ | VAR_Z _ | VAR_R _ -> true
  | _ -> false

let pp_token_short fmt tok =
  match tok.typ with
  | INTEGER n -> Format.fprintf fmt "%d" n
  | FLOAT f -> Format.fprintf fmt "%f" f
  | BOOLEAN b -> Format.fprintf fmt "%b" b
  | VAR_V n -> Format.fprintf fmt "V%d" n
  | VAR_Z n -> Format.fprintf fmt "Z%d" n
  | VAR_R n -> Format.fprintf fmt "R%d" n
  | IDENT s -> Format.fprintf fmt "%s" s
  | EOF -> Format.fprintf fmt "<EOF>"
  | ERROR msg -> Format.fprintf fmt "<ERROR: %s>" msg
  | _ -> Format.fprintf fmt "%s" tok.lexeme
