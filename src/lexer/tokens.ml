(** Token Definitions for Plankalkul

    Supports both Zuse's original Unicode symbols and ASCII equivalents.

    Ported from plankalkul.py (2025) with attribution to:
    - Zane Hambly's interpreter
    - Rojas et al., FU Berlin (2000)

    Adaptation: Token types restructured for OCaml ADT pattern matching.
    Added: 2D notation specific tokens (ROW_V, ROW_K, ROW_S, PIPE)
*)

(** Source location for tokens *)
type location = {
  file: string;
  line: int;
  column: int;
}
[@@deriving show, eq]

let dummy_loc = { file = "<unknown>"; line = 0; column = 0 }

(** Token types *)
type token_type =
  (* Literals *)
  | INTEGER of int
  | FLOAT of float
  | BOOLEAN of bool

  (* Variables - Zuse's V/Z/R convention *)
  | VAR_V of int    (** Input variable: V0, V1, ... *)
  | VAR_Z of int    (** Intermediate: Z0, Z1, ... (Zwischenwerte) *)
  | VAR_R of int    (** Result: R0, R1, ... *)

  (* Identifiers *)
  | IDENT of string     (** Plan/function name *)
  | TYPE_ANNOT of string (** Type annotation like [:8.0] or [:m*X] *)

  (* Arithmetic operators *)
  | PLUS              (** + *)
  | MINUS             (** - *)
  | MULTIPLY          (** * or x *)
  | DIVIDE            (** / or : *)

  (* Comparison operators *)
  | EQ                (** = *)
  | NEQ               (** != or /= *)
  | LT                (** < *)
  | GT                (** > *)
  | LEQ               (** <= or =< *)
  | GEQ               (** >= or => (context-dependent!) *)

  (* Logical operators *)
  | AND               (** & or /\ *)
  | OR                (** | or \/ *)
  | NOT               (** ! or ~ *)
  | IMPL              (** -> implication *)
  | EQUIV             (** <-> equivalence *)

  (* Assignment and flow *)
  | ARROW             (** => assignment *)
  | COND_ARROW        (** -> conditional execution *)

  (* Delimiters *)
  | LPAREN            (** ( *)
  | RPAREN            (** ) *)
  | LBRACKET          (** [ *)
  | RBRACKET          (** ] *)
  | LBRACE            (** { *)
  | RBRACE            (** } *)
  | COMMA             (** , *)
  | COLON             (** : *)
  | SEMICOLON         (** ; *)
  | DOT               (** . *)
  | PIPE              (** | - used in 2D notation row labels *)

  (* Keywords *)
  | P                 (** Plan declaration *)
  | TABLE             (** Truth table declaration (Zuse's decision tables) *)
  | DELTA             (** Δ - Delta plan group prefix (PΔ300 for chess plans) *)
  | SIGMA             (** Σ - Sigma plan group prefix *)
  | PHI               (** Φ - Phi plan group prefix *)
  | W                 (** While loop - repeat until all conditions false *)
  | W0                (** Hidden counter loop *)
  | W1                (** Count up loop: 0 to n-1 *)
  | W2                (** Count down loop: n-1 to 0 *)
  | W3                (** While m ≥ n *)
  | W4                (** While m ≤ n *)
  | W5                (** Toward target, auto direction *)
  | W6                (** List iteration until empty (Bruines p.12) *)
  | FIN               (** Terminate/break (also value from µ/λ) *)
  | END               (** End of plan *)

  (* 2D notation specific *)
  | ROW_V             (** V| row marker *)
  | ROW_K             (** K| row marker *)
  | ROW_S             (** S| row marker *)
  | ROW_HASH          (** #| comment row marker *)
  | NEWLINE           (** Significant in 2D notation *)

  (* Set/list operations (the functional features!) *)
  | CARDINALITY       (** N( - length/size *)
  | FIND_UNIQUE       (** ' - find unique element *)
  | FILTER_SET        (** ^ - filter to set *)
  | FILTER_SEQ        (** ^^ - filter to sequence *)
  | MU                (** u - forward iterator *)
  | LAMBDA            (** \ - backward iterator *)

  (* Quantifiers *)
  | FORALL            (** (x) - universal quantifier *)
  | EXISTS            (** (Ex) - existential quantifier *)

  (* Membership *)
  | IN                (** e - element of *)

  (* Special *)
  | EOF
  | ERROR of string   (** Lexer error with message *)
[@@deriving show, eq]

(** A token with its type and location *)
type token = {
  typ: token_type;
  lexeme: string;       (** Original text that produced this token *)
  loc: location;
}
[@@deriving show]

(** Create a token *)
let make_token typ lexeme loc = { typ; lexeme; loc }

(** Unicode to ASCII mapping

    Zuse used mathematical notation. We support both forms.
    This table is ported from plankalkul.py with additions.
*)
(** Unicode to ASCII mapping table
    Maps Unicode characters to their ASCII equivalents.
    Using string pairs since OCaml chars are single-byte.
*)
let unicode_map = [
  (* Arrows *)
  ("→", "->");   (* rightwards arrow *)
  ("⇒", "=>");   (* rightwards double arrow *)

  (* Arithmetic *)
  ("×", "*");    (* multiplication sign *)
  ("÷", "/");    (* division sign *)

  (* Comparison *)
  ("≠", "!=");   (* not equal to *)
  ("≤", "<=");   (* less than or equal *)
  ("≥", ">=");   (* greater than or equal *)

  (* Logical *)
  ("∧", "&");    (* logical and *)
  ("∨", "|");    (* logical or *)
  ("¬", "!");    (* not sign *)
  ("↔", "<->");  (* left right arrow (equivalence) *)

  (* Quantifiers *)
  ("∀", "(x)");  (* for all - we expand to Plankalkul form *)
  ("∃", "(Ex)"); (* there exists *)

  (* Set theory *)
  ("∈", " in ");  (* element of *)
  ("μ", "u");     (* mu - forward iterator *)
  ("λ", "\\");    (* lambda - backward iterator *)

  (* Greek letters used as identifiers *)
  ("η", "eta");   (* eta - component index variable *)
  ("ξ", "xi");    (* xi - sometimes used as index *)
  ("ν", "nu");    (* nu - sometimes used as index *)
  ("Π", "Pi");    (* pi - product *)
  ("α", "alpha"); (* alpha *)
  ("β", "beta");  (* beta *)
  ("γ", "gamma"); (* gamma *)

  (* Plan group prefixes - separated with spaces to tokenize properly *)
  ("Δ", " Delta "); (* delta - plan group prefix (chess plans) *)
  ("Σ", " Sigma "); (* sigma - plan group prefix *)
  ("Φ", " Phi ");   (* phi - plan group prefix *)
]

(** Normalise Unicode to ASCII equivalents *)
let normalise_unicode s =
  (* Simple string replacement - replaces all occurrences of unicode with ascii *)
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

(** Is this a loop keyword? *)
let is_loop_keyword = function
  | W | W0 | W1 | W2 | W3 | W4 | W5 | W6 -> true
  | _ -> false

(** Is this a variable token? *)
let is_variable = function
  | VAR_V _ | VAR_Z _ | VAR_R _ -> true
  | _ -> false

(** Pretty print token for error messages *)
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
