(** Plankalkul Type System

    The type system is built from bits upward:
    - Primitive: single bit (Ja-Nein-Werte)
    - Arrays: n × σ
    - Tuples: (σ, τ)
    - Predefined numeric types (A8-A13)
*)

(** Primitive types *)
type primitive =
  | Bit                     (** Single bit (0) - Ja-Nein-Werte *)
  | SignBit                 (** Sign bit (S0) *)
  | BitWidth of int         (** S1.n - n-bit integer (historical notation) *)
[@@deriving show, eq]

(** Predefined numeric types from Zuse's manuscript *)
type numeric_type =
  | A8                       (** Natural number *)
  | A9                       (** Positive integer *)
  | A10                      (** Integer (signed) *)
  | A11                      (** Positive fraction *)
  | A12                      (** Fraction *)
  | A13                      (** Complex number *)
[@@deriving show, eq]

(** Full type representation *)
type typ =
  | TPrimitive of primitive
  | TNumeric of numeric_type
  | TArray of int * typ                (** n × σ - array of n elements *)
  | TMultiArray of int list * typ      (** m × n × ... × σ - multi-dimensional *)
  | TTuple of typ list                 (** (σ, τ, ...) *)
  | TUserDefined of string             (** User-defined composite type *)
  | TInferred                          (** Type to be inferred *)
[@@deriving show, eq]

(** Type annotation as it appears in source *)
type type_annotation = {
  bits: int option;           (** Bit width if specified *)
  dimensions: int list;       (** Array dimensions *)
  numeric: numeric_type option;
}
[@@deriving show]

(** Pretty print a type *)
let rec pp_typ fmt = function
  | TPrimitive Bit -> Format.fprintf fmt "0"
  | TPrimitive SignBit -> Format.fprintf fmt "S0"
  | TPrimitive (BitWidth n) -> Format.fprintf fmt "S1.%d" n
  | TNumeric n -> Format.fprintf fmt "A%d" (match n with
      | A8 -> 8 | A9 -> 9 | A10 -> 10 | A11 -> 11 | A12 -> 12 | A13 -> 13)
  | TArray (n, t) -> Format.fprintf fmt "%d × %a" n pp_typ t
  | TMultiArray (dims, t) ->
      Format.fprintf fmt "%s × %a"
        (String.concat " × " (List.map string_of_int dims)) pp_typ t
  | TTuple ts ->
      Format.fprintf fmt "(%a)"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_typ) ts
  | TUserDefined name -> Format.fprintf fmt "%s" name
  | TInferred -> Format.fprintf fmt "?"
