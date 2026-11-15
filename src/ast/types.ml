type primitive =
  | Bit                     (** Zuse's Ja-Nein-Wert, written 0 *)
  | SignBit
  | BitWidth of int         (** S1.n, Zuse's n-bit integer *)
[@@deriving show, eq]

(** The numeric types A8 to A13 from Zuse's manuscripts *)
type numeric_type =
  | A8                       (** Natural number *)
  | A9                       (** Positive integer *)
  | A10                      (** Integer (signed) *)
  | A11                      (** Positive fraction *)
  | A12                      (** Fraction *)
  | A13                      (** Complex number *)
[@@deriving show, eq]

type typ =
  | TPrimitive of primitive
  | TNumeric of numeric_type
  | TArray of int * typ
  | TMultiArray of int list * typ
  | TTuple of typ list
  | TUserDefined of string
  | TInferred
[@@deriving show, eq]

type type_annotation = {
  bits: int option;
  dimensions: int list;
  numeric: numeric_type option;
}
[@@deriving show]

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
