(** Error Types for Plankalkül Compiler

    A taxonomy of all the ways a Plankalkül program can disappoint its author.

    Following the grand tradition of compiler error messages—from the cryptic
    "SYNTAX ERROR" of early FORTRAN to Elm's famously helpful suggestions—we
    provide structured error representation with:

    - Error codes for categorization (E001-E999, because three digits
      seemed sufficient in 1970 and we see no reason to change now)
    - Severity levels (Error, Warning, Note, Hint—a gradient from
      "your program won't compile" to "have you considered...")
    - Source location information (because "somewhere in your code"
      is not, in fact, helpful)
    - Suggestions for fixes (we try to be constructive)

    As Zuse himself might have noted, the difference between a working
    program and a broken one is often a single misplaced character.
*)

(** Error severity levels *)
type severity =
  | Error       (** Compilation cannot continue *)
  | Warning     (** Compilation continues, but behavior may be unexpected *)
  | Note        (** Additional information for context *)
  | Hint        (** Suggestion for improvement *)
[@@deriving show]

(** Error categories with codes *)
type error_code =
  (* Syntax errors: E001-E099 *)
  | E001  (** Unexpected token *)
  | E002  (** Missing expected token *)
  | E003  (** Unterminated string/comment *)
  | E004  (** Invalid character *)
  | E005  (** Malformed number literal *)
  | E006  (** Invalid variable name *)
  | E007  (** Missing plan body *)
  | E008  (** Invalid loop syntax *)
  | E009  (** Invalid quantifier syntax *)
  | E010  (** Invalid iterator syntax *)

  (* Type errors: E100-E199 *)
  | E100  (** Type mismatch in assignment *)
  | E101  (** Type mismatch in binary operation *)
  | E102  (** Type mismatch in comparison *)
  | E103  (** Non-numeric type in arithmetic *)
  | E104  (** Non-boolean type in condition *)
  | E105  (** Array type expected *)
  | E106  (** Incompatible function argument types *)
  | E107  (** Unknown type annotation *)

  (* Resolution errors: E200-E299 *)
  | E200  (** Undefined variable *)
  | E201  (** Undefined plan *)
  | E202  (** Undefined plan group *)
  | E203  (** Argument count mismatch *)
  | E204  (** Circular dependency *)

  (* Semantic errors: E300-E399 *)
  | E300  (** Assignment to input variable *)
  | E301  (** FIN outside loop *)
  | E302  (** Unreachable code after FIN *)
  | E303  (** Empty loop body *)
  | E304  (** Infinite loop detected *)

  (* 2D notation errors: E400-E499 *)
  | E400  (** Column misalignment *)
  | E401  (** Invalid row marker *)
  | E402  (** Missing required row *)
  | E403  (** Grid structure error *)

  (* Internal errors: E900-E999 *)
  | E900  (** Internal compiler error *)
  | E901  (** Feature not implemented *)
[@@deriving show]

(** Get error code as string (e.g., "E001") *)
let error_code_str = function
  | E001 -> "E001" | E002 -> "E002" | E003 -> "E003" | E004 -> "E004"
  | E005 -> "E005" | E006 -> "E006" | E007 -> "E007" | E008 -> "E008"
  | E009 -> "E009" | E010 -> "E010"
  | E100 -> "E100" | E101 -> "E101" | E102 -> "E102" | E103 -> "E103"
  | E104 -> "E104" | E105 -> "E105" | E106 -> "E106" | E107 -> "E107"
  | E200 -> "E200" | E201 -> "E201" | E202 -> "E202" | E203 -> "E203"
  | E204 -> "E204"
  | E300 -> "E300" | E301 -> "E301" | E302 -> "E302" | E303 -> "E303"
  | E304 -> "E304"
  | E400 -> "E400" | E401 -> "E401" | E402 -> "E402" | E403 -> "E403"
  | E900 -> "E900" | E901 -> "E901"

(** Get human-readable description for error code *)
let error_description = function
  | E001 -> "Unexpected token"
  | E002 -> "Missing expected token"
  | E003 -> "Unterminated string or comment"
  | E004 -> "Invalid character"
  | E005 -> "Malformed number literal"
  | E006 -> "Invalid variable name"
  | E007 -> "Missing plan body"
  | E008 -> "Invalid loop syntax"
  | E009 -> "Invalid quantifier syntax"
  | E010 -> "Invalid iterator syntax"
  | E100 -> "Type mismatch in assignment"
  | E101 -> "Type mismatch in operation"
  | E102 -> "Type mismatch in comparison"
  | E103 -> "Non-numeric type in arithmetic"
  | E104 -> "Non-boolean type in condition"
  | E105 -> "Array type expected"
  | E106 -> "Incompatible function argument types"
  | E107 -> "Unknown type annotation"
  | E200 -> "Undefined variable"
  | E201 -> "Undefined plan"
  | E202 -> "Undefined plan group"
  | E203 -> "Argument count mismatch"
  | E204 -> "Circular dependency"
  | E300 -> "Assignment to input variable"
  | E301 -> "FIN outside loop"
  | E302 -> "Unreachable code"
  | E303 -> "Empty loop body"
  | E304 -> "Infinite loop"
  | E400 -> "Column misalignment"
  | E401 -> "Invalid row marker"
  | E402 -> "Missing required row"
  | E403 -> "Grid structure error"
  | E900 -> "Internal compiler error"
  | E901 -> "Feature not implemented"

(** Source span for error location *)
type source_span = {
  file: string;
  start_line: int;
  start_col: int;
  end_line: int;
  end_col: int;
}
[@@deriving show]

(** Create a source span from a single point *)
let point_span file line col = {
  file;
  start_line = line;
  start_col = col;
  end_line = line;
  end_col = col + 1;
}

(** Create a source span from start/end *)
let make_span file start_line start_col end_line end_col = {
  file;
  start_line;
  start_col;
  end_line;
  end_col;
}

(** A compiler error/warning/note *)
type compiler_error = {
  code: error_code;
  severity: severity;
  message: string;
  span: source_span;
  context_lines: (int * string) list;  (** Line number and content for context *)
  suggestion: string option;           (** Suggested fix *)
  related: source_span list;           (** Related locations *)
}
[@@deriving show]

(** Create a simple error *)
let make_error code message span = {
  code;
  severity = Error;
  message;
  span;
  context_lines = [];
  suggestion = None;
  related = [];
}

(** Create a warning *)
let make_warning code message span = {
  code;
  severity = Warning;
  message;
  span;
  context_lines = [];
  suggestion = None;
  related = [];
}

(** Add a suggestion to an error *)
let with_suggestion err suggestion =
  { err with suggestion = Some suggestion }

(** Add context lines to an error *)
let with_context err lines =
  { err with context_lines = lines }

(** Add related locations *)
let with_related err spans =
  { err with related = spans }

(** Error collection for multi-error reporting *)
type error_collector = {
  mutable errors: compiler_error list;
  mutable has_errors: bool;
}

(** Create a new error collector *)
let create_collector () = {
  errors = [];
  has_errors = false;
}

(** Record an error *)
let record_error collector err =
  collector.errors <- err :: collector.errors;
  if err.severity = Error then
    collector.has_errors <- true

(** Get all errors in order *)
let get_errors collector =
  List.rev collector.errors

(** Check if any errors were recorded *)
let has_errors collector =
  collector.has_errors

(** Count errors by severity *)
let count_by_severity collector severity =
  List.length (List.filter (fun e -> e.severity = severity) collector.errors)
