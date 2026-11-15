type severity =
  | Error
  | Warning
  | Note
  | Hint
[@@deriving show]

type error_code =
  | E001
  | E002
  | E003
  | E004
  | E005
  | E006
  | E007
  | E008
  | E009
  | E010

  | E100
  | E101
  | E102
  | E103
  | E104
  | E105
  | E106
  | E107

  | E200
  | E201
  | E202
  | E203
  | E204

  | E300
  | E301
  | E302
  | E303
  | E304

  | E400
  | E401
  | E402
  | E403

  | E900
  | E901
[@@deriving show]

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

type source_span = {
  file: string;
  start_line: int;
  start_col: int;
  end_line: int;
  end_col: int;
}
[@@deriving show]

let point_span file line col = {
  file;
  start_line = line;
  start_col = col;
  end_line = line;
  end_col = col + 1;
}

let make_span file start_line start_col end_line end_col = {
  file;
  start_line;
  start_col;
  end_line;
  end_col;
}

type compiler_error = {
  code: error_code;
  severity: severity;
  message: string;
  span: source_span;
  context_lines: (int * string) list;
  suggestion: string option;
  related: source_span list;
}
[@@deriving show]

let make_error code message span = {
  code;
  severity = Error;
  message;
  span;
  context_lines = [];
  suggestion = None;
  related = [];
}

let make_warning code message span = {
  code;
  severity = Warning;
  message;
  span;
  context_lines = [];
  suggestion = None;
  related = [];
}

let with_suggestion err suggestion =
  { err with suggestion = Some suggestion }

let with_context err lines =
  { err with context_lines = lines }

let with_related err spans =
  { err with related = spans }

type error_collector = {
  mutable errors: compiler_error list;
  mutable has_errors: bool;
}

let create_collector () = {
  errors = [];
  has_errors = false;
}

let record_error collector err =
  collector.errors <- err :: collector.errors;
  if err.severity = Error then
    collector.has_errors <- true

let get_errors collector =
  List.rev collector.errors

let has_errors collector =
  collector.has_errors

let count_by_severity collector severity =
  List.length (List.filter (fun e -> e.severity = severity) collector.errors)
