(** 2D Grid Structure for Plankalkül

    The 2D notation is essentially a spreadsheet where:
    - Top row contains expressions/operations
    - V| row contains variable indices
    - K| row contains component indices
    - S| row contains type annotations

    Each column is a vertical slice representing one variable reference
    or operation.

    Attribution: 2D notation from Zuse's original manuscripts (1941-1945)
    Implementation by Zane Hambly (2025-2026)
*)

open Provenance.Sources

(** Row type in 2D notation *)
type row_type =
  | RowExpr       (** Expression/operation row (no prefix or " |") *)
  | RowV          (** V| - Variable indices *)
  | RowK          (** K| - Component indices *)
  | RowS          (** S| - Structure/type annotations *)
  | RowComment    (** ;| or #| - Comment row *)
  | RowBlank      (** Empty row *)
  | RowUnknown    (** Unrecognized row *)
[@@deriving show, eq]

(** A single row in the grid *)
type row = {
  row_type: row_type;
  content: string;        (** Content after the prefix (e.g., after "V|") *)
  raw: string;            (** Original line including prefix *)
  line_num: int;          (** 1-based line number in source file *)
}
[@@deriving show]

(** A column extracted from the grid *)
type column = {
  col_start: int;         (** Start character position (0-based) *)
  col_end: int;           (** End character position (exclusive) *)
  expr_text: string;      (** Text from expression row *)
  var_text: string;       (** Text from V row *)
  comp_text: string;      (** Text from K row *)
  type_text: string;      (** Text from S row *)
}
[@@deriving show]

(** A complete 2D block (one statement/expression) *)
type block = {
  rows: row list;
  columns: column list;
  start_line: int;
  end_line: int;
  block_provenance: provenance;
}
[@@deriving show]

(** Alignment warning *)
type alignment_warning = {
  warn_line: int;
  warn_col: int;
  warn_message: string;
  expected_col: int option;  (** Expected column position if known *)
}
[@@deriving show]

(** Parser state for grid reading *)
type grid_state = {
  lines: string array;
  mutable pos: int;           (** Current line index *)
  mutable current_provenance: provenance;
  file: string;
  mutable warnings: alignment_warning list;  (** Collected alignment warnings *)
}

(** Remove (* *) style comments from source text.
    These are multi-line comments used in some Plankalkül sources (e.g., Hovestar).
    We strip them before grid parsing to avoid confusing the 2D notation parser. *)
let strip_ml_comments source =
  let len = String.length source in
  let buf = Buffer.create len in
  let rec loop i in_comment =
    if i >= len then ()
    else if in_comment then begin
      (* Look for closing star-paren *)
      if i + 1 < len && source.[i] = '*' && source.[i+1] = ')' then
        loop (i + 2) false
      else begin
        (* Preserve newlines so line numbers stay correct *)
        if source.[i] = '\n' then Buffer.add_char buf '\n';
        loop (i + 1) true
      end
    end else begin
      (* Look for opening paren-star *)
      if i + 1 < len && source.[i] = '(' && source.[i+1] = '*' then
        loop (i + 2) true
      else begin
        Buffer.add_char buf source.[i];
        loop (i + 1) false
      end
    end
  in
  loop 0 false;
  Buffer.contents buf

(** Create a grid state from source text *)
let create ?(file="<input>") ?(provenance=unknown) source =
  (* Strip ML-style comments first *)
  let without_ml_comments = strip_ml_comments source in
  (* Normalise Unicode characters (Greek letters, special symbols) to ASCII *)
  let normalised = Tokens.normalise_unicode without_ml_comments in
  let lines = String.split_on_char '\n' normalised |> Array.of_list in
  {
    lines;
    pos = 0;
    current_provenance = provenance;
    file;
    warnings = [];
  }

(** Record an alignment warning *)
let add_warning state line col message expected_col =
  let warn = { warn_line = line; warn_col = col; warn_message = message; expected_col } in
  state.warnings <- warn :: state.warnings

(** Get all collected warnings *)
let get_warnings state = List.rev state.warnings

(** Clear warnings *)
let clear_warnings state = state.warnings <- []

(** Check if we're at end of input *)
let at_end state =
  state.pos >= Array.length state.lines

(** Get current line without advancing *)
let current_line state =
  if at_end state then None
  else Some state.lines.(state.pos)

(** Advance to next line *)
let advance state =
  if not (at_end state) then
    state.pos <- state.pos + 1

(** Get current line number (1-based) *)
let current_line_num state = state.pos + 1

(* ============================================================================
   ROW TYPE DETECTION
   ============================================================================ *)

(** Detect the type of a row from its content *)
let detect_row_type line =
  let trimmed = String.trim line in
  if String.length trimmed = 0 then
    RowBlank
  else if String.length trimmed >= 2 then
    let prefix = String.sub trimmed 0 2 in
    match prefix with
    | "V|" -> RowV
    | "K|" -> RowK
    | "S|" -> RowS
    | ";|" | "#|" -> RowComment
    | " |" -> RowExpr
    | _ ->
      (* Check for single character prefix *)
      if String.length trimmed >= 1 && trimmed.[0] = '|' then
        RowExpr
      else if String.length trimmed >= 1 && trimmed.[0] = ';' then
        RowComment
      else
        (* No prefix - likely expression row or start of block *)
        RowExpr
  else
    RowUnknown

(** Extract content after row prefix, preserving column alignment.
    All row types use a 2-character prefix (e.g., " |", "V|", "K|", "S|")
    so we skip exactly 2 characters to maintain alignment. *)
let extract_content row_type line =
  (* Find the pipe character which marks the prefix boundary *)
  match String.index_opt line '|' with
  | Some pipe_pos ->
    (* Content starts after the pipe *)
    if pipe_pos + 1 < String.length line then
      String.sub line (pipe_pos + 1) (String.length line - pipe_pos - 1)
    else ""
  | None ->
    (* No pipe - return the line as-is (for lines without standard prefix) *)
    match row_type with
    | RowBlank -> ""
    | _ -> line

(** Parse a single line into a row *)
let parse_row line_num line =
  let row_type = detect_row_type line in
  let content = extract_content row_type line in
  { row_type; content; raw = line; line_num }

(* ============================================================================
   PROVENANCE PARSING FROM COMMENTS
   ============================================================================ *)

(** Parse a SOURCE comment to extract provenance *)
let parse_source_comment line =
  (* Look for patterns like:
     ; SOURCE: ZIA-0367, page 10
     ; SOURCE: ZIA-0410, p.5
     ; FROM: Bruines p.12 *)
  let line = String.trim line in
  if String.length line < 3 then None
  else
    let content = String.sub line 1 (String.length line - 1) |> String.trim in
    (* Check for SOURCE: or FROM: prefix *)
    let parse_ref s =
      (* Try to extract archive ID and page *)
      let archive_id =
        if String.length s >= 8 && String.sub s 0 4 = "ZIA-" then
          let id_str = String.sub s 0 8 in
          match id_str with
          | "ZIA-0367" -> Some ZIA_0367
          | "ZIA-0368" -> Some ZIA_0368
          | "ZIA-0410" -> Some ZIA_0410
          | _ -> None
        else if String.length s >= 7 && String.sub s 0 7 = "Bruines" then
          Some Bruines_2010
        else
          None
      in
      (* Try to find page number *)
      let page =
        (* Look for "page N", "p.N", or "p N" *)
        let regex_patterns = [
          Str.regexp {|page[ ]*\([0-9]+\)|};
          Str.regexp {|p\.[ ]*\([0-9]+\)|};
          Str.regexp {|p[ ]+\([0-9]+\)|};
        ] in
        let rec try_patterns = function
          | [] -> None
          | pat :: rest ->
            try
              ignore (Str.search_forward pat s 0);
              Some (int_of_string (Str.matched_group 1 s))
            with Not_found -> try_patterns rest
        in
        try_patterns regex_patterns
      in
      match archive_id with
      | Some ZIA_0367 -> Some (from_zia_0367 ?page ())
      | Some ZIA_0368 -> Some (from_zia_0368 ?page ())
      | Some ZIA_0410 -> Some (from_zia_0410 ?page ())
      | Some Bruines_2010 -> Some (from_bruines ?page ())
      | _ -> None
    in
    if String.length content >= 7 && String.sub content 0 7 = "SOURCE:" then
      parse_ref (String.sub content 7 (String.length content - 7) |> String.trim)
    else if String.length content >= 5 && String.sub content 0 5 = "FROM:" then
      parse_ref (String.sub content 5 (String.length content - 5) |> String.trim)
    else
      None

(* ============================================================================
   COLUMN DETECTION
   ============================================================================ *)

(** Find column boundaries in the expression row.

    Columns are separated by:
    - Operators: =>, ->, +, -, *, /, etc.
    - Brackets: [, ], (, )
    - Significant whitespace gaps

    We also use | characters in V/K/S rows as alignment guides.
*)

(** Character positions of significant boundaries *)
type boundary = {
  pos: int;
  kind: [`Operator | `Bracket | `Whitespace | `Pipe];
}

(** Find all | characters in a string (used as alignment guides) *)
let find_pipes s =
  let rec aux acc i =
    if i >= String.length s then List.rev acc
    else if s.[i] = '|' then aux (i :: acc) (i + 1)
    else aux acc (i + 1)
  in
  aux [] 0

(** Find column boundaries using combined heuristics *)
let find_column_boundaries expr_row v_row k_row s_row =
  (* Strategy:
     1. Find | characters in V/K/S rows - these are explicit separators
     2. Find operators in expression row
     3. Find bracket boundaries
     4. Use whitespace gaps as fallback *)

  (* Get all pipe positions from V/K/S rows *)
  let v_pipes = find_pipes v_row in
  let k_pipes = find_pipes k_row in
  let s_pipes = find_pipes s_row in

  (* Combine and deduplicate pipe positions *)
  let all_pipes = List.sort_uniq compare (v_pipes @ k_pipes @ s_pipes) in

  (* Find operators in expression row *)
  let expr_len = String.length expr_row in
  let rec find_operators acc i =
    if i >= expr_len then List.rev acc
    else
      let is_op =
        (* Two-character operators *)
        (i + 1 < expr_len &&
         let two = String.sub expr_row i 2 in
         two = "=>" || two = "->" || two = "<=" || two = ">=" ||
         two = "!=" || two = "/=") ||
        (* Single-character operators *)
        (let c = expr_row.[i] in
         c = '+' || c = '-' || c = '*' || c = '/' || c = '=' ||
         c = '<' || c = '>' || c = '&' || c = '|' || c = '^')
      in
      if is_op then
        let len = if i + 1 < expr_len &&
                    (String.sub expr_row i 2 = "=>" ||
                     String.sub expr_row i 2 = "->" ||
                     String.sub expr_row i 2 = "<=" ||
                     String.sub expr_row i 2 = ">=" ||
                     String.sub expr_row i 2 = "!=" ||
                     String.sub expr_row i 2 = "/=")
                  then 2 else 1 in
        find_operators ((i, i + len) :: acc) (i + len)
      else
        find_operators acc (i + 1)
  in
  let operators = find_operators [] 0 in

  (* Find bracket positions *)
  let rec find_brackets acc i =
    if i >= expr_len then List.rev acc
    else
      let c = expr_row.[i] in
      if c = '[' || c = ']' || c = '(' || c = ')' || c = '{' || c = '}' then
        find_brackets (i :: acc) (i + 1)
      else
        find_brackets acc (i + 1)
  in
  let brackets = find_brackets [] 0 in

  (* Combine all boundaries *)
  let boundaries =
    List.map (fun p -> (p, `Pipe)) all_pipes @
    List.concat_map (fun (start, _) -> [(start, `Operator)]) operators @
    List.map (fun p -> (p, `Bracket)) brackets
  in

  (* Sort by position and remove duplicates *)
  boundaries
  |> List.sort (fun (p1, _) (p2, _) -> compare p1 p2)
  |> List.fold_left (fun acc (p, k) ->
       match acc with
       | (prev_p, _) :: _ when prev_p = p -> acc
       | _ -> (p, k) :: acc
     ) []
  |> List.rev

(** Extract text from a row at given column range *)
let extract_at_range s start_pos end_pos =
  let s_len = String.length s in
  if start_pos >= s_len then ""
  else
    let actual_end = min end_pos s_len in
    let extracted = String.sub s start_pos (actual_end - start_pos) in
    String.trim extracted

(** Build columns from row data and boundaries *)
let build_columns expr_row v_row k_row s_row boundaries =
  (* Add implicit boundaries at start and end *)
  let max_len = max (String.length expr_row)
                    (max (String.length v_row)
                         (max (String.length k_row) (String.length s_row))) in
  let all_boundaries =
    (0, `Whitespace) :: boundaries @ [(max_len, `Whitespace)]
    |> List.sort_uniq (fun (p1, _) (p2, _) -> compare p1 p2)
  in

  (* Create columns between adjacent boundaries *)
  let rec make_columns acc = function
    | [] | [_] -> List.rev acc
    | (start_pos, _) :: ((end_pos, _) :: _ as rest) ->
      let expr_text = extract_at_range expr_row start_pos end_pos in
      let var_text = extract_at_range v_row start_pos end_pos in
      let comp_text = extract_at_range k_row start_pos end_pos in
      let type_text = extract_at_range s_row start_pos end_pos in

      (* Only create column if there's meaningful content *)
      if String.length expr_text > 0 || String.length var_text > 0 then
        let col = {
          col_start = start_pos;
          col_end = end_pos;
          expr_text;
          var_text;
          comp_text;
          type_text;
        } in
        make_columns (col :: acc) rest
      else
        make_columns acc rest
  in
  make_columns [] all_boundaries

(* ============================================================================
   BLOCK READING
   ============================================================================ *)

(** Read a complete 2D block from the grid state.

    A block consists of:
    - An expression row (may have " |" prefix or none)
    - A V| row (variable indices)
    - An optional K| row (component indices)
    - An optional S| row (type annotations)

    Blocks are separated by blank lines.
*)
let read_block state =
  if at_end state then None
  else begin
    let _start_line = current_line_num state in
    let rows = ref [] in
    let found_content = ref false in

    (* Skip leading blank lines and comments, but capture SOURCE comments *)
    while not (at_end state) && not !found_content do
      match current_line state with
      | None -> ()
      | Some line ->
        let row = parse_row (current_line_num state) line in
        match row.row_type with
        | RowBlank -> advance state
        | RowComment ->
          (* Check for SOURCE comment *)
          (match parse_source_comment line with
           | Some prov -> state.current_provenance <- prov
           | None -> ());
          advance state
        | _ -> found_content := true
    done;

    if at_end state then None
    else begin
      (* Read rows until blank line or end *)
      let block_start = current_line_num state in
      let done_reading = ref false in
      while not (at_end state) && not !done_reading do
        match current_line state with
        | None -> done_reading := true
        | Some line ->
          let row = parse_row (current_line_num state) line in
          match row.row_type with
          | RowBlank ->
            (* End of block - advance past the blank line but stop reading *)
            advance state;
            done_reading := true
          | RowComment ->
            (* Include comments in block but don't stop *)
            rows := row :: !rows;
            advance state
          | RowExpr when String.trim line = "END" ->
            (* END marker - skip it and stop reading *)
            advance state;
            done_reading := true
          | _ ->
            rows := row :: !rows;
            advance state;
            (* Check if next line starts a new block (expression row after S row) *)
            if row.row_type = RowS then begin
              match current_line state with
              | Some next_line ->
                let next_type = detect_row_type next_line in
                if next_type = RowExpr || next_type = RowBlank then
                  done_reading := true  (* End block, don't advance *)
              | None -> ()
            end
      done;

      let rows = List.rev !rows in
      let end_line = current_line_num state - 1 in

      (* Extract row contents by type *)
      let find_row rt =
        List.find_opt (fun r -> r.row_type = rt) rows
        |> Option.map (fun r -> r.content)
        |> Option.value ~default:""
      in

      let expr_row = find_row RowExpr in
      let v_row = find_row RowV in
      let k_row = find_row RowK in
      let s_row = find_row RowS in

      (* Find column boundaries and build columns *)
      let boundaries = find_column_boundaries expr_row v_row k_row s_row in
      let columns = build_columns expr_row v_row k_row s_row boundaries in

      Some {
        rows;
        columns;
        start_line = block_start;
        end_line;
        block_provenance = state.current_provenance;
      }
    end
  end

(** Read all blocks from source *)
let read_all_blocks state =
  let rec aux acc =
    match read_block state with
    | None -> List.rev acc
    | Some block -> aux (block :: acc)
  in
  aux []

(** Convenience: read blocks from string *)
let blocks_from_string ?(file="<input>") ?(provenance=unknown) source =
  let state = create ~file ~provenance source in
  read_all_blocks state

(* ============================================================================
   ALIGNMENT CHECKING

   Zuse's 2D notation relies critically on vertical alignment. A variable
   index in the V row must align with its corresponding expression in the
   expression row, like a spreadsheet where the columns actually matter.

   Misalignment is a common source of confusion, especially when mixing
   tabs and spaces (a practice we discourage in the strongest possible
   terms). These functions detect and report alignment issues, hopefully
   before the programmer spends an hour wondering why V0 has the wrong type.
   ============================================================================ *)

(** Find token positions in a row (start position of each non-whitespace segment) *)
let find_token_positions content =
  let len = String.length content in
  let positions = ref [] in
  let i = ref 0 in
  while !i < len do
    (* Skip whitespace *)
    while !i < len && (content.[!i] = ' ' || content.[!i] = '\t') do
      incr i
    done;
    if !i < len then begin
      let start = !i in
      (* Skip non-whitespace *)
      while !i < len && content.[!i] <> ' ' && content.[!i] <> '\t' do
        incr i
      done;
      positions := start :: !positions
    end
  done;
  List.rev !positions

(** Check alignment between expression row and V/K/S rows *)
let check_block_alignment state block =
  let get_row_content row_type =
    List.find_opt (fun r -> r.row_type = row_type) block.rows
    |> Option.map (fun r -> (r.content, r.line_num))
  in

  let expr_opt = get_row_content RowExpr in
  let v_opt = get_row_content RowV in
  let k_opt = get_row_content RowK in
  let s_opt = get_row_content RowS in

  (* Get positions from expression row as reference *)
  let expr_positions = match expr_opt with
    | Some (content, _) -> find_token_positions content
    | None -> []
  in

  (* Check V row alignment *)
  (match v_opt with
   | Some (v_content, v_line) ->
     let v_positions = find_token_positions v_content in
     (* Check if V positions roughly align with expression positions *)
     List.iter (fun v_pos ->
       let has_nearby = List.exists (fun e_pos ->
         abs (v_pos - e_pos) <= 2  (* Allow 2 character tolerance *)
       ) expr_positions in
       if not has_nearby && expr_positions <> [] then
         add_warning state v_line v_pos
           "V row index may not align with expression variable"
           (List.find_opt (fun e_pos -> abs (v_pos - e_pos) <= 5) expr_positions)
     ) v_positions
   | None -> ());

  (* Check K row alignment against V row *)
  (match k_opt, v_opt with
   | Some (k_content, k_line), Some (v_content, _) ->
     let k_positions = find_token_positions k_content in
     let v_positions = find_token_positions v_content in
     List.iter (fun k_pos ->
       let has_nearby = List.exists (fun v_pos ->
         abs (k_pos - v_pos) <= 2
       ) v_positions in
       if not has_nearby && v_positions <> [] then
         add_warning state k_line k_pos
           "K row index may not align with V row variable"
           (List.find_opt (fun v_pos -> abs (k_pos - v_pos) <= 5) v_positions)
     ) k_positions
   | _, _ -> ());

  (* Check S row alignment against V row *)
  (match s_opt, v_opt with
   | Some (s_content, s_line), Some (v_content, _) ->
     let s_positions = find_token_positions s_content in
     let v_positions = find_token_positions v_content in
     List.iter (fun s_pos ->
       let has_nearby = List.exists (fun v_pos ->
         abs (s_pos - v_pos) <= 2
       ) v_positions in
       if not has_nearby && v_positions <> [] then
         add_warning state s_line s_pos
           "S row type may not align with V row variable"
           (List.find_opt (fun v_pos -> abs (s_pos - v_pos) <= 5) v_positions)
     ) s_positions
   | _, _ -> ())

(** Format a visual grid representation for error messages *)
let format_block_grid block =
  let buf = Buffer.create 256 in
  List.iter (fun row ->
    let prefix = match row.row_type with
      | RowExpr -> " |"
      | RowV -> "V|"
      | RowK -> "K|"
      | RowS -> "S|"
      | RowComment -> ";|"
      | RowBlank -> "  "
      | RowUnknown -> "?|"
    in
    Buffer.add_string buf prefix;
    Buffer.add_string buf row.content;
    Buffer.add_char buf '\n'
  ) block.rows;
  Buffer.contents buf

(** Format alignment warnings with visual context *)
let format_warnings state =
  let warnings = get_warnings state in
  if warnings = [] then ""
  else begin
    let buf = Buffer.create 512 in
    Buffer.add_string buf "Alignment warnings:\n";
    List.iter (fun warn ->
      Buffer.add_string buf (Printf.sprintf "  Line %d, column %d: %s\n"
        warn.warn_line warn.warn_col warn.warn_message);
      (match warn.expected_col with
       | Some exp ->
         Buffer.add_string buf (Printf.sprintf "    Suggestion: move to column %d\n" exp)
       | None -> ())
    ) warnings;
    Buffer.contents buf
  end
