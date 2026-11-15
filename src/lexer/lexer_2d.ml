(* In Zuse's 2D notation the expression row gives a variable's kind (V, Z or R) and the V row below it gives the index. *)

open Tokens
open Grid
open Provenance.Sources

type lexer_2d_state = {
  grid_state: grid_state;
  mutable current_block: block option;
  mutable block_tokens: token list;
  mutable token_pos: int;
  file: string;
  mutable default_provenance: provenance;
  mutable check_alignment: bool;
}

let create ?(file="<input>") ?(provenance=unknown) ?(check_alignment=true) source = {
  grid_state = Grid.create ~file ~provenance source;
  current_block = None;
  block_tokens = [];
  token_pos = 0;
  file;
  default_provenance = provenance;
  check_alignment;
}

let make_tok typ lexeme line col = {
  typ;
  lexeme;
  loc = { file = "<2d>"; line; column = col };
}

type pos_entry = {
  start_pos: int;
  end_pos: int;
  value: string;
}

let build_position_map row_content =
  let len = String.length row_content in
  let entries = ref [] in
  let i = ref 0 in

  while !i < len do
    while !i < len && (row_content.[!i] = ' ' || row_content.[!i] = '\t') do
      incr i
    done;

    if !i < len then begin
      let start_pos = !i in
      let buf = Buffer.create 8 in
      while !i < len && row_content.[!i] <> ' ' && row_content.[!i] <> '\t' do
        Buffer.add_char buf row_content.[!i];
        incr i
      done;
      let end_pos = !i in
      let value = Buffer.contents buf in
      if String.length value > 0 then
        entries := { start_pos; end_pos; value } :: !entries
    end
  done;

  List.rev !entries

let lookup_at_position pos_map position =
  List.find_opt (fun entry ->
    position >= entry.start_pos && position < entry.end_pos
  ) pos_map
  |> Option.map (fun e -> e.value)

let find_nearest_entry pos_map position =
  let scored = List.map (fun entry ->
    let dist =
      if position < entry.start_pos then entry.start_pos - position
      else if position >= entry.end_pos then position - entry.end_pos + 1
      else 0
    in
    (dist, entry)
  ) pos_map in
  match List.sort (fun (d1, _) (d2, _) -> compare d1 d2) scored with
  | (_, entry) :: _ -> Some entry.value
  | [] -> None

let tokenize_with_positions expr_text =
  let state = Lexer_linear.create ~file:"<2d-expr>" expr_text in
  let tokens = Lexer_linear.tokenise state in

  List.filter_map (fun tok ->
    match tok.typ with
    | EOF -> None
    | NEWLINE -> None
    | _ ->
      let start_pos = tok.loc.column - 1 in
      let end_pos = start_pos + String.length tok.lexeme in
      Some (tok, start_pos, end_pos)
  ) tokens

let is_var_ident tok =
  match tok.typ with
  | IDENT s -> s = "V" || s = "Z" || s = "R" || s = "v" || s = "z" || s = "r"
  | _ -> false

let var_kind_of_ident s =
  match String.uppercase_ascii s with
  | "V" -> `V
  | "Z" -> `Z
  | "R" -> `R
  | _ -> `Z

let block_to_tokens_correlated block =
  let get_row_content row_type =
    List.find_opt (fun r -> r.row_type = row_type) block.rows
    |> Option.map (fun r -> r.content)
    |> Option.value ~default:""
  in

  let expr_content = get_row_content RowExpr in
  let v_content = get_row_content RowV in
  let k_content = get_row_content RowK in
  let s_content = get_row_content RowS in

  let v_map = build_position_map v_content in
  let k_map = build_position_map k_content in

  let v_list = build_position_map v_content |> List.map (fun e -> e.value) in
  let s_list = build_position_map s_content |> List.map (fun e -> e.value) in

  let expr_tokens_with_pos = tokenize_with_positions expr_content in

  let result_tokens = ref [] in
  let var_count = ref 0 in

  List.iter (fun (tok, start_pos, _end_pos) ->
    if is_var_ident tok then begin
      let var_kind = var_kind_of_ident tok.lexeme in
      let current_var_idx = !var_count in
      incr var_count;

      let index_str =
        match lookup_at_position v_map start_pos with
        | Some s -> Some s
        | None ->
          match lookup_at_position v_map (start_pos + 1) with
          | Some s -> Some s
          | None ->
            if current_var_idx < List.length v_list then
              Some (List.nth v_list current_var_idx)
            else
              find_nearest_entry v_map start_pos
      in

      let index = match index_str with
        | Some s -> (try int_of_string s with Failure _ -> 0)
        | None -> 0
      in

      let var_tok = match var_kind with
        | `V -> VAR_V index
        | `Z -> VAR_Z index
        | `R -> VAR_R index
      in

      let new_tok = make_tok var_tok tok.lexeme block.start_line (start_pos + 1) in
      result_tokens := new_tok :: !result_tokens;

      let k_list = build_position_map k_content |> List.map (fun e -> e.value) in
      let comp_str =
        if current_var_idx < List.length k_list then
          let s = List.nth k_list current_var_idx in
          if String.length s > 0 then Some s else None
        else
          match lookup_at_position k_map start_pos with
          | Some s when String.length s > 0 -> Some s
          | _ -> lookup_at_position k_map (start_pos + 1)
      in
      (match comp_str with
       | Some cs when String.length cs > 0 ->
         result_tokens := make_tok LBRACKET "[" block.start_line start_pos :: !result_tokens;
         let comp_tokens = tokenize_with_positions cs in
         List.iter (fun (ctok, _, _) ->
           result_tokens := { ctok with loc = { ctok.loc with line = block.start_line } } :: !result_tokens
         ) comp_tokens;
         result_tokens := make_tok RBRACKET "]" block.start_line start_pos :: !result_tokens
       | _ -> ());

      let type_str =
        if current_var_idx < List.length s_list then
          Some (List.nth s_list current_var_idx)
        else
          None
      in

      (match type_str with
       | Some ts when String.length ts > 0 ->
         let annot = Printf.sprintf "[:%s]" ts in
         let annot_tok = make_tok (TYPE_ANNOT annot) annot block.start_line (start_pos + 1) in
         result_tokens := annot_tok :: !result_tokens
       | _ -> ())
    end else begin
      let adjusted_tok = { tok with
        loc = { tok.loc with line = block.start_line; column = start_pos + 1 }
      } in
      result_tokens := adjusted_tok :: !result_tokens
    end
  ) expr_tokens_with_pos;

  let tokens = List.rev !result_tokens in
  tokens @ [make_tok NEWLINE "\n" block.end_line 0]

let block_to_tokens block =
  block_to_tokens_correlated block

let load_next_block state =
  match Grid.read_block state.grid_state with
  | None ->
    state.current_block <- None;
    state.block_tokens <- [make_tok EOF "" 0 0];
    state.token_pos <- 0
  | Some block ->
    state.current_block <- Some block;
    state.block_tokens <- block_to_tokens block;
    state.token_pos <- 0;
    state.default_provenance <- block.block_provenance;
    if state.check_alignment then
      Grid.check_block_alignment state.grid_state block

let next_token state =
  if state.current_block = None && state.block_tokens = [] then
    load_next_block state;

  if state.token_pos >= List.length state.block_tokens then begin
    load_next_block state;
    if state.token_pos >= List.length state.block_tokens then
      make_tok EOF "" 0 0
    else
      let tok = List.nth state.block_tokens state.token_pos in
      state.token_pos <- state.token_pos + 1;
      tok
  end else begin
    let tok = List.nth state.block_tokens state.token_pos in
    state.token_pos <- state.token_pos + 1;
    tok
  end

let is_plan_signature tokens =
  let rec has_pattern = function
    | { typ = IDENT _; _ } :: { typ = LPAREN; _ } :: rest ->
      let rec find_arrow = function
        | { typ = RPAREN; _ } :: { typ = ARROW; _ } :: _ -> true
        | [] -> false
        | _ :: rest -> find_arrow rest
      in
      find_arrow rest
    | _ :: rest -> has_pattern rest
    | [] -> false
  in
  has_pattern tokens

let inject_structure_tokens all_tokens =
  let rec take_since_newline collected = function
    | { typ = NEWLINE; _ } :: _ -> collected
    | { typ = LBRACE; _ } :: _ -> collected
    | t :: rest -> take_since_newline (t :: collected) rest
    | [] -> collected
  in

  let rec process acc in_plan = function
    | [] -> List.rev acc
    | ({ typ = NEWLINE; _ } as nl) :: rest when not in_plan ->
      let block_tokens = take_since_newline [] acc in
      if is_plan_signature block_tokens then
        let lbrace = make_tok LBRACE "{" nl.loc.line 0 in
        process (nl :: lbrace :: acc) true rest
      else
        process (nl :: acc) in_plan rest
    | { typ = FIN; _ } :: rest when in_plan ->
      let rbrace = make_tok RBRACE "}" 0 0 in
      process (rbrace :: acc) false rest
    | { typ = FIN; _ } :: rest ->
      process acc in_plan rest
    | ({ typ = EOF; _ } as eof) :: rest when in_plan ->
      let rbrace = make_tok RBRACE "}" eof.loc.line (eof.loc.column) in
      process (eof :: rbrace :: acc) false rest
    | tok :: rest ->
      process (tok :: acc) in_plan rest
  in
  process [] false all_tokens

let tokenise state =
  let rec collect acc =
    let tok = next_token state in
    match tok.typ with
    | EOF -> List.rev (tok :: acc)
    | _ -> collect (tok :: acc)
  in
  let raw_tokens = collect [] in
  inject_structure_tokens raw_tokens

let tokenise_no_newlines state =
  tokenise state
  |> List.filter (fun tok -> tok.typ <> NEWLINE)

let tokenise_string ?(file="<string>") ?(provenance=unknown) ?(check_alignment=true) source =
  let state = create ~file ~provenance ~check_alignment source in
  tokenise state

let tokenise_with_warnings ?(file="<string>") ?(provenance=unknown) source =
  let state = create ~file ~provenance ~check_alignment:true source in
  let tokens = tokenise state in
  let warnings = Grid.get_warnings state.grid_state in
  (tokens, warnings)

let current_provenance state = state.default_provenance

let is_2d_notation source =
  let lines = String.split_on_char '\n' source in
  List.exists (fun line ->
    let trimmed = String.trim line in
    String.length trimmed >= 2 &&
    (String.sub trimmed 0 2 = "V|" ||
     String.sub trimmed 0 2 = "K|" ||
     String.sub trimmed 0 2 = "S|")
  ) lines

type notation_type = Notation2D | NotationLinear | NotationMixed

let detect_notation source =
  if is_2d_notation source then
    Notation2D
  else
    NotationLinear

type token_stream = {
  mutable tokens: token list;
  mutable position: int;
  stream_provenance: provenance;
  stream_warnings: Grid.alignment_warning list;
}

let token_stream_from_source ?(file="<input>") ?(provenance=unknown) ?(check_alignment=true) source =
  let state = create ~file ~provenance ~check_alignment source in
  let tokens = tokenise state in
  let warnings = Grid.get_warnings state.grid_state in
  {
    tokens;
    position = 0;
    stream_provenance = state.default_provenance;
    stream_warnings = warnings;
  }

let stream_warnings stream = stream.stream_warnings

let stream_has_warnings stream = stream.stream_warnings <> []

let format_stream_warnings stream =
  if stream.stream_warnings = [] then ""
  else begin
    let buf = Buffer.create 512 in
    Buffer.add_string buf "2D notation alignment warnings:\n";
    List.iter (fun (warn : Grid.alignment_warning) ->
      Buffer.add_string buf (Printf.sprintf "  Line %d, column %d: %s\n"
        warn.warn_line warn.warn_col warn.warn_message);
      (match warn.expected_col with
       | Some exp ->
         Buffer.add_string buf (Printf.sprintf "    Suggestion: move to column %d\n" exp)
       | None -> ())
    ) stream.stream_warnings;
    Buffer.contents buf
  end

let stream_next stream =
  if stream.position >= List.length stream.tokens then
    make_tok EOF "" 0 0
  else begin
    let tok = List.nth stream.tokens stream.position in
    stream.position <- stream.position + 1;
    tok
  end

let stream_peek stream =
  if stream.position >= List.length stream.tokens then
    make_tok EOF "" 0 0
  else
    List.nth stream.tokens stream.position

let stream_provenance stream = stream.stream_provenance

let get_warnings state = Grid.get_warnings state.grid_state

let clear_warnings state = Grid.clear_warnings state.grid_state

let format_warnings state = Grid.format_warnings state.grid_state

let has_warnings state = Grid.get_warnings state.grid_state <> []

let format_current_block state =
  match state.current_block with
  | Some block -> Grid.format_block_grid block
  | None -> ""
