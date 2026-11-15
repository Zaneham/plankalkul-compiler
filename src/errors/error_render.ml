open Error_types

module Colors = struct
  let reset = "\027[0m"
  let bold = "\027[1m"
  let red = "\027[31m"
  let yellow = "\027[33m"
  let blue = "\027[34m"
  let cyan = "\027[36m"
  let green = "\027[32m"
  let dim = "\027[2m"

  let use_colors = ref true

  let apply code s =
    if !use_colors then code ^ s ^ reset else s
end

let format_severity severity =
  let (color, text) = match severity with
    | Error -> (Colors.red, "ERROR")
    | Warning -> (Colors.yellow, "WARNING")
    | Note -> (Colors.blue, "NOTE")
    | Hint -> (Colors.green, "HINT")
  in
  Colors.apply (Colors.bold ^ color) text

let format_code code =
  Colors.apply Colors.cyan (Printf.sprintf "[%s]" (error_code_str code))

let format_location span =
  Colors.apply Colors.bold (
    Printf.sprintf "%s:%d:%d" span.file span.start_line span.start_col
  )

let generate_caret_line start_col end_col =
  let spaces = String.make (start_col - 1) ' ' in
  let carets = String.make (max 1 (end_col - start_col)) '^' in
  Colors.apply Colors.red (spaces ^ carets)

let format_source_line line_num content =
  let num_str = Printf.sprintf "%5d" line_num in
  Printf.sprintf "%s | %s"
    (Colors.apply Colors.dim num_str)
    content

let render_error err =
  let buf = Buffer.create 256 in

  Buffer.add_string buf (format_severity err.severity);
  Buffer.add_char buf ' ';
  Buffer.add_string buf (format_code err.code);
  Buffer.add_string buf ": ";
  Buffer.add_string buf (Colors.apply Colors.bold (error_description err.code));
  if err.message <> error_description err.code then begin
    Buffer.add_string buf " - ";
    Buffer.add_string buf err.message
  end;
  Buffer.add_char buf '\n';

  Buffer.add_string buf "  ";
  Buffer.add_string buf (Colors.apply Colors.dim "Location: ");
  Buffer.add_string buf (format_location err.span);
  Buffer.add_char buf '\n';

  if err.context_lines <> [] then begin
    List.iter (fun (line_num, content) ->
      Buffer.add_string buf (format_source_line line_num content);
      Buffer.add_char buf '\n';

      if line_num = err.span.start_line then begin
        Buffer.add_string buf "      | ";
        let end_col = if err.span.end_line = line_num then err.span.end_col else String.length content + 1 in
        Buffer.add_string buf (generate_caret_line err.span.start_col end_col);
        Buffer.add_char buf '\n'
      end
    ) err.context_lines
  end;

  (match err.suggestion with
   | Some sug ->
     Buffer.add_string buf "  ";
     Buffer.add_string buf (Colors.apply Colors.green "Suggestion: ");
     Buffer.add_string buf sug;
     Buffer.add_char buf '\n'
   | None -> ());

  if err.related <> [] then begin
    Buffer.add_string buf "  ";
    Buffer.add_string buf (Colors.apply Colors.dim "Related: ");
    let locs = List.map format_location err.related in
    Buffer.add_string buf (String.concat ", " locs);
    Buffer.add_char buf '\n'
  end;

  Buffer.contents buf

let render_errors errors =
  let rendered = List.map render_error errors in
  String.concat "\n" rendered

let render_summary collector =
  let error_count = count_by_severity collector Error in
  let warning_count = count_by_severity collector Warning in

  let buf = Buffer.create 64 in

  if error_count > 0 then begin
    Buffer.add_string buf (Colors.apply Colors.red (
      Printf.sprintf "%d error%s"
        error_count
        (if error_count = 1 then "" else "s")
    ))
  end;

  if warning_count > 0 then begin
    if error_count > 0 then Buffer.add_string buf ", ";
    Buffer.add_string buf (Colors.apply Colors.yellow (
      Printf.sprintf "%d warning%s"
        warning_count
        (if warning_count = 1 then "" else "s")
    ))
  end;

  if error_count = 0 && warning_count = 0 then
    Buffer.add_string buf (Colors.apply Colors.green "No errors");

  Buffer.contents buf

let print_error err =
  prerr_string (render_error err)

let print_errors collector =
  prerr_string (render_errors (get_errors collector));
  prerr_newline ();
  prerr_endline (render_summary collector)

let read_context_lines filename start_line end_line =
  try
    let ic = open_in filename in
    let lines = ref [] in
    let line_num = ref 1 in
    try
      while !line_num <= end_line do
        let line = input_line ic in
        if !line_num >= start_line then
          lines := (!line_num, line) :: !lines;
        incr line_num
      done;
      close_in ic;
      List.rev !lines
    with End_of_file ->
      close_in ic;
      List.rev !lines
  with Sys_error _ -> []

let enrich_with_context err =
  let start = max 1 (err.span.start_line - 1) in
  let finish = err.span.end_line + 1 in
  let context = read_context_lines err.span.file start finish in
  with_context err context

let format_simple err =
  Printf.sprintf "%s:%d:%d: %s: %s"
    err.span.file
    err.span.start_line
    err.span.start_col
    (match err.severity with Error -> "error" | Warning -> "warning" | Note -> "note" | Hint -> "hint")
    err.message

let disable_colors () =
  Colors.use_colors := false

let enable_colors () =
  Colors.use_colors := true
