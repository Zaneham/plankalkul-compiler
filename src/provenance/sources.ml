(** Source Provenance Tracking

    Every Plankalkül construct can be traced to its historical source.
    This matters because Zuse refined the language over decades.

    Provenance layers (in order of authority):
    1. PRIMARY: 1941-1945 wartime manuscripts
    2. SECONDARY: 1945-1949 post-war formalisations
    3. TERTIARY: 1972 published paper
    4. QUATERNARY: 1998-2000 FU Berlin implementation choices
    5. QUINARY: 2010 Bruines formal semantics

    When in doubt, defer to the earlier source.

    Attribution: Provenance system by Zane Hambly (2025-2026)
    For Prof. Raúl Rojas and the academic community.
*)

(* ============================================================================
   ARCHIVE IDENTIFIERS
   ============================================================================ *)

(** Zuse Internet Archive document identifiers *)
type archive_id =
  | ZIA_0367      (** 1941 - Logical formalisms, chess *)
  | ZIA_0368      (** 1941 - Schachprogramme preliminary *)
  | ZIA_0410      (** 1942 - Chess programming elaborations *)
  | TAL_1         (** Theorie der Angewandten Logistik, Book 1 *)
  | TAL_2         (** Theorie der Angewandten Logistik, Book 2 *)
  | GMD_1972      (** 1972 GMD Report "Der Plankalkül" *)
  | FUB_2000      (** FU Berlin implementation *)
  | Bruines_2010  (** Bruines thesis *)
  | Hovestar      (** Hovestar reference implementation *)
  | Custom of string  (** For other sources *)
[@@deriving show, eq]

(* ============================================================================
   MANUSCRIPT DATABASE
   ============================================================================ *)

(** Full manuscript metadata *)
type manuscript = {
  id: archive_id;
  title_de: string;           (** Original German title *)
  title_en: string;           (** English translation *)
  year: int;
  pages: int;
  archive_url: string option; (** Link to Zuse Internet Archive *)
  description: string;
}
[@@deriving show]

(** The canonical manuscript database *)
let manuscripts = [
  { id = ZIA_0367;
    title_de = "Vorbereitung des Plankalküls, logische Formalismen, Schachspiel";
    title_en = "Preparation of Plankalkül, logical formalisms, chess";
    year = 1941;
    pages = 28;
    archive_url = Some "https://zuse.zib.de/item/gHI1cPsUuQDmRlHs";
    description = "Movement functions, Boolean logic, piece encoding. \
                   Contains the foundational movement formulas for all chess pieces.";
  };
  { id = ZIA_0368;
    title_de = "Vorarbeiten zum Plankalkül. Schachprogramme";
    title_en = "Preliminary work on Plankalkül. Chess programs";
    year = 1941;
    pages = 22;
    archive_url = Some "https://zuse.zib.de/item/gHI1cPsUuQDmRlHt";
    description = "Board representation, directions, color encoding, zones. \
                   Contains the 8 compass directions and color functions.";
  };
  { id = ZIA_0410;
    title_de = "Ausarbeitungen zur Programmierung des Schachspiels";
    title_en = "Elaborations on the programming of chess";
    year = 1942;
    pages = 28;
    archive_url = Some "https://zuse.zib.de/item/gHI1cPsUuQDmRlHu";
    description = "Game tree, minimax algorithm, state transitions. \
                   The world's first minimax implementation, 8 years before Shannon.";
  };
  { id = TAL_2;
    title_de = "Theorie der Angewandten Logistik, Band 2";
    title_en = "Theory of Applied Logistics, Volume 2";
    year = 1945;
    pages = 0;  (* Unknown *)
    archive_url = None;
    description = "Post-war formalisation of Plankalkül. \
                   More systematic treatment of the type system and control flow.";
  };
  { id = GMD_1972;
    title_de = "Der Plankalkül";
    title_en = "The Plankalkül";
    year = 1972;
    pages = 285;
    archive_url = None;
    description = "Zuse's retrospective paper describing Plankalkül. \
                   Published 27 years after design, some details may be reconstructed.";
  };
  { id = FUB_2000;
    title_de = "Plankalkül: Die erste höhere Programmiersprache und ihre Implementation";
    title_en = "Plankalkül: The First High-Level Programming Language and its Implementation";
    year = 2000;
    pages = 0;
    archive_url = Some "http://www.zib.de/zuse/Inhalt/Programme/Plankalkuel/Plankalkuel-Report/Plankalkuel-Report.htm";
    description = "Prof. Raúl Rojas et al. at FU Berlin. \
                   First working implementation, 55 years after design.";
  };
  { id = Bruines_2010;
    title_de = "Plankalkül: Formale Semantik";
    title_en = "Plankalkül: Formal Semantics";
    year = 2010;
    pages = 21;
    archive_url = None;
    description = "Bachelor thesis providing formal semantics. \
                   Authoritative source for loop semantics (W through W6) and Fin behavior.";
  };
]

(** Look up manuscript by ID *)
let find_manuscript id =
  List.find_opt (fun m -> m.id = id) manuscripts

(* ============================================================================
   CONFIDENCE LEVELS
   ============================================================================ *)

(** How confident are we in this provenance? *)
type confidence =
  | Verified of string option
      (** Exact match to manuscript text. Optional: the original text. *)
  | Interpreted
      (** Clear meaning from manuscript, adapted for our syntax. *)
  | Inferred
      (** Not explicitly in manuscript, but logically necessary. *)
  | Deviation of string
      (** Deliberate departure from manuscript, with reason. *)
[@@deriving show, eq]

(** Is this confidence level trustworthy for academic citation? *)
let is_citable = function
  | Verified _ | Interpreted -> true
  | Inferred | Deviation _ -> false

(* ============================================================================
   SOURCE REFERENCES
   ============================================================================ *)

(** Provenance layer - how authoritative is this interpretation? *)
type provenance_layer =
  | Primary     (** 1941-1945 manuscripts - the real stuff *)
  | Secondary   (** 1945-1949 post-war formalisation *)
  | Tertiary    (** 1972 published paper *)
  | Quaternary  (** 1998-2000 implementation decisions *)
  | Quinary     (** 2010 formal semantics *)
  | Modern      (** Our additions/interpretations *)
[@@deriving show, eq, ord]

(** Year ranges for each layer *)
let layer_years = function
  | Primary -> (1941, 1945)
  | Secondary -> (1945, 1949)
  | Tertiary -> (1972, 1972)
  | Quaternary -> (1998, 2000)
  | Quinary -> (2010, 2010)
  | Modern -> (2025, 2100)

(** Determine layer from year *)
let layer_of_year year =
  if year >= 1941 && year <= 1945 then Primary
  else if year >= 1945 && year <= 1949 then Secondary
  else if year = 1972 then Tertiary
  else if year >= 1998 && year <= 2000 then Quaternary
  else if year = 2010 then Quinary
  else Modern

(** Source document reference *)
type source_ref = {
  archive_id: archive_id;
  page: int option;           (** Page number if known *)
  section: string option;     (** Section/chapter if applicable *)
  year: int;                  (** Year of the source *)
  original_text: string option;  (** Exact transcription from manuscript *)
  original_formula: string option;  (** Mathematical formula if applicable *)
}
[@@deriving show]

(** Create a source reference *)
let make_source_ref ~archive_id ?page ?section ?original_text ?original_formula () =
  let year = match find_manuscript archive_id with
    | Some m -> m.year
    | None -> 0
  in
  { archive_id; page; section; year; original_text; original_formula }

(* ============================================================================
   FULL PROVENANCE
   ============================================================================ *)

(** Full provenance information for a construct *)
type provenance = {
  primary_source: source_ref option;     (** Main source *)
  supporting_sources: source_ref list;   (** Additional sources *)
  confidence: confidence;
  layer: provenance_layer;
  notes: string option;                  (** Implementation notes *)
  verified_by: string option;            (** Who verified against manuscript *)
  verification_date: string option;      (** When verified *)
}
[@@deriving show]

(** Unknown/unattributed provenance *)
let unknown = {
  primary_source = None;
  supporting_sources = [];
  confidence = Inferred;
  layer = Modern;
  notes = Some "Source not yet attributed";
  verified_by = None;
  verification_date = None;
}

(** Create an inferred provenance with notes *)
let inferred ?notes () = {
  unknown with
  confidence = Inferred;
  notes;
}

(* ============================================================================
   PROVENANCE BUILDERS
   ============================================================================ *)

(** Create a provenance from the 1941 logical formalisms manuscript *)
let from_zia_0367 ?page ?section ?original_text ?original_formula ?notes () = {
  primary_source = Some {
    archive_id = ZIA_0367;
    page;
    section;
    year = 1941;
    original_text;
    original_formula;
  };
  supporting_sources = [];
  confidence = (match original_text with Some t -> Verified (Some t) | None -> Interpreted);
  layer = Primary;
  notes;
  verified_by = None;
  verification_date = None;
}

(** Create a provenance from the 1941 schachprogramme manuscript *)
let from_zia_0368 ?page ?section ?original_text ?original_formula ?notes () = {
  primary_source = Some {
    archive_id = ZIA_0368;
    page;
    section;
    year = 1941;
    original_text;
    original_formula;
  };
  supporting_sources = [];
  confidence = (match original_text with Some t -> Verified (Some t) | None -> Interpreted);
  layer = Primary;
  notes;
  verified_by = None;
  verification_date = None;
}

(** Create a provenance from the 1942 chess manuscript *)
let from_zia_0410 ?page ?section ?original_text ?original_formula ?notes () = {
  primary_source = Some {
    archive_id = ZIA_0410;
    page;
    section;
    year = 1942;
    original_text;
    original_formula;
  };
  supporting_sources = [];
  confidence = (match original_text with Some t -> Verified (Some t) | None -> Interpreted);
  layer = Primary;
  notes;
  verified_by = None;
  verification_date = None;
}

(** Create a provenance from Bruines thesis *)
let from_bruines ?page ?section ?original_text ?notes () = {
  primary_source = Some {
    archive_id = Bruines_2010;
    page;
    section;
    year = 2010;
    original_text;
    original_formula = None;
  };
  supporting_sources = [];
  confidence = (match original_text with Some t -> Verified (Some t) | None -> Interpreted);
  layer = Quinary;
  notes;
  verified_by = None;
  verification_date = None;
}

(** Create a provenance from FU Berlin implementation *)
let from_fu_berlin ?notes () = {
  primary_source = Some {
    archive_id = FUB_2000;
    page = None;
    section = None;
    year = 2000;
    original_text = None;
    original_formula = None;
  };
  supporting_sources = [];
  confidence = Interpreted;
  layer = Quaternary;
  notes;
  verified_by = None;
  verification_date = None;
}

(** Create a verified provenance with original text *)
let verified ~archive_id ~page ~original_text ?original_formula ?notes () =
  let year = match find_manuscript archive_id with
    | Some m -> m.year
    | None -> 0
  in
  {
    primary_source = Some {
      archive_id;
      page = Some page;
      section = None;
      year;
      original_text = Some original_text;
      original_formula;
    };
    supporting_sources = [];
    confidence = Verified (Some original_text);
    layer = layer_of_year year;
    notes;
    verified_by = None;
    verification_date = None;
  }

(** Mark a provenance as verified by someone *)
let mark_verified ~by ~date prov = {
  prov with
  verified_by = Some by;
  verification_date = Some date;
}

(** Add a supporting source *)
let add_supporting_source src prov = {
  prov with
  supporting_sources = src :: prov.supporting_sources;
}

(** Create a deviation provenance *)
let deviation ~reason ?based_on ?notes () = {
  primary_source = based_on;
  supporting_sources = [];
  confidence = Deviation reason;
  layer = Modern;
  notes;
  verified_by = None;
  verification_date = None;
}

(* ============================================================================
   PROVENANCE MERGING
   ============================================================================ *)

(** Compare provenance authority - earlier/more primary wins *)
let compare_authority p1 p2 =
  compare_provenance_layer p1.layer p2.layer

(** Merge two provenances, preferring the more authoritative *)
let merge p1 p2 =
  if compare_authority p1 p2 <= 0 then
    { p1 with supporting_sources =
        (match p2.primary_source with
         | Some s -> s :: p1.supporting_sources @ p2.supporting_sources
         | None -> p1.supporting_sources @ p2.supporting_sources) }
  else
    { p2 with supporting_sources =
        (match p1.primary_source with
         | Some s -> s :: p2.supporting_sources @ p1.supporting_sources
         | None -> p2.supporting_sources @ p1.supporting_sources) }

(* ============================================================================
   CITATION FORMATTING
   ============================================================================ *)

(** Archive ID to string *)
let archive_id_to_string = function
  | ZIA_0367 -> "ZIA-0367"
  | ZIA_0368 -> "ZIA-0368"
  | ZIA_0410 -> "ZIA-0410"
  | TAL_1 -> "TAL-1"
  | TAL_2 -> "TAL-2"
  | GMD_1972 -> "GMD-1972"
  | FUB_2000 -> "FUB-2000"
  | Bruines_2010 -> "Bruines-2010"
  | Hovestar -> "Hovestar"
  | Custom s -> s

(** Pretty-print a source reference (short form) *)
let pp_source_short fmt src =
  let id_str = archive_id_to_string src.archive_id in
  match src.page with
  | Some p -> Format.fprintf fmt "%s, p.%d" id_str p
  | None -> Format.fprintf fmt "%s" id_str

(** Pretty-print a provenance (short form for error messages) *)
let pp_short fmt prov =
  match prov.primary_source with
  | None ->
    (match prov.confidence with
     | Inferred -> Format.fprintf fmt "[inferred]"
     | Deviation r -> Format.fprintf fmt "[deviation: %s]" r
     | _ -> Format.fprintf fmt "[unknown]")
  | Some src ->
    let conf_str = match prov.confidence with
      | Verified _ -> ""
      | Interpreted -> " (interpreted)"
      | Inferred -> " (inferred)"
      | Deviation _ -> " (deviation)"
    in
    Format.fprintf fmt "[%a (%d)%s]" pp_source_short src src.year conf_str

(** Full academic citation *)
let cite_source src =
  let base = match src.archive_id with
    | ZIA_0367 ->
        "Zuse, K. (1941). \"Vorbereitung des Plankalküls, logische Formalismen, Schachspiel\" [ZIA-0367]"
    | ZIA_0368 ->
        "Zuse, K. (1941). \"Vorarbeiten zum Plankalkül. Schachprogramme\" [ZIA-0368]"
    | ZIA_0410 ->
        "Zuse, K. (1942). \"Ausarbeitungen zur Programmierung des Schachspiels\" [ZIA-0410]"
    | TAL_1 ->
        "Zuse, K. (1945). \"Theorie der Angewandten Logistik\", Band 1"
    | TAL_2 ->
        "Zuse, K. (1945). \"Theorie der Angewandten Logistik\", Band 2"
    | GMD_1972 ->
        "Zuse, K. (1972). \"Der Plankalkül\" [GMD Report]"
    | FUB_2000 ->
        "Rojas, R. et al. (2000). \"Plankalkül: The First High-Level Programming Language and its Implementation\", FU Berlin"
    | Bruines_2010 ->
        "Bruines, B. (2010). \"Plankalkül: Formal Semantics\" [Bachelor Thesis, Radboud University]"
    | Hovestar ->
        "Hovestar Project. \"Plankalkül Implementation\" [GitHub]"
    | Custom s -> s
  in
  match src.page, src.section with
  | Some p, Some s -> Printf.sprintf "%s, Section %s, p.%d" base s p
  | Some p, None -> Printf.sprintf "%s, p.%d" base p
  | None, Some s -> Printf.sprintf "%s, Section %s" base s
  | None, None -> base

(** Full citation for a provenance *)
let cite prov =
  let primary = match prov.primary_source with
    | None ->
      (match prov.confidence with
       | Inferred -> "Inferred interpretation"
       | Deviation r -> Printf.sprintf "Deviation from original: %s" r
       | _ -> "Unknown source")
    | Some src -> cite_source src
  in
  let supporting = match prov.supporting_sources with
    | [] -> ""
    | srcs ->
        let cites = List.map cite_source srcs in
        Printf.sprintf "\n  Supporting: %s" (String.concat "; " cites)
  in
  let original = match prov.primary_source with
    | Some { original_text = Some t; _ } -> Printf.sprintf "\n  Original: \"%s\"" t
    | Some { original_formula = Some f; _ } -> Printf.sprintf "\n  Formula: %s" f
    | _ -> ""
  in
  let verification = match prov.verified_by, prov.verification_date with
    | Some by, Some date -> Printf.sprintf "\n  Verified by %s on %s" by date
    | _ -> ""
  in
  primary ^ original ^ supporting ^ verification

(** Generate a C comment with provenance *)
let to_c_comment prov =
  match prov.primary_source with
  | None -> "/* Source: inferred */"
  | Some src ->
      let base = Printf.sprintf "/* Source: %s (%d)"
                   (archive_id_to_string src.archive_id) src.year in
      let page = match src.page with
        | Some p -> Printf.sprintf ", p.%d" p
        | None -> ""
      in
      let original = match src.original_formula with
        | Some f -> Printf.sprintf "\n   Original: %s" f
        | None -> match src.original_text with
          | Some t when String.length t < 60 -> Printf.sprintf "\n   Original: \"%s\"" t
          | _ -> ""
      in
      let conf = match prov.confidence with
        | Verified _ -> "\n   Confidence: VERIFIED"
        | Interpreted -> "\n   Confidence: INTERPRETED"
        | Inferred -> "\n   Confidence: INFERRED"
        | Deviation r -> Printf.sprintf "\n   Confidence: DEVIATION - %s" r
      in
      base ^ page ^ original ^ conf ^ " */"

(* ============================================================================
   FEATURE REGISTRY
   ============================================================================ *)

(** Language features and their provenance *)
type feature =
  | FeatureLoopW
  | FeatureLoopW0
  | FeatureLoopW1
  | FeatureLoopW2
  | FeatureLoopW3
  | FeatureLoopW4
  | FeatureLoopW5
  | FeatureLoopW6
  | FeatureVarV
  | FeatureVarZ
  | FeatureVarR
  | FeatureTypeA8
  | FeatureTypeA9
  | FeatureTypeA10
  | FeatureTypeA11
  | FeatureTypeA12
  | FeatureTypeA13
  | FeatureFin
  | FeatureMu
  | FeatureLambda
  | FeatureQuantifierForall
  | FeatureQuantifierExists
  | FeatureSetFilter
  | FeatureSetFilterSeq
  | FeatureFindUnique
  | Feature2DNotation
  | FeatureLinearNotation
  | FeaturePlanTemplates
  | FeatureAssertions
  | FeatureTruthTables
  | FeatureDeltaNotation
  | FeatureChessPredicates
[@@deriving show, eq]

(** Feature registry with canonical provenance *)
let feature_provenance = function
  | FeatureLoopW ->
      from_bruines ~page:7
        ~original_text:"W [ b1→S1 | b2→S2 | ... ] - repeat until all false or Fin" ()
  | FeatureLoopW0 ->
      from_bruines ~page:7
        ~original_text:"W0(n) [ S ] - repeat n times, counter hidden" ()
  | FeatureLoopW1 ->
      from_bruines ~page:7
        ~original_text:"W1(n) ⇒ i [ S ] - count up: i = 0..n-1" ()
  | FeatureLoopW2 ->
      from_bruines ~page:7
        ~original_text:"W2(n) ⇒ i [ S ] - count down: i = n-1..0" ()
  | FeatureLoopW3 ->
      from_bruines ~page:7
        ~original_text:"W3(n,m) ⇒ i [ S ] - while m ≥ n" ()
  | FeatureLoopW4 ->
      from_bruines ~page:7
        ~original_text:"W4(n,m) ⇒ i [ S ] - while m ≤ n" ()
  | FeatureLoopW5 ->
      from_bruines ~page:7
        ~original_text:"W5(n,m) ⇒ i [ S ] - toward m, auto-direction" ()
  | FeatureLoopW6 ->
      from_bruines ~page:12
        ~original_text:"W6 - iterate list until empty"
        ~notes:"Discovered from Bruines p.12, not in earlier sources" ()
  | FeatureFin ->
      from_bruines ~page:7
        ~original_text:"Fin - break/early termination, also returned by µ/λ" ()
  | FeatureMu ->
      from_bruines ~page:8
        ~original_text:"µx(x ∈ l ∧ R(x)) - forward iterator, returns Fin when exhausted" ()
  | FeatureLambda ->
      from_bruines ~page:8
        ~original_text:"λx(x ∈ l ∧ R(x)) - backward iterator, returns Fin when exhausted" ()
  | FeatureQuantifierForall ->
      from_bruines ~page:12
        ~original_text:"(x)(x ∈ l ⇒ R(x)) - universal quantifier as expression" ()
  | FeatureQuantifierExists ->
      from_bruines ~page:12
        ~original_text:"(Ex)(x ∈ l ⇒ R(x)) - existential quantifier as expression" ()
  | FeatureSetFilter ->
      from_bruines ~page:12
        ~original_text:"ˆx(x ∈ l ∧ R(x)) - filter to set" ()
  | FeatureSetFilterSeq ->
      from_bruines ~page:12
        ~original_text:"ˆˆx(x ∈ l ∧ R(x)) - filter to sequence" ()
  | FeatureFindUnique ->
      from_bruines ~page:12
        ~original_text:"´x(x ∈ l ∧ R(x)) - find unique element" ()
  | Feature2DNotation ->
      from_zia_0367 ~page:1
        ~notes:"2D notation with V/K/S rows is the original Zuse notation" ()
  | FeatureLinearNotation ->
      from_fu_berlin
        ~notes:"Linearised notation is a modern adaptation for ease of input" ()
  | FeaturePlanTemplates ->
      inferred ~notes:"Plan templates for higher-order functions, source unclear" ()
  | FeatureAssertions ->
      from_bruines ~page:4
        ~original_text:"Pre/post conditions on plans" ()
  | FeatureTruthTables ->
      from_zia_0368 ~page:20
        ~original_text:"Truth tables for piece movement validation"
        ~notes:"Decision tables mapping boolean inputs to outputs (pp.20-21). \
                Also visible in ZIA-0410 p.4. Zuse used these for chess piece \
                movement rules - one of the earliest forms of declarative programming." ()
  | FeatureDeltaNotation ->
      from_zia_0410 ~page:1
        ~original_text:"PΔ300 - Delta plan group for chess"
        ~notes:"Greek letter prefixes (Δ, Σ, Φ) used throughout ZIA-0410 to \
                categorize plans. The Delta (Δ) notation specifically denotes \
                chess-related plans in Zuse's manuscripts." ()
  | FeatureChessPredicates ->
      from_zia_0368 ~page:1
        ~original_text:"Nord, Süd, Ost, West, NO, SO, SW, NW direction functions"
        ~notes:"Chess predicates defined across ZIA-0367/0368: \
                Eck (corner), Kant (edge), Farb (color), Zone, Quadr (quadrant). \
                Movement predicates: Orth (orthogonal), Dag (diagonal), \
                Spr (Springer/knight), Ben (adjacent), gus (queen-like)." ()
  | _ -> unknown

(** Get the canonical provenance for a language feature *)
let provenance_for_feature = feature_provenance

(* ============================================================================
   BIBLIOGRAPHY GENERATION
   ============================================================================ *)

(** Collect all unique sources from a list of provenances *)
let collect_sources provs =
  let all_sources = List.concat_map (fun p ->
    match p.primary_source with
    | Some s -> s :: p.supporting_sources
    | None -> p.supporting_sources
  ) provs in
  (* Deduplicate by archive_id and page *)
  let seen = Hashtbl.create 16 in
  List.filter (fun s ->
    let key = (s.archive_id, s.page) in
    if Hashtbl.mem seen key then false
    else (Hashtbl.add seen key (); true)
  ) all_sources

(** Generate a bibliography from collected sources *)
let generate_bibliography provs =
  let sources = collect_sources provs in
  let by_archive = Hashtbl.create 8 in
  List.iter (fun s ->
    let existing = Hashtbl.find_opt by_archive s.archive_id |> Option.value ~default:[] in
    Hashtbl.replace by_archive s.archive_id (s :: existing)
  ) sources;

  let buf = Buffer.create 1024 in
  Buffer.add_string buf "=== Bibliography ===\n\n";

  Hashtbl.iter (fun archive_id sources ->
    let manuscript = find_manuscript archive_id in
    let title = match manuscript with
      | Some m -> Printf.sprintf "%s (%d)" m.title_en m.year
      | None -> archive_id_to_string archive_id
    in
    Buffer.add_string buf (Printf.sprintf "%s\n" title);

    (* List pages used *)
    let pages = List.filter_map (fun s -> s.page) sources
                |> List.sort_uniq compare in
    if pages <> [] then begin
      let page_str = String.concat ", " (List.map string_of_int pages) in
      Buffer.add_string buf (Printf.sprintf "  Pages: %s\n" page_str)
    end;
    Buffer.add_string buf "\n"
  ) by_archive;

  Buffer.contents buf
