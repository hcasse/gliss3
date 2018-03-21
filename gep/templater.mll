{

(** Templater allows to generate files from templates.
	This templates may contains special token that are replaced
	by values retrieved from a dictionnary.

	Basically, an expression "$(identifier)" is replaced by a text
	found in the dictionnary.

	Templates language support support conditional statement
	in form "$(if identifier) ... $(end)" or
	"$(if identifier) ... $(else) ... $(end)". The identifier is looked
	in the dictionnary and must be resolved as a boolean.

	Loops are allowed using "$(foreach identifier) ... $(end)". In this
	case, the identifier must be resolved to a collection and the loop body
	is generated as many times there is elements in the collection.
	Identifiers contained in the body are resolved against special
	dictionnaries associated with each collection element.

	Finally, notes that "$$" expression is reduceded to "$$".
	*)

exception Error of string

(** Current file. *)
let file = ref ""

(** Current line. *)
let line = ref 0

(** Raise a template error.
	@param msg	Error message. *)
let error msg =
	raise (Error (Printf.sprintf "%s:%d: %s" !file !line msg))

(** Fallback symbol to look if a text symbol is not found (TEXT or FUN). *)
let fallback_text = "!text"

(** Fallback symbol to look for if a collection symbol is not found (COLL or GEN_COLL). *)
let fallback_coll = "!coll"

(** Fallback symbol to look if isdef fail (BOOL or GEN_BOOL). *)
let fallback_ifdef = "!isdef"

(** Fallback symbol to evaluate a symbol that is not found (BOOL or GEN_BOOL). *)
let fallback_bool = "!bool"

(** Type of dictionnaries. *)
type dict_t = (string * value_t) list

(** Values of a dictionnary *)
and  value_t =
	  TEXT of (out_channel -> unit)						(** function called when identifier is found *)
	| COLL of ((dict_t -> unit) -> dict_t -> unit)		(** collection : argument function must be called for each element
															with a dictionnary fixed for the current element. *)
	| BOOL of (unit -> bool)							(** boolean value *)
	| FUN of (out_channel -> string -> unit)			(** function value *)
	| GEN_COLL of (string -> (dict_t -> unit) -> dict_t -> unit)
		(** collection : argument function must be called for each element with a dictionary fixed for the current element. *)
	| GEN_BOOL of (string -> bool)						(** generic boolean symbol *)


type state_t =
	| TOP
	| THEN
	| ELSE
	| FOREACH
	| WITH


(** Perform text evaluation (and function if any)
	@param out	Out channel.
	@param dict	Used dictionnary.
	@param id	Text identifier. *)
let do_text out dict id =
	let fail _ = raise (Error (Printf.sprintf "symbol \"%s\" cannot be found or has bad type" id)) in

	(* parse arguments *)
	let p =  try String.index id ':' with Not_found -> -1 in
	let id = if p == -1 then id else String.sub id 0 p in
	let args = if p == -1 then "" else (String.sub id (p + 1) ((String.length id) - p - 1)) in
		
	let lookup id args fb =
		try
			(match List.assoc id dict with
			| TEXT f -> f out
			| FUN f ->  f out args
			| _ -> fb ())
		with Not_found ->
			fb () in
	
	try
		lookup id args (fun _ -> lookup fallback_text id fail)
	with Not_found ->
		raise (Error (Printf.sprintf "uncaught Not_found in generation with \"%s\"" id))


(** Get a collection.
	@param dict	Dictionnary to look in.
	@param id	Identifier.
	@return		Found collection function. *)
let do_coll dict id =

	let lookup i f =
		try
			match List.assoc i dict with
			| COLL f -> f
			| GEN_COLL f -> (f id)
			| _ -> f ()
		with Not_found ->
			f () in

	lookup id
		(fun _ -> lookup fallback_coll
			(fun _ -> error (Printf.sprintf "\"%s\" is undefined or not a collection" id)))


(** Get a boolean value.
	@param dict		Dictionnary to look in.
	@param id		Identifier.
	@return			Boolean value. *)
let do_bool dict id =

	let lookup i f =
		try
			match List.assoc i dict with
			| BOOL f -> f ()
			| GEN_BOOL f -> f id
			| _ -> f ()
		with Not_found ->
			f () in

	lookup id
		(fun _ -> lookup fallback_bool
			(fun _ -> false))


(** Test if a definition is provided in the dictionary.
	@param dict		Dictionnary to look in.
	@param id		Identifier.
	@return			Boolean value. *)
let do_ifdef dict id =
	if List.mem_assoc id dict then true else
	try
		match List.assoc fallback_ifdef dict with
		| GEN_BOOL f -> f id
		| _ -> false
	with Not_found -> false
}

let blank = [' ' '\t']
let id = [^ ' ' '\t' ')']+

rule scanner out dict state = parse
  "$$"
  	{ output_char out '$'; scanner out dict state lexbuf }

|  "$(foreach" blank (id as id) ")" ('\n'? as nl)
  	{
		let buf = Buffer.contents (scan_end (Buffer.create 1024) 0 lexbuf) in
		let f = do_coll dict id in
		if nl <> "" then incr line;
		f (fun dict -> scanner out dict FOREACH (Lexing.from_string buf)) dict;
		scanner out dict state lexbuf
	}

| "$(with" blank (id as id) ')'  ('\n'? as nl)
	{
		let buf = Buffer.contents (scan_end (Buffer.create 1024) 0 lexbuf) in
		let f = do_coll dict id in
		if nl <> "" then incr line;
		f (fun dict -> scanner out dict WITH (Lexing.from_string buf)) dict;
		scanner out dict state lexbuf
	}

| "$(end)" ('\n'? as nl)
	{
		if nl <> "" then incr line;		
		if state = TOP then error "extraneous $(end) tag"
	}

| "$(else)" ('\n'? as nl)
	{
		if nl <> "" then incr line;
		if state = THEN then skip out dict 0 lexbuf
		else failwith "'else' out of 'if'"
	}

| "$(if" blank '!' (id as id) ')' ('\n'? as nl)
	{
		let cond = do_bool dict id in
		if nl <> "" then incr line;
		if not cond then scanner out dict THEN lexbuf
		else skip out dict 0 lexbuf;
		scanner out dict state lexbuf
	}

| "$(if" blank (id as id) ')' ('\n'? as nl)
	{
		let cond = do_bool dict id in
		if nl <> "" then incr line;
		if cond then scanner out dict THEN lexbuf
		else skip out dict 0 lexbuf;
		scanner out dict state lexbuf
	}

| "$(ifdef" blank (id as id) ')' ('\n'? as nl)
	{
		let cond = do_ifdef dict id in
		if nl <> "" then incr line;
		if cond then scanner out dict THEN lexbuf
		else skip out dict 0 lexbuf;
		scanner out dict state lexbuf
	}

| "$(ifndef" blank (id as id) ')' ('\n'? as nl)
	{
		let cond = do_ifdef dict id in
		if nl <> "" then incr line;
		if not cond then scanner out dict THEN lexbuf
		else skip out dict 0 lexbuf;
		scanner out dict state lexbuf
	}

| "$(" ([^ ')']+ as id) ")"
	{ do_text out dict id; scanner out dict state lexbuf }

| "//$"
	{ comment out dict state lexbuf }

| '\n'
	{ incr line; output_char out '\n'; scanner out dict state lexbuf }
| _ as c
	{ output_char out c; scanner out dict state lexbuf }

| eof
	{ () }

and comment out dict state = parse
	"\n"
		{ incr line; scanner out dict state lexbuf }
|	_
		{ comment out dict state lexbuf }

and skip out dict cnt = parse
  "$$"
  	{ skip out dict cnt lexbuf }
| "$(foreach" blank?
  	{ skip out dict (cnt + 1) lexbuf }
| "$(if" blank?
	{ skip out dict (cnt + 1) lexbuf }
| "$(end)" '\n'?
	{ if cnt = 0 then () else skip out dict (cnt -1) lexbuf }
| "$(else)" '\n'?
	{	if cnt = 0 then scanner out dict ELSE lexbuf
		else skip out dict cnt lexbuf }
| "\n"
	{ incr line; skip out dict cnt lexbuf }
| _
	{ skip out dict cnt lexbuf }
| eof
	{ failwith "unclosed if" }


and scan_end buf cnt = parse
  "$$" as s
  	{ Buffer.add_string buf s; scan_end buf cnt lexbuf }
| "$(foreach" blank as s
  	{ Buffer.add_string buf s; scan_end buf (cnt + 1) lexbuf }
| "$(if" blank as s
	{ Buffer.add_string buf s; scan_end buf (cnt + 1) lexbuf }
| "$(end)" as s
	{ if cnt = 0 then buf
	else (Buffer.add_string buf s; scan_end buf (cnt - 1) lexbuf) }
| '\n'
	{ incr line; Buffer.add_char buf '\n'; scan_end buf cnt lexbuf }
| _ as c
	{ Buffer.add_char buf c; scan_end buf cnt lexbuf }
| eof
	{ failwith "unclosed foreach" }


{
(** Perform a template generation.
	@param dict		Dictionnary to use.
	@param in_path	Input template path.
	@param out_path	Path of the output file. *)
let generate_path dict in_path out_path =
	let output = open_out out_path in
	let input = open_in in_path in
	file := in_path;
	line := 1;	
	scanner output dict TOP (Lexing.from_channel input);
	close_in input;
	close_out output;
	file := "";
	line := 0

(** Perform a template generation.
	@param dict		Dictionnary to use.
	@param template	Template name (take from SOURCE_DIRECTORY/templates)
	@param out_path	Path of the output file. *)
let generate dict template out_path =
	generate_path dict (Irg.native_path (Config.source_dir ^ "/templates/" ^ template)) out_path
}

