{

(** Templater allows to generate files from templates.
	This templates may contains special token that are replaced
	by values retrieved from a dictionnary.
	
	The following syntax is supported in templates:
	* $$ -- output only one dollar
	* //$ text \n -- discared from output
	* $(id) -- replaced by evaluation of id
	* $(with id) ... $(end) -- go on with id as new map
	* $(foreach id) ... $(end) -- apply content on each value of collection id
	* $(if id) ... $(else) ... $(end) -- display first content if id is true, second else.
	* $(if !id) ... $(else) ... $(end) -- display second content if id is true, first else.
	* $(ifdef id) ... $(else) ... $(end) -- display first content if id exists, second else.
	* $(ifndef id) ... $(else) ... $(end) -- display first content if id does not exist, second else.

	As input, it uses map that may contain symbols:
	* outputting text,
	* evaluating as a condition,
	* providing a collection.
	
	To achieve this, the scanner are parameterized with a context made of:
	(output stream, output path, current map, state, configuration list, map join function)

	The map simply returns a value in a function of an identifier.
	
	Configuration list can contain:
	* MAP_JOIN f -- function used to join 2 maps.
	* ON_UNDEF f -- function called an 
	*)

open List
open Printf

exception Error of string


(** Values in a map. *)
type value =
	| NOVAL								(** Undefined value. *)
	| TEXT of (out_channel -> unit)		(** Generate a text on the given out channel. *)
	| MAP of map						(** New map. *)
	| COND of (unit -> bool)			(** Condition. *)
	| COLL of ((map -> unit) -> unit)	(* Iteration on a collection of items. *)

(** Map of values used by the generator. *)
and map = string -> value


(** Empty map. *)
let empty_map = fun _ -> NOVAL


(** Internal states of the templater. *)
type state =
	| TOP
	| THEN
	| ELSE
	| FOREACH
	| WITH


(** Configuration items. *)
type config =
	| MAP_JOIN of (map -> map -> map)
	| ON_UNDEF of (string -> unit)
	| ON_BAD_TYPE of (string -> string -> unit)


(** Context of analysis. *)
and context = out_channel * string * map * state * config list


(** Hierarchycal joint: m1 is looked first, then m2 if not found.
	@param m1	First map.
	@param m2	Second map.
	@param i	Identifier to solve.
	@return		Corresponding value. *)
let join_hierarch m1 m2 i =
	let r = m1 i in
	if r <> NOVAL then r else m2 i


(** Map join function only recalling the last map.
	@param m1	First map.
	@param m2	Second map.
	@param i	Identifier to solve.
	@return		Corresponding value. *)	
let join_last m1 m2 i = m1 i


(** Default configuration list. *)
let default_config = [
		MAP_JOIN		join_hierarch;
		ON_UNDEF		(fun id -> raise (Error (sprintf "%s cannot be found!" id)));
		ON_BAD_TYPE		(fun id typ -> raise (Error (sprintf "bad symbol type %s: %s required!" id typ)));
	]


let out_of (o, _, _, _, _, _) = o
let file_of (_, f, _, _, _, _) = f
let map_of (_, _, m, _, _, _) = m
let state_of (_, _, _, s, _, _) = s
let config_of (_, _, _, _, c, _) = c
let join_of (_, _, _, _, _, j) = j


(** Change the context with given map and state. *)
let enter (o, f, m, _, c, j) map state = (o, f, j map m, state, c, j)


(** Change the state of the context. *)
let change (o, f, m, _, c, j) state = (o, f, m, state, c, j)


(** Called when a symbol with the bad type is found. *)
let on_bad_type id typ ctx =
	let rec look cs =
		match cs with
		| []					-> ()
		| (ON_BAD_TYPE f)::_	-> f id typ
		| _::cs					-> look cs in
	look (config_of ctx)


(** Called when an undefined symbol is found. *)
let on_undef id ctx =
	let rec look cs =
		match cs with
		| []					-> ()
		| (ON_UNDEF f):: _	-> f id
		| _::cs					-> look cs in
	look (config_of ctx)

}

let blank = [' ' '\t']
let id = [^ ' ' '\t' ')']+

(* main scanner *)
rule scanner ctx = parse
  "$$"
  	{ output_char (out_of ctx) '$'; scanner ctx lexbuf }

| "//$"
	{ comment ctx lexbuf }


| blank* "$(with" blank (id as id) ')' blank* '\n'?
	{
		let m = match (map_of ctx) id with
			| NOVAL		-> on_undef id ctx; (fun _ -> NOVAL)
			| MAP m		-> m
			| _			-> on_bad_type id "map" ctx; (fun _ -> NOVAL) in
		let buf = Buffer.contents (scan_end (Buffer.create 1024) 0 lexbuf) in
		scanner (enter ctx m WITH) (Lexing.from_string buf);
		scanner ctx lexbuf
	}

| blank* "$(foreach" blank (id as id) ")" blank* '\n'?
  	{
		let f = match (map_of ctx) id with
			| NOVAL		-> on_undef id ctx; (fun _ -> ())
			| COLL f	-> f
			| _			-> on_bad_type id "collection" ctx; (fun _ -> ()) in
		let buf = Buffer.contents (scan_end (Buffer.create 1024) 0 lexbuf) in
		f (fun m -> scanner (enter ctx m FOREACH) (Lexing.from_string buf));
		scanner ctx lexbuf
	}

| blank* "$(end)" blank* '\n'?
	{ if (state_of ctx) = TOP then raise (Error ("extraneous $(end) tag")) }

| blank* "$(else)" blank* '\n'?
	{
		if (state_of ctx) = THEN then skip ctx 0 lexbuf
		else raise (Error "'else' out of 'if'")
	}

| blank* "$(if" blank* (id as id) ')' blank* '\n'?
	{
		let c = match (map_of ctx) id with
			| NOVAL		-> on_undef id ctx; false
			| COND f	-> f()
			| _			-> on_bad_type id "collection" ctx; false in
		if c then scanner (change ctx THEN) lexbuf
		else skip ctx 0 lexbuf;
		scanner ctx lexbuf
	}

| "$(if" blank* '!' (id as id) ')' blank* '\n'?
	{
		let c = match (map_of ctx) id with
			| NOVAL		-> on_undef id ctx; false
			| COND f	-> f()
			| _			-> on_bad_type id "collection" ctx; false in
		if not c then scanner (change ctx THEN) lexbuf
		else skip ctx 0 lexbuf;
		scanner ctx lexbuf
	}

| blank* "$(ifdef" blank* (id as id) ')' '\n'?
	{
		let c = ((map_of ctx) id) <> NOVAL in
		if c then scanner (change ctx THEN) lexbuf
		else skip ctx 0 lexbuf;
		scanner ctx lexbuf
	}

| blank* "$(ifndef" blank* (id as id) ')' '\n'?
	{
		let c = ((map_of ctx) id) = NOVAL in
		if c then scanner (change ctx THEN) lexbuf
		else skip ctx 0 lexbuf;
		scanner ctx lexbuf
	}

| "$(" ([^ ')']+ as id) ")"
	{
		match (map_of ctx) id with
		| NOVAL		-> on_undef id ctx
		| TEXT f	-> f (out_of ctx)
		| _ 		-> on_bad_type id "text" ctx
	}

| _ as c
	{ output_char (out_of ctx) c; scanner ctx lexbuf }

| eof
	{ () }


(* comment-out end-of-line scanner *)
and comment ctx = parse
	"\n"
		{ scanner ctx lexbuf }
|	_
		{ comment ctx lexbuf }


(* scanner until $(end) tag *)
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
| _ as c
	{ Buffer.add_char buf c; scan_end buf cnt lexbuf }
| eof
	{ failwith "unclosed foreach" }


(* skip structure keywords to $(else) or $(end). *)
and skip ctx cnt = parse
  "$$"
  	{ skip ctx cnt lexbuf }
| "$(foreach" blank?
  	{ skip ctx (cnt + 1) lexbuf }
| "$(if" blank?
	{ skip ctx (cnt + 1) lexbuf }
| "$(end)" blank* '\n'?
	{ if cnt = 0 then () else skip ctx (cnt -1) lexbuf }
| "$(else)" blank* '\n'?
	{	if cnt = 0 then scanner (change ctx ELSE) lexbuf
		else skip ctx cnt lexbuf }
| _
	{ skip ctx cnt lexbuf }
| eof
	{ failwith "unclosed if" }


{

(** Perform a template generation using the given streams.
	@param input	Input stream.
	@param output	Output streaml.
	@param ctx		Context to use.
	@raise Error	If there is an error in the script. *)
let make_stream input output ctx =
	let lexbuf = Lexing.from_channel input in
	try
		scanner (change ctx TOP) lexbuf
	with Error s ->
		let path = file_of ctx in
		if path = "" then
			raise (Error (sprintf "%d: %s" lexbuf.Lexing.lex_start_pos s))
		else
			let f = open_in path in
			let rec look n p =
				if p >= lexbuf.Lexing.lex_start_pos then n else
				look (n + 1) (String.length (input_line f)) in
			let l = look 0 0 in
			close_in f;
			raise (Error (sprintf "%s:%d: %s" path l s))


(** Perform a template generation using the given paths.
	@param map		Map to use.
	@param in_path	Template path.
	@param out_path	Output path.
	@param config	Configuration to use.
	@raise Error	If there is an error in the script. *)
let make map in_path out_path config =
	let input = open_in in_path in
	let output = open_out out_path in
	let rec get_join cs =
		match cs with
		| []				-> join_hierarch
		| (MAP_JOIN f)::_	-> f
		| _::cs				-> get_join cs in
	make_stream input output (output, out_path, map, TOP, config, get_join config)
}
