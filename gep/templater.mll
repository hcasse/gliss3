{

(** Type of dictionnaries. *)
type dict_t = (string * value_t) list

(** Value of a dictionnary *)
and  value_t =
	  TEXT of (out_channel -> unit)										(** a simple text display *)
	| COLL of ((dict_t -> unit) -> dict_t -> unit)		(** collection *)
	| BOOL of (unit -> bool)											(** boolean value *)


(** Perform text evaluation.
	@param out	Out channel.
	@param dict	Used dictionnary.
	@param id	Text identifier. *)
let do_text out dict id =
	try
		(match List.assoc id dict with
		  TEXT f -> f out
		| _ -> failwith (id ^ " is not a text !"))
	with Not_found ->
		failwith (id ^ " is undefined !")


(** Get a collection.
	@param dict	Dictionnary to look in.
	@param id	Identifier.
	@return		Found collection function. *)
let do_coll dict id =
	try
		(match List.assoc id dict with
		  COLL f -> f
		| _ -> failwith (id ^ " is not a collection"))
	with Not_found ->
		failwith (id ^ " is undefined !")


(** Get a boolean value.
	@param dict		Dictionnary to look in.
	@param id		Identifier.
	@return			Boolean value. *)
let do_bool dict id =
	try
		(match List.assoc id dict with
		  BOOL f -> f ()
		| _ -> failwith (id ^ " is not a boolean"))
	with Not_found ->
		failwith (id ^ " is undefined !")
	

}

let blank = [' ' '\t']
let id = [^ ' ' '\t' ')']+

rule scanner out dict = parse
  "$$"
  	{ output_char out '$'; scanner out dict lexbuf }

|  "$(foreach" blank (id as id) ")" '\n'?
  	{
		let buf = Buffer.contents (scan_end (Buffer.create 1024) 0 lexbuf) in
		let f = do_coll dict id in
		f (fun dict -> scanner out dict (Lexing.from_string buf)) dict;
		scanner out dict lexbuf
	}

| "$(end)" '\n'?
	{ () }

| "$(else)" '\n'?
	{ skip out dict 0 lexbuf; scanner out dict lexbuf }

| "$(if" blank '!' (id as id) ')' '\n'?
	{ if not (do_bool dict id) then scanner out dict lexbuf else skip out dict 0 lexbuf }

| "$(if" blank (id as id) ')' '\n'?
	{ if do_bool dict id then scanner out dict lexbuf else skip out dict 0 lexbuf }

| "$(" ([^ ')']+ as id) ")"
	{ do_text out dict id; scanner out dict lexbuf }

| _ as c
	{ output_char out c; scanner out dict lexbuf } 

| eof
	{ () }

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
	{ if cnt = 0 then scanner out dict lexbuf else skip out dict cnt lexbuf }
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
| _ as c
	{ Buffer.add_char buf c; scan_end buf cnt lexbuf }
| eof
	{ failwith "unclosed foreach" }


{
let format_date date =
	let tm = Unix.localtime date in
	Printf.sprintf "%0d/%02d/%02d %02d:%02d:%02d"
		tm.Unix.tm_year tm.Unix.tm_mon tm.Unix.tm_mday
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let default info =
	[
		("date", TEXT (fun out -> output_string out (format_date (Unix.time ()))));
		("proc", TEXT (fun out -> output_string out info.Toc.proc));
		("PROC", TEXT (fun out -> output_string out (String.uppercase info.Toc.proc)));
		("version", TEXT (fun out -> output_string out "GLISS V2.0 Copyright (c) 2009 IRIT - UPS"))
	]

let generate dict template out_path =
	let output = open_out out_path in
	let input = open_in (Config.source_dir ^ "/templates/" ^ template) in
	scanner output dict (Lexing.from_channel input);
	close_in input;
	close_out output
}
