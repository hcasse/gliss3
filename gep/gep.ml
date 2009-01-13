(*
 * $Id: gep.ml,v 1.12 2009/01/13 14:37:19 casse Exp $
 * Copyright (c) 2008, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of OGliss.
 *
 * OGliss is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * OGliss is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OGliss; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)
open Lexing

exception UnsupportedMemory of Irg.spec

(*module OrderedString = struct
	type t = string
	let compare s1 s2 = String.compare s1 s2
end
module StringSet = Set.Make(OrderedString)*)

module OrderedType = struct
	type t = Toc.c_type
	let compare s1 s2 = if s1 = s2 then 0 else if s1 < s2 then (-1) else 1
end
module TypeSet = Set.Make(OrderedType)

(* module management *)
let modules = ref [("mem", "fast_mem")]
let add_module text =
	let (new_id, new_imp) =
		try
			let idx = String.index text ':' in
			(String.sub text 0 idx,
			String.sub text (idx + 1) ((String.length text) - idx - 1))
		with Not_found ->
			(text, text) in
	let rec set lst =
		match lst with
		  (id, imp)::tl ->
			if id = new_id then (id, new_imp)::tl
			else (id, imp)::(set tl)
		| _ -> [(new_id, new_imp)] in
	modules := set !modules


(* options *)
let nmp: string ref = ref ""
let paths = [
	Config.install_dir ^ "/lib/gliss/lib";
	Config.source_dir ^ "/lib";
	Sys.getcwd ()]
let quiet = ref false
let verbose = ref false
let memory = ref "fast_mem"
let options = [
	("-v", Arg.Set verbose, "verbose mode");
	("-q", Arg.Set quiet, "quiet mode");
	("-m", Arg.String add_module, "add a module (module_name:actual_module)]")
]

let free_arg arg =
	if !nmp = ""
	then nmp := arg
	else raise (Arg.Bad "only one NML file required") 
let usage_msg = "SYNTAX: gep [options] NML_FILE\n\tGenerate code for a simulator"
let _ =
	Arg.parse options free_arg usage_msg;
	if !nmp = "" then begin
		prerr_string "ERROR: one NML file must be given !\n";
		Arg.usage options usage_msg;
		exit 1
	end


(** Build the given directory.
	@param path			Path of the directory.
	@raise Sys_error	If there is an error. *)
let makedir path =
	if not (Sys.file_exists path) then
		try 
			Unix.mkdir path 0o740
		with Unix.Unix_error (code, _, _) ->
			raise (Sys_error (Printf.sprintf "cannot create \"%s\": %s" path (Unix.error_message code)))
	else
		let stats = Unix.stat path in
		if stats.Unix.st_kind <> Unix.S_DIR
		then raise (Sys_error (Printf.sprintf "cannot create directory \"%s\": file in the middle" path))


(* Test if memory or register attributes contains ALIAS.
	@param attrs	Attributes to test.
	@return			True if it contains "alias", false else. *)
let rec contains_alias attrs =
	match attrs with
	  [] -> false
	| (Irg.ALIAS _)::_ -> true
	| _::tl -> contains_alias tl


(** Format date (in seconds) and return a stirng.
	@param date	Date to format.
	@return		Date formatted as a string. *)
let format_date date =
	let tm = Unix.localtime date in
	Printf.sprintf "%0d/%02d/%02d %02d:%02d:%02d"
		tm.Unix.tm_year tm.Unix.tm_mon tm.Unix.tm_mday
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec


(* Universal environment building *)
let out f = Templater.TEXT (fun out -> output_string out (f ()))

let get_params inst f dict =

	let get_type t =
		match t with
		  Irg.TYPE_EXPR t -> t
		| Irg.TYPE_ID n ->
			(match (Irg.get_symbol n) with
			  Irg.TYPE (_, t) -> t
			| _ -> Irg.NO_TYPE) in

	ignore (List.fold_left
		(fun (i: int) (n, t) ->
			let t = get_type t in
			(if t <> Irg.NO_TYPE then
				f (
					("PARAM", out (fun _ -> n)) ::
					("INDEX", out (fun _ -> string_of_int i)) ::
					("TYPE", out (fun _ -> Toc.type_to_string (Toc.convert_type t))) ::
					dict));
			i + 1)
		0
		(Iter.get_params inst))

let get_instruction f dict _ i = f
	(("IDENT", out (fun _ -> Iter.get_name i)) ::
	("ICODE", Templater.TEXT (fun out -> Printf.fprintf out "%d" (Iter.get_id i))) ::
	("params", Templater.COLL (get_params i)) ::
	dict)

let get_register f dict _ sym =
	match sym with
	  Irg.REG (name, size, t, attrs) -> f (
	  	("type", out (fun _ -> Toc.type_to_string (Toc.convert_type t))) ::
		("name", out (fun _ -> name)) ::
		("NAME", out (fun _ -> String.uppercase name)) ::
		("aliased", Templater.BOOL (fun _ -> contains_alias attrs)) ::
		("array", Templater.BOOL (fun _ -> size > 1)) ::
		("size", out (fun _ -> string_of_int size)) ::
		dict)	(* make_array size*)
	| _ -> ()

let get_value f dict t =
	f (
		("name", out (fun _ -> Toc.type_to_field t)) ::
		("type", out (fun _ -> Toc.type_to_string t)) ::
		dict
	)

let get_param f dict t =
	f (
		("NAME", out (fun _ -> String.uppercase (Toc.type_to_field t))) ::
		dict
	)

let get_memory f dict key sym = 
	match sym with
	  Irg.MEM (name, size, Irg.CARD(8), attrs) ->
	  	f (
			("NAME", out (fun _ -> String.uppercase name)) ::
			("aliased", Templater.BOOL (fun _ -> contains_alias attrs)) ::
			dict
		)
	| _ -> ()
	

let get_module f dict (name, _) =
	f (
		("name", out (fun _ -> name)) ::
		("NAME", out (fun _ -> String.uppercase name)) ::
		dict
	)

let make_env info =

	let param_types =
		let collect_field set (name, t) =
			match t with
			  Irg.TYPE_EXPR t -> TypeSet.add (Toc.convert_type t) set
			| Irg.TYPE_ID n ->
				(match (Irg.get_symbol n) with
				  Irg.TYPE (_, t) -> TypeSet.add (Toc.convert_type t) set
				| _ -> set) in
	
		let collect_fields set params =
			List.fold_left collect_field set params in
		Iter.iter (fun set i -> collect_fields set (Iter.get_params i)) TypeSet.empty in

	("instructions", Templater.COLL (fun f dict -> Iter.iter (get_instruction f dict) ())) ::
	("registers", Templater.COLL (fun f dict -> Irg.StringHashtbl.iter (get_register f dict ) Irg.syms)) ::
	("values", Templater.COLL (fun f dict -> TypeSet.iter (get_value f dict) param_types)) ::
	("params", Templater.COLL (fun f dict -> TypeSet.iter (get_param f dict) param_types)) ::
	("memories", Templater.COLL (fun f dict -> Irg.StringHashtbl.iter (get_memory f dict) Irg.syms)) ::
	("modules", Templater.COLL (fun f dict -> List.iter (get_module f dict) !modules)) ::
	("date", out (fun _ -> format_date (Unix.time ()))) ::
	("proc", out (fun _ -> info.Toc.proc)) ::
	("PROC", out (fun _ -> String.uppercase info.Toc.proc)) ::
	("version", out (fun _ -> "GLISS V2.0 Copyright (c) 2009 IRIT - UPS")) ::
	[]


(** Perform a symbolic link.
	@param src	Source file to link.
	@param dst	Destination to link to. *)
let link src dst =
	if Sys.file_exists dst then Sys.remove dst;
	Unix.symlink src dst


(* regular expressions *)
let lower_re = Str.regexp "gliss_"
let upper_re = Str.regexp "GLISS_"

(** Replace the "gliss" and "GLISS" words in the input file
	to create the output file.
	@param info		Generation information.
	@param in_file	Input file.
	@param out_file	Output file. *)
let replace_gliss info in_file out_file =
	let in_stream = open_in in_file in
	let out_stream = open_out out_file in
	let lower = info.Toc.proc ^ "_" in 
	let upper = String.uppercase lower in
	let rec trans _ =
		let line = input_line in_stream in
		output_string out_stream
			(Str.global_replace upper_re upper (Str.global_replace lower_re lower line));
		output_char out_stream '\n';
		trans () in
	try
		trans ()
	with End_of_file ->
		close_in in_stream;
		close_out out_stream


(** Link a module for building.
	@param info	Generation information.
	@param m	Original module name.
	@param name	Final name of the module. *)
let process_module info m name =

	(* find the module *)
	let rec find paths =
		if paths = [] then raise (Sys_error ("cannot find module " ^ m)) else
		let path = (List.hd paths) ^ "/" ^ m in
		if Sys.file_exists (path ^ ".c") then path
		else find (List.tl paths) in
	let path = find paths in
	
	(* link it *)
	let source = info.Toc.spath ^ "/" ^ name ^ ".c" in
	let header = info.Toc.spath ^ "/" ^ name ^ ".h" in
	if not !quiet then Printf.printf "creating \"%s\"\n" source;
	replace_gliss info (path ^ ".c") source;
	if not !quiet then Printf.printf "creating \"%s\"\n" header;
	replace_gliss info (path ^ ".h") header


let make_template template file dict =
	if not !quiet then Printf.printf "creating \"%s\"\n" file;
	Templater.generate dict template file
	

(* main program *)
let _ =
	try	
		begin
		
			(* parsing NMP *)
			let lexbuf = Lexing.from_channel (open_in !nmp) in
			Parser.top Lexer.main lexbuf;
			let info = Toc.info () in
			let dict = make_env info in
			
			(* include generation *)
			if not !quiet then Printf.printf "creating \"include/\"\n";
			makedir "include";
			if not !quiet then Printf.printf "creating \"%s\"\n" info.Toc.ipath;
			makedir info.Toc.ipath;
			make_template "id.h" ("include/" ^ info.Toc.proc ^ "/id.h") dict;
			make_template "api.h" ("include/" ^ info.Toc.proc ^ "/api.h") dict;
			make_template "macros.h" ("include/" ^ info.Toc.proc ^ "/macros.h") dict;
			
			(* source generation *)
			if not !quiet then Printf.printf "creating \"include/\"\n";
			makedir "src";
			link
				((Unix.getcwd ()) ^ "/" ^ info.Toc.ipath)
				(info.Toc.spath ^ "/target");
			make_template "Makefile" "src/Makefile" dict;
			make_template "state.h" "src/state.h" dict;
			
			(* module linkig *)
			process_module info "gliss" "gliss";
			List.iter (fun (id, impl) -> process_module info impl id) !modules
		end

	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error"; exit 2
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr); exit 2
	| Sem.SemError msg ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg); exit 2
	| Irg.IrgError msg ->
		Lexer.display_error (Printf.sprintf "ERROR: %s" msg); exit 2
	| Sem.SemErrorWithFun (msg, fn) ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg);
		fn (); exit 2;
	| Toc.Error msg -> 
		Printf.fprintf stderr "ERROR: %s\n" msg;
		exit 4
	| Sys_error msg ->
		Printf.eprintf "ERROR: %s\n" msg; exit 1
	| Failure e ->
		Lexer.display_error e; exit 3
