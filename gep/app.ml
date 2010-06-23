(*
 * $Id: app.ml,v 1.16 2009/11/26 09:01:16 casse Exp $
 * Copyright (c) 2009, IRIT - UPS <casse@irit.fr>
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

(*
  Usefull list of dependencies in order to work with the interactive Ocaml toplevel :
  (Do not forget to do make to have the latest version of the cmo binnairies)
*)
(*
  #directory "../irg";;
  #directory "../gep";;
  
  #load "unix.cma";;
  #load "str.cma";;
  #load "config.cmo";;
  #load "irg.cmo";;
  #load "instantiate.cmo";;
  #load "lexer.cmo";;
  #load "sem.cmo";;
  #load "IdMaker.cmo";;
  #load "iter.cmo";;
  #load "toc.cmo";;
  #load "parser.cmo";;
  #load "irgUtil.cmo";;
  #load "templater.cmo";;
*)


module OrderedType = 
struct
	type t = Toc.c_type
	let compare s1 s2 = if s1 = s2 then 0 else if s1 < s2 then (-1) else 1
end

module TypeSet = Set.Make(OrderedType);;

(** Gather information useful for the generation. *)
type maker_t = {
	mutable get_params: Iter.inst -> int -> string -> Irg.type_expr -> Templater.dict_t -> Templater.dict_t;
	mutable get_instruction: Iter.inst -> Templater.dict_t -> Templater.dict_t
}



(** Build the given directory.
	@param path			Path of the directory.
	@raise Sys_error	If there is an error. *)
let rec makedir path =
	if not (Sys.file_exists path) then
		try
			(try
				let p = String.rindex path '/' in
				makedir (String.sub path 0 p)
			with Not_found -> ());
			Printf.printf "creating \"%s\"\n" path;
			Unix.mkdir path 0o740
		with Unix.Unix_error (code, _, _) ->
			raise (Sys_error (Printf.sprintf "cannot create \"%s\": %s" path (Unix.error_message code)))
	else
		let stats = Unix.stat path in
		if stats.Unix.st_kind <> Unix.S_DIR
		then raise (Sys_error (Printf.sprintf "cannot create directory \"%s\": file in the middle" path))



(* regular expressions *)
let lower_re = Str.regexp "gliss_"
let upper_re = Str.regexp "GLISS_"
let path_re = Str.regexp "gliss/"

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
			(Str.global_replace path_re (info.Toc.proc ^ "/")
			(Str.global_replace upper_re upper
			(Str.global_replace lower_re lower line)));
		output_char out_stream '\n';
		trans () in
	try
		trans ()
	with End_of_file ->
		close_in in_stream;
		close_out out_stream


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


let out f = Templater.TEXT (fun out -> output_string out (f ()))

let get_params maker inst f dict =

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
				f (maker.get_params inst i n t (
					("PARAM", out (fun _ -> n)) ::
					("INDEX", out (fun _ -> string_of_int i)) ::
					("TYPE", out (fun _ -> Toc.type_to_string (Toc.convert_type t))) ::
					("PARAM_TYPE", out (fun _ -> String.uppercase (Toc.type_to_field (Toc.convert_type t)))) ::
					("param_type", out (fun _ -> Toc.type_to_field (Toc.convert_type t))) ::
					dict)));
			i + 1)
		0
		(Iter.get_params inst))

let get_instruction maker f dict _ i = f
	(maker.get_instruction  i
		(("IDENT", out (fun _ -> String.uppercase (Iter.get_name i))) ::
		("ident", out (fun _ -> Iter.get_name i)) ::
		("ICODE", Templater.TEXT (fun out -> Printf.fprintf out "%d" (Iter.get_id i))) ::
		("params", Templater.COLL (get_params maker i)) ::
		("has_param", Templater.BOOL (fun _ -> (List.length (Iter.get_params  i)) > 0)) ::
		("num_params", Templater.TEXT (fun out -> Printf.fprintf out "%d" (List.length (Iter.get_params i)))) ::
		dict))

let get_register f dict _ sym =
	match sym with
	  Irg.REG (name, size, t, attrs) -> f (
	  	("type", out (fun _ -> Toc.type_to_string (Toc.convert_type t))) ::
		("name", out (fun _ -> name)) ::
		("NAME", out (fun _ -> String.uppercase name)) ::
		("aliased", Templater.BOOL (fun _ -> contains_alias attrs)) ::
		("array", Templater.BOOL (fun _ -> size > 1)) ::
		("size", out (fun _ -> string_of_int size)) ::
		("printf_format", out (fun _ -> Toc.type_to_printf_format (Toc.convert_type t))) ::
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
			("name", out (fun _ -> name)) ::
			("aliased", Templater.BOOL (fun _ -> contains_alias attrs)) ::
			dict
		)
	| _ -> ()


let maker _ = {
	get_params = (fun _ _ _ _ dict -> dict);
	get_instruction = (fun _ dict -> dict)
}

(*
make_env : Toc.info_t -> maker_t -> (string * Templater.value_t) list
*)
let make_env info maker =

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

	("instructions", Templater.COLL (fun f dict -> Iter.iter (get_instruction maker f dict) ())) ::
	("registers", Templater.COLL (fun f dict -> Irg.StringHashtbl.iter (get_register f dict ) Irg.syms)) ::
	("values", Templater.COLL (fun f dict -> TypeSet.iter (get_value f dict) param_types)) ::
	("params", Templater.COLL (fun f dict -> TypeSet.iter (get_param f dict) param_types)) ::
	("memories", Templater.COLL (fun f dict -> Irg.StringHashtbl.iter (get_memory f dict) Irg.syms)) ::
	("date", out (fun _ -> format_date (Unix.time ()))) ::
	("proc", out (fun _ -> info.Toc.proc)) ::
	("PROC", out (fun _ -> String.uppercase info.Toc.proc)) ::
	("version", out (fun _ -> "GLISS V2.0 Copyright (c) 2009 IRIT - UPS")) ::
	("SOURCE_PATH", out (fun _ -> info.Toc.spath)) ::
	("INCLUDE_PATH", out (fun _ -> info.Toc.ipath)) ::
	[]


(* Activate a switch.
	@param name		Switch name.
	@param value	Value of the switch.
	@param dict		Dictionnary to activate in.
	@return			Result dictionnary. *)
let add_switch name value dict =
	(name, Templater.BOOL (fun _ -> value))::dict


(**
 * Load a NMP file and launch the given function on it
 * (and capture and display all exceptions).
 * @param file	File to process.
 * @param f		Function to work with definitions.
 *)
let process file f =
	try
		IrgUtil.load file;
		let info = Toc.info () in
		f info
	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error"; exit 2
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr); exit 2
	| Sem.SemError msg ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg); exit 2
	| Irg.IrgError msg ->
		Lexer.display_error (Printf.sprintf "ERROR: %s" msg); exit 2
	| Irg.RedefinedSymbol sym ->
		Lexer.display_error (Printf.sprintf "ERROR: redefined symbol \"%s\"" sym); exit 2
	| Sem.SemErrorWithFun (msg, fn) ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg);
		fn (); exit 2;
	| Toc.Error msg ->
		Printf.fprintf stderr "ERROR: %s\n" msg; exit 4
	| Toc.PreError f ->
		output_string stderr "ERROR: ";
		f stderr;
		output_char stderr '\n';
		exit 4
	| Toc.LocError (file, line, f) ->
		Printf.fprintf stderr "ERROR: %s:%d: " file line;
		f stderr;
		output_char stderr '\n';
		exit 1
	| Sys_error msg ->
		Printf.fprintf stderr "ERROR: %s\n" msg; exit 1
	| Unix.Unix_error (err, _, path) ->
		Printf.fprintf stderr "ERROR: %s on \"%s\"\n" (Unix.error_message err) path; exit 4
	(*| Failure e ->
		Lexer.display_error e; exit 3*)


(** Find a source from "lib/"
	@param source		Looked source.
	@param paths		List of paths to look in.
	@raise Not_found	If the source can not be found. *)
let rec find_lib source paths =
	match paths with
	| [] ->  raise Not_found
	| path::tail ->
		let source_path = path ^ "/" ^ source in
		if Sys.file_exists source_path then path
		else find_lib source tail


(* options *)
let nmp: string ref = ref ""
let quiet = ref false
let verbose = ref false
let options = [
	("-v", Arg.Set verbose, "verbose mode");
	("-q", Arg.Set quiet, "quiet mode");
]


(** Run a standard command using IRG. Capture and display all errors.
	@param f		Function to run once the IRG is load.
	@param args		Added arguments.
	@param help		Help text about the command. *)
let run args help f =
	let free_arg arg =
		if !nmp = ""
		then nmp := arg
		else Printf.fprintf stderr "WARNING: only one NML file required, %s ignored\n" arg in
	Arg.parse (options @ args) free_arg help;
	if !nmp = "" then
		begin
			prerr_string "ERROR: one NML file must be given !\n";
			Arg.usage options help;
			exit 1
		end
	else
		process !nmp f


(** Build a template, possibly informing the user.
	@param template		Template name.
	@param file			File path to output to.
	@param dict			Dictionary to use. *)
let make_template template file dict =
	if not !quiet then (Printf.printf "creating \"%s\"\n" file; flush stdout);
	Templater.generate dict template file
 
