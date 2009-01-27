(*
 * $Id: gep.ml,v 1.18 2009/01/27 21:56:54 casse Exp $
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


(* Universal environment building *)
let get_module f dict (name, _) =
	f (
		("name", App.out (fun _ -> name)) ::
		("NAME", App.out (fun _ -> String.uppercase name)) ::
		("is_mem", Templater.BOOL (fun _ -> name = "mem")) ::
		dict
	)

let make_env info =

	("modules", Templater.COLL (fun f dict -> List.iter (get_module f dict) !modules)) ::
	(* declarations of fetch tables *)
	("INIT_FETCH_TABLES_32", Templater.TEXT(fun out -> Fetch.output_all_table_C_decl out 32)) ::
	("target_bitorder", Templater.TEXT(fun out -> Fetch.output_bit_order out)) ::
	(App.make_env info)


(** Perform a symbolic link.
	@param src	Source file to link.
	@param dst	Destination to link to. *)
let link src dst =
	if Sys.file_exists dst then Sys.remove dst;
	Unix.symlink src dst


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
	App.process !nmp
		(fun info ->
			let dict = make_env info in
			
			add_module "fetch:fetch32";
			
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
			(* fetch (determining the ID of a given instruction) *)
			(*make_template "fetch.c" "src/fetch.c" dict;
			make_template "fetch.h" "src/fetch.h" dict;*)
			make_template "api.c" "src/api.c" dict;
			make_template "platform.h" "src/platform.h" dict;
			make_template "fetch_table32.h" "src/fetch_table.h" dict;
			make_template "decode_table32.h" "src/decode_table.h" dict;
			
			(* module linkig *)
			process_module info "gliss" "gliss";
			List.iter (fun (id, impl) -> process_module info impl id) !modules;
			Unix.rename (info.Toc.spath ^ "/mem.h") (info.Toc.ipath ^ "/mem.h");
			
			(* decode test *)
			(*Iter.iter (fun accu sp -> begin print_string ("mask_params "^(Iter.get_name sp)^"\n");
					List.iter (fun x -> Printf.printf "%s\n" (Decode.get_string_mask_for_param_from_op sp x)) [0; 1; 2; 3; 4; 5; 6] end) ()*)
		)
