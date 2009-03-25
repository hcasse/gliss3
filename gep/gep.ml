(*
 * $Id: gep.ml,v 1.28 2009/03/25 13:31:10 casse Exp $
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
let size = ref 0
let sources : string list ref = ref []
let options = [
	("-v", Arg.Set verbose, "verbose mode");
	("-q", Arg.Set quiet, "quiet mode");
	("-m", Arg.String add_module, "add a module (module_name:actual_module)]");
	("-s", Arg.Set_int size, "for fixed-size ISA, size of the instructions in bits (to control NMP images)");
	("-a", Arg.String (fun a -> sources := a::!sources), "add a source file to the library compilation")
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


(* Universal environment building *)
let get_module f dict (name, _) =
	f (
		("name", App.out (fun _ -> name)) ::
		("NAME", App.out (fun _ -> String.uppercase name)) ::
		("is_mem", Templater.BOOL (fun _ -> name = "mem")) ::
		dict
	)

let get_source f dict source =
	f (("path", App.out (fun _ -> source)) :: dict)

let make_env info =

	let min_size =
		Iter.iter (fun min inst ->
			let size = Fetch.get_instruction_length inst in
			if size < min then size else min)
			1024 in

	let add_mask_32_to_param inst idx _ _ dict =
		("mask_32", Templater.TEXT (fun out -> Printf.fprintf out "0X%08lX" (Fetch.str01_to_int32 (Decode.get_string_mask_for_param_from_op inst idx)))) ::
		dict in
	let add_size_to_inst inst dict =
		("size", Templater.TEXT (fun out -> Printf.fprintf out "%d" (Fetch.get_instruction_length inst))) ::
		("gen_code", Templater.TEXT (fun out -> 
			let info = Toc.info () in
			info.Toc.out <- out;
			Toc.set_inst info inst;
			Toc.gen_action info "action")) ::
		dict in

	let maker = App.maker() in
	maker.App.get_params <- add_mask_32_to_param;
	maker.App.get_instruction <- add_size_to_inst;

	("modules", Templater.COLL (fun f dict -> List.iter (get_module f dict) !modules)) ::
	("sources", Templater.COLL (fun f dict -> List.iter (get_source f dict) !sources)) ::
	(* declarations of fetch tables *)
	("INIT_FETCH_TABLES_32", Templater.TEXT(fun out -> Fetch.output_all_table_C_decl out 32)) ::
	("min_instruction_size", Templater.TEXT (fun out -> Printf.fprintf out "%d" min_size)) ::
	(App.make_env info maker)


(** Perform a symbolic link.
	@param src	Source file to link.
	@param dst	Destination to link to. *)
let link src dst =
	if Sys.file_exists dst then Sys.remove dst;
	Unix.symlink src dst


(** Link a module for building.
	@param info	Generation information.
	@param m	Original module name.
	@param name	Final name of the module. *)
let process_module info m name =
	try
		let source = info.Toc.spath ^ "/" ^ name ^ ".c" in
		let header = info.Toc.spath ^ "/" ^ name ^ ".h" in
		let path = App.find_lib (m ^ ".c") paths in
		if not !quiet then Printf.printf "creating \"%s\"\n" source;
		App.replace_gliss info (path ^ "/" ^ m ^ ".c") source;
		if not !quiet then Printf.printf "creating \"%s\"\n" header;
		App.replace_gliss info (path ^ "/" ^ m ^ ".h") header
	with Not_found ->
		raise (Sys_error ("cannot find module " ^ m))


let make_template template file dict =
	if not !quiet then (Printf.printf "creating \"%s\"\n" file; flush stdout);
	Templater.generate dict template file

(* main program *)
let _ =
	App.process !nmp
		(fun info ->
			let dict = make_env info in
			
			(* include generation *)

			
			if not !quiet then Printf.printf "creating \"include/\"\n";
			App.makedir "include";
			if not !quiet then Printf.printf "creating \"%s\"\n" info.Toc.ipath;
			App.makedir info.Toc.ipath;
			make_template "id.h" ("include/" ^ info.Toc.proc ^ "/id.h") dict;
			make_template "api.h" ("include/" ^ info.Toc.proc ^ "/api.h") dict;
			make_template "macros.h" ("include/" ^ info.Toc.proc ^ "/macros.h") dict;
			
			(* source generation *)

			if not !quiet then Printf.printf "creating \"include/\"\n";
			App.makedir "src";

			link
				((Unix.getcwd ()) ^ "/" ^ info.Toc.ipath)
				(info.Toc.spath ^ "/target");
			make_template "Makefile" "src/Makefile" dict;
			make_template "api.c" "src/api.c" dict;
			make_template "platform.h" "src/platform.h" dict;
			make_template "fetch_table32.h" "src/fetch_table.h" dict;
			make_template "decode_table32.h" "src/decode_table.h" dict;
			make_template "inst_size_table.h" "src/inst_size_table.h" dict;
			make_template "code_table.h" "src/code_table.h" dict;
			
			(* module linkig *)
			process_module info "gliss" "gliss";
			List.iter (fun (id, impl) -> process_module info impl id) !modules;
			Unix.rename (info.Toc.spath ^ "/mem.h") (info.Toc.ipath ^ "/mem.h");
			
			
			(*Iter.iter
			(fun a sp ->
				Printf.printf "#%d %s (%d bits):\n\t%s\n\t%s\n\t%s\n"
				(Iter.get_id sp)
				(Iter.get_name sp)
				(Fetch.get_instruction_length sp)
				(Fetch.get_string_mask_from_op sp)
				(Fetch.get_string_value_on_mask_from_op sp)
				(Fetch.get_value_on_mask sp))
			()*)
			

			(* toc test *)
			(*let iter_func accu sp =
				let action =
					match Iter.get_attr sp "action" with
					Iter.STAT(s) ->
						s
					| _ ->
						failwith ("no action stat attribute for the spec "^(Iter.get_name sp))
				in
				let info = Toc.info ()
				in		
				let params = Iter.get_params sp
				in
				begin
				print_string ("\naction of "^(Iter.get_name sp)^"\n");
				Irg.print_spec sp;
				print_string "prep+gen\n";
				
				Irg.param_stack params;
				let a1 = Toc.prepare_stat info action
				in
				Toc.declare_temps info;
				Toc.gen_stat info a1;
				Toc.cleanup_temps info;	
				Irg.param_unstack params;
				end
			in
			Iter.iter iter_func ()
	*)
	
	
		)
