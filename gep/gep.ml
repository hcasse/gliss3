(*
 * $Id: gep.ml,v 1.35 2009/04/22 14:21:18 barre Exp $
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


(** module structure *)
type gmod = {
	mutable iname: string;		(** interface name *)
	mutable aname: string;		(** actual name *)
	mutable path: string;		(** path to the module *)
	mutable libadd: string;		(** linkage options *)
}


(** Build a new module.
	@param _iname	Interface name.
	@param _aname	Actual name. *)
let new_mod _iname _aname = {
		iname = _iname;
		aname = _aname;
		path = "";
		libadd = "";
	}

(** list of modules *)
let modules = ref [
	new_mod "mem" "fast_mem";
	new_mod "grt" "grt";
	new_mod "error" "error"
]


(** Add a module to the list of module from arguments.
	@param text		Text of the ragument. *)
let add_module text =
	let new_mod =
		try
			let idx = String.index text ':' in
			new_mod
				(String.sub text 0 idx)
				(String.sub text (idx + 1) ((String.length text) - idx - 1))
		with Not_found ->
			new_mod text text in
	let rec set lst =
		match lst with
		  m::tl ->
			if m.iname = new_mod.iname then new_mod::tl
			else m::(set tl)
		| _ -> [new_mod] in
	modules := set !modules


(* options *)
let paths = [
	Config.install_dir ^ "/lib/gliss/lib";
	Config.source_dir ^ "/lib";
	Sys.getcwd ()]
let sim = ref false
let memory = ref "fast_mem"
let size = ref 0
let sources : string list ref = ref []
let options = [
	("-m", Arg.String add_module, "add a module (module_name:actual_module)]");
	("-s", Arg.Set_int size, "for fixed-size ISA, size of the instructions in bits (to control NMP images)");
	("-a", Arg.String (fun a -> sources := a::!sources), "add a source file to the library compilation");
	("-S", Arg.Set sim, "generate the simulator application")
]


(** Build an environment for a module.
	@param f	Function to apply to the environment.
	@param dict	Embedding environment.
	@param m	Module to process. *)
let get_module f dict m =
	f (
		("name", App.out (fun _ -> m.iname)) ::
		("NAME", App.out (fun _ -> String.uppercase m.iname)) ::
		("is_mem", Templater.BOOL (fun _ -> m.iname = "mem")) ::
		("PATH", App.out (fun _ -> m.path)) ::
		("LIBADD", App.out (fun _ -> m.libadd)) ::
		dict
	)

let get_source f dict source =
	f (("path", App.out (fun _ -> source)) :: dict)


(** Build a template environment.
	@param info		Information for generation.
	@return			Default template environement. *)
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
	("gen_pc_incr", Templater.TEXT (fun out -> 
			let info = Toc.info () in
			info.Toc.out <- out;
			Toc.gen_stat info (Toc.gen_pc_increment info))) ::
	("gen_init_code", Templater.TEXT (fun out -> 
			let info = Toc.info () in
			info.Toc.out <- out;
			Toc.gen_stat info (Toc.get_init_code () ))) ::
	("NPC_NAME", Templater.TEXT (fun out -> output_string out  (String.uppercase info.Toc.npc_name))) ::
	("PC_NAME", Templater.TEXT (fun out -> output_string out  (String.uppercase info.Toc.pc_name))) ::
	("PPC_NAME", Templater.TEXT (fun out -> output_string out  (String.uppercase info.Toc.ppc_name))) ::
	(App.make_env info maker)


(** Perform a symbolic link.
	@param src	Source file to link.
	@param dst	Destination to link to. *)
let link src dst =
	if Sys.file_exists dst then Sys.remove dst;
	Unix.symlink src dst


(** Regular expression for LIBADD *)
let libadd_re = Str.regexp "^LIBADD=\\(.*\\)"


(** Find a module and set the path.
	@param m	Module to find. *)
let find_mod m =

	let rec find_lib paths = 
		match paths with
		| [] ->  raise (Sys_error ("cannot find module " ^ m.aname))
		| path::tail ->
			let source_path = path ^ "/" ^ m.aname ^ ".c" in
			if Sys.file_exists source_path then m.path <- path
		else find_lib tail in

	let rec read_lines input =
		let line = input_line input in
		if Str.string_match libadd_re line 0 then
			m.libadd <- Str.matched_group 1 line;
		read_lines input in

	find_lib paths;
	let info_path = m.path ^ "/" ^ m.aname ^ ".info" in
	if Sys.file_exists info_path then
		try
			read_lines (open_in info_path)
		with End_of_file ->
			()


(** Link a module for building.
	@param info	Generation information.
	@param m	Module to process. *)
let process_module info m =
	let source = info.Toc.spath ^ "/" ^ m.iname ^ ".c" in
	let header = info.Toc.spath ^ "/" ^ m.iname ^ ".h" in
	if not !App.quiet then Printf.printf "creating \"%s\"\n" source;
	App.replace_gliss info (m.path ^ "/" ^ m.aname ^ ".c") source;
	if not !App.quiet then Printf.printf "creating \"%s\"\n" header;
	App.replace_gliss info (m.path ^ "/" ^ m.aname ^ ".h") header


let make_template template file dict =
	if not !App.quiet then (Printf.printf "creating \"%s\"\n" file; flush stdout);
	Templater.generate dict template file

(* main program *)
let _ =
	App.run
		options
		"SYNTAX: gep [options] NML_FILE\n\tGenerate code for a simulator"
		(fun info ->
			let dict = make_env info in

			Printf.printf "PC=%s, NPC=%s, PPC=%s\n" info.Toc.pc_name info.Toc.npc_name info.Toc.ppc_name;	(* !!DEBUG!! *)
			(* include generation *)

			List.iter find_mod !modules;
			
			if not !App.quiet then Printf.printf "creating \"include/\"\n";
			App.makedir "include";
			if not !App.quiet then Printf.printf "creating \"%s\"\n" info.Toc.hpath;
			App.makedir info.Toc.hpath;
			make_template "id.h" ("include/" ^ info.Toc.proc ^ "/id.h") dict;
			make_template "api.h" ("include/" ^ info.Toc.proc ^ "/api.h") dict;
			make_template "macros.h" ("include/" ^ info.Toc.proc ^ "/macros.h") dict;
			
			(* source generation *)

			if not !App.quiet then Printf.printf "creating \"include/\"\n";
			App.makedir "src";

			link
				(info.Toc.hpath)
				(info.Toc.spath ^ "/target");
			make_template "Makefile" "src/Makefile" dict;
			make_template "gliss-config" ("src/" ^ info.Toc.proc ^ "-config") dict;
			make_template "api.c" "src/api.c" dict;
			make_template "platform.h" "src/platform.h" dict;
			make_template "fetch_table32.h" "src/fetch_table.h" dict;
			make_template "decode_table32.h" "src/decode_table.h" dict;
			make_template "inst_size_table.h" "src/inst_size_table.h" dict;
			make_template "code_table.h" "src/code_table.h" dict;
			
			(* module linking *)
			List.iter (process_module info) !modules;
			Unix.rename (info.Toc.spath ^ "/mem.h") (info.Toc.hpath ^ "/mem.h");
			
			(* generate application *)
			if !sim then
				try
					let path = App.find_lib "sim/sim.c" paths in
					App.makedir "sim";
					App.replace_gliss info
						(path ^ "/" ^ "sim/sim.c")
						("sim/" ^ info.Toc.proc ^ "-sim.c" );
					Templater.generate_path
						[ ("proc", Templater.TEXT (fun out -> output_string out info.Toc.proc)) ]
						(path ^ "/sim/Makefile")
						"sim/Makefile"
				with Not_found ->
					raise (Sys_error "no template to make sim program")
		)
