(*
 * $Id: gep.ml,v 1.39 2009/11/26 09:01:16 casse Exp $
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
	mutable iname: string;				(** interface name *)
	mutable aname: string;				(** actual name *)
	mutable path: string;				(** path to the module *)
	mutable libadd: string;				(** linkage options *)
	mutable cflags: string;				(** library compilation option *)
	mutable code_header: string;		(** to put on code header *)
}


(** Build a new module.
	@param _iname	Interface name.
	@param _aname	Actual name. *)
let new_mod _iname _aname = {
		iname = _iname;
		aname = _aname;
		path = "";
		libadd = "";
		cflags = "";
		code_header = "";
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
		("CFLAGS", App.out (fun _ -> m.cflags)) ::
		("CODE_HEADER", App.out (fun _ -> m.code_header)) ::
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
	let invalid_category = "-1"
	in
	let get_category i =
		try
		(match Iter.get_attr i "category" with
		Iter.EXPR(e) ->
			(try
				let c = Sem.eval_const e
				in
				match c with
				Irg.STRING_CONST(s) ->
					s
				| _ ->
					failwith "the attribute named \"category\" is defined as a number in a string constant (like \"4\" or \"8\")"
			with Sem.SemError _ -> failwith "the attribute named \"category\" should be a constant")
		| Iter.STAT(s) ->
			failwith "the attribute named \"category\" mustn't be a statement but an expression.")
		with Not_found -> (* attribute "category" doesn't exist, return invalid category *)
			invalid_category
	in
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
		(* ppc category, will return the value of the category attribute in ppc description, could be extended to other procs *)
		("category", Templater.TEXT (fun out -> Printf.fprintf out "%s" (get_category inst) )) ::
		dict in

	let maker = App.maker() in
	maker.App.get_params <- add_mask_32_to_param;
	maker.App.get_instruction <- add_size_to_inst;

	("modules", Templater.COLL (fun f dict -> List.iter (get_module f dict) !modules)) ::
	("sources", Templater.COLL (fun f dict -> List.iter (get_source f dict) !sources)) ::
	(* declarations of fetch tables *)
	("INIT_FETCH_TABLES", Templater.TEXT(fun out -> Fetch.output_all_table_C_decl out)) ::
	("min_instruction_size", Templater.TEXT (fun out -> Printf.fprintf out "%d" min_size)) ::
	(* for category table for ppc, category is always a number >= 0, let's take -1 for an invalid category (eg. for instr unknown) *)
	("invalid_category", Templater.TEXT (fun out -> output_string out invalid_category)) ::
	("gen_pc_incr", Templater.TEXT (fun out ->
			let info = Toc.info () in
			info.Toc.out <- out;
			Toc.gen_stat info (Toc.gen_pc_increment info))) ::
	("gen_init_code", Templater.TEXT (fun out ->
			let info = Toc.info () in
			info.Toc.out <- out;
			Toc.gen_stat info (Toc.get_init_code () ))) ::
	("NPC_NAME", Templater.TEXT (fun out -> output_string out  ((*String.uppercase*) info.Toc.npc_name))) ::
	("PC_NAME", Templater.TEXT (fun out -> output_string out  ((*String.uppercase*) info.Toc.pc_name))) ::
	("PPC_NAME", Templater.TEXT (fun out -> output_string out  ((*String.uppercase*) info.Toc.ppc_name))) ::
	(App.make_env info maker)


(** Perform a symbolic link.
	@param src	Source file to link.
	@param dst	Destination to link to. *)
let link src dst =
	if Sys.file_exists dst then Sys.remove dst;
	Unix.symlink src dst


(** Regular expression for LIBADD *)
let libadd_re = Str.regexp "^LIBADD=\\(.*\\)"


(** regular expression for CFLAGS *)
let cflags_re = Str.regexp "^CFLAGS=\\(.*\\)"


(** regular expression for CODE_HEADER *)
let code_header_re = Str.regexp "^CODE_HEADER=\\(.*\\)"



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
			m.libadd <- Str.matched_group 1 line
		else if Str.string_match cflags_re line 0 then
			m.cflags <- Str.matched_group 1 line
		else if Str.string_match code_header_re line 0 then
			m.code_header <- m.code_header ^ (Str.matched_group 1 line) ^ "\n";
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


(* main program *)
let _ =
	App.run
		options
		"SYNTAX: gep [options] NML_FILE\n\tGenerate code for a simulator"
		(fun info ->
			let dict = make_env info in

			(* include generation *)

			List.iter find_mod !modules;

			if not !App.quiet then Printf.printf "creating \"include/\"\n";
			App.makedir "include";
			if not !App.quiet then Printf.printf "creating \"%s\"\n" info.Toc.hpath;
			App.makedir info.Toc.hpath;
			App.make_template "id.h" ("include/" ^ info.Toc.proc ^ "/id.h") dict;
			App.make_template "api.h" ("include/" ^ info.Toc.proc ^ "/api.h") dict;
			App.make_template "macros.h" ("include/" ^ info.Toc.proc ^ "/macros.h") dict;

			(* source generation *)

			if not !App.quiet then Printf.printf "creating \"include/\"\n";
			App.makedir "src";

			link
				(info.Toc.hpath)
				(info.Toc.spath ^ "/target");
			App.make_template "Makefile" "src/Makefile" dict;
			App.make_template "gliss-config" ("src/" ^ info.Toc.proc ^ "-config") dict;
			App.make_template "api.c" "src/api.c" dict;
			App.make_template "platform.h" "src/platform.h" dict;
			App.make_template "fetch_table32.h" "src/fetch_table.h" dict;
			App.make_template "decode_table32.h" "src/decode_table.h" dict;
			App.make_template "inst_size_table.h" "src/inst_size_table.h" dict;
			App.make_template "code_table.h" "src/code_table.h" dict;
			(* for ppc category table *)
			App.make_template "category_table.h" "src/category_table.h" dict;

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
