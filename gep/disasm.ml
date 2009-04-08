(*
 * $Id: disasm.ml,v 1.10 2009/04/08 08:27:46 casse Exp $
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

exception CommandError of string

(* library path *)
let paths = [
	Config.install_dir ^ "/lib/gliss/lib";
	Config.source_dir ^ "/lib";
	Sys.getcwd ()]


(* argument list *)
let nmp: string ref = ref ""
let quiet = ref false
let verbose = ref false
let out = ref "disasm.c"
let command = ref false
let options = [
	("-v", Arg.Set verbose, "verbose mode");
	("-q", Arg.Set quiet, "quiet mode");
	("-o", Arg.Set_string out, "output file");
	("-c", Arg.Set command, "generate also the command")
]

(* argument decoding *)
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


(** Generate code to perform disassembly.
	@param out	Output channel.
	@param inst	Current instruction.
	@param expr	Syntax expression.
	@raise Error	If there is an unsupported syntax expression. *)
let rec gen_disasm info inst expr =
	
	let str text = Irg.CONST (Irg.STRING, Irg.STRING_CONST text) in
	
	let format fmt args s i =
		if s >= i then
			Irg.NOP
		else
			let fmt = String.sub fmt s i in
			Irg.CANON_STAT ("__buffer += sprintf", (Irg.REF "__buffer")::(str fmt)::args) in
	
	let rec scan fmt args s used i =
		match args with
		| [] -> format fmt used s (String.length fmt)
		| hd::tl ->
			if i >= (String.length fmt) then format fmt used s i else
			if fmt.[i] <> '%' then scan fmt args s used (i + 1) else
			if i + 1 >= String.length fmt then format fmt used s i else
			if fmt.[i + 1] != 's' then scan fmt tl s (hd::used) (i + 2) else
			Irg.SEQ (format fmt used s i, scan fmt args (i + 2) [] (i + 2)) in
	
	match expr with
	| Irg.FORMAT (fmt, args) ->
		scan fmt args 0 [] 0
	| Irg.CONST (_, Irg.STRING_CONST s) ->
		format "%s" [str s] 0 2
	| Irg.NONE
	| Irg.CANON_EXPR _
	| Irg.REF _
	| Irg.FIELDOF _
	| Irg.ITEMOF _
	| Irg.BITFIELD _
	| Irg.UNOP _
	| Irg.BINOP _
	| Irg.IF_EXPR _
	| Irg.SWITCH_EXPR _
	| Irg.CONST _
	| Irg.COERCE _
	| Irg.EINLINE _ ->
		Toc.error_on_expr "bad syntax expression" expr
	| Irg.ELINE (file, line, e) ->
		Toc.locate_error file line (gen_disasm info inst) e


(** Perform the disassembling of the given instruction.
	@param inst		Instruction to get syntax from.
	@param out		Output to use. *)
let disassemble inst out info =
	info.Toc.out <- out;
	Toc.set_inst info inst;
	
	(* get syntax *)
	let syntax =
		try
			match Iter.get_attr inst "syntax" with
			  Iter.STAT _ -> raise (Toc.Error "syntax must be an expression")
			| Iter.EXPR e -> e
		with Not_found -> raise (Toc.Error "no attribute") in

	(* disassemble *)
	let params = Iter.get_params inst in
	Irg.param_stack params;
	let stats = Toc.prepare_stat info (gen_disasm info inst syntax) in
	Toc.declare_temps info;
	Toc.gen_stat info stats;
	Toc.cleanup_temps info;	
	Irg.param_unstack params


let _ =
	let display_error msg = Printf.fprintf stderr "ERROR: %s\n" msg in
	try
		App.process !nmp
			(fun info ->
				Irg.add_symbol "__buffer" (Irg.VAR ("__buffer", 1, Irg.NO_TYPE));
			
				(* generate disassemble source *)
				let maker = App.maker () in
				maker.App.get_instruction <- (fun inst dict ->
					("disassemble", Templater.TEXT (fun out -> disassemble inst out info)) :: dict);
				let dict = App.make_env info maker in			
				if not !quiet then (Printf.printf "creating \"%s\"\n" !out; flush stdout);
				Templater.generate dict "disasm.c" !out;
				
				(* generate the command *)
				if !command then
					begin
						try
							let path = App.find_lib "disasm/disasm.c" paths in
							App.makedir "disasm";
							App.replace_gliss info
								(path ^ "/" ^ "disasm/disasm.c")
								("disasm/" ^ info.Toc.proc ^ "-disasm.c" );
							Templater.generate_path
								[ ("proc", Templater.TEXT (fun out -> output_string out info.Toc.proc)) ]
								(path ^ "/disasm/Makefile")
								"disasm/Makefile"
						with Not_found ->
							raise (CommandError  "no template to make disasm program")
					end
			)
	with Toc.Error msg ->
		display_error msg
	| CommandError msg ->
		display_error msg
	| Toc.LocError (file, line, f) ->
		Printf.fprintf stderr "ERROR: %s:%d: " file line;
		f stderr;
		output_char stderr '\n'
	| Toc.PreError f ->
		output_string stderr "ERROR: ";
		f stderr;
		output_char stderr '\n'


