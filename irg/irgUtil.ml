(*
 * $Id$
 * Copyright (c) 2010, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of GLISS2.
 *
 * GLISS2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GLISS2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GLISS2; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

open Irg

(**	Run nmp2nml on the given file.
	@param file	File to run on.
	@return		NMP output. *)
let run_nmp2nml file =

	(* find the command *)
	let cmd =
		let cmd = Config.source_dir ^ "/gep/gliss-nmp2nml.pl" in
		if Sys.file_exists (native_path cmd) then cmd else
		let cmd = Config.install_dir ^ "/bin/gliss-nmp2nml.pl" in
		if Sys.file_exists (native_path cmd) then cmd else
		begin
			Printf.fprintf stderr "ERROR: cannot find gliss-nmp2nml.pl to process %s\n" file;
			exit 1
		end in

	(* run it *)
	let cmd =
		if is_windows then
			let cmd = "perl " ^ (native_path cmd) in
			Printf.sprintf "%s %s" cmd file
		else
			Printf.sprintf "%s %s" cmd file in
	Unix.open_process_in cmd


(** Performed several checks once all specification are known:
	* that all OR-operation are defined,
	* that all AND-operation parameters are defined.
	@raise Irg.Error	If an error is found. *)
let check_ops _ =

	let check_param op_name (_, t) =
		match t with
		| Irg.TYPE_EXPR _ -> ()
		| Irg.TYPE_ID n ->
			match Irg.get_symbol n with
			| Irg.TYPE _
			| Irg.AND_MODE _
			| Irg.OR_MODE _
			| Irg.AND_OP _
			| Irg.OR_OP _ -> ()
			| _ -> Irg.error_symbol op_name (fun out -> Printf.fprintf out "parameter type \"%s\" used in \"%s\" is not a valid type" n op_name ) in

	let check name op =
		match get_symbol op with
		| Irg.UNDEF -> error_symbol name (fun out -> Printf.fprintf out "symbol \"%s\" used in op \"%s\" is not defined" op name)
		| Irg.AND_OP _ | Irg.OR_OP _ -> ()
		| _ -> error_symbol name (fun out -> Printf.fprintf out "op \"%s\" used in \"%s\" should be an op" op name) in
		
	Irg.iter (fun name spec ->
		match spec with
		| Irg.OR_OP (_, ops) -> List.iter (check name) ops
		| Irg.AND_OP (_,  params, _) -> List.iter (check_param name) params
		| _ -> ())


(** Check that all OR-mode are defined.
	@raise Irg.Error	If an error is found. *)
let check_modes _ =

	let check_param mode_name (_, t) =
		match t with
		| Irg.TYPE_EXPR _ -> ()
		| Irg.TYPE_ID n ->
			match Irg.get_symbol n with
			| Irg.TYPE _
			| Irg.AND_MODE _
			| Irg.OR_MODE _ -> ()
			| _ -> error_symbol mode_name (fun out -> Printf.fprintf out "parameter type \"%s\" used in \"%s\" is not a valid type" n mode_name ) in

	let check name mode =
		match get_symbol mode with
		| Irg.UNDEF -> error_symbol name (fun out -> Printf.fprintf out "symbol \"%s\" used in mode \"%s\" is not defined" mode name)
		| Irg.OR_MODE _ | Irg.AND_MODE _ -> ()
		| _ -> error_symbol name (fun out -> Printf.fprintf out "symbol \"%s\" used in \"%s\" should be a mode" mode name) in
	Irg.iter (fun name spec ->
		match spec with
		| Irg.OR_MODE (_, modes) -> List.iter (check name) modes
		| Irg.AND_MODE (_,  params, _, _) -> List.iter (check_param name) params
		| _ -> ())


(* TODO*)
(* Perform a second complete check: no more "unknown type" should remain. *)


(** Second phase type check: expressions.
	@param e	Expression to check.
	@return		Checked expression.
	@raise		Irg.Error. *)
let rec check_expr e =
	
	let check t =
		if t == Irg.ANY_TYPE then Irg.pre_error "unresolved type" else t in
	
	match e with
	| Irg.COERCE (t, e) -> Irg.COERCE(t, check_expr e)
	| Irg.FORMAT (f, args) -> Irg.FORMAT (f, List.map check_expr args)
	| Irg.CANON_EXPR(t, f, args) ->
		if t == Irg.ANY_TYPE
		then Irg.CANON_EXPR (Irg.CARD(32), f, List.map check_expr args) 
		else Irg.CANON_EXPR (t, f, List.map check_expr args)
	| Irg.FIELDOF (t, pid, cid) ->
		let t = if t = Irg.ANY_TYPE then check (Sem.type_of_field pid cid) else t in 
		Irg.FIELDOF (t, pid, cid)
	| Irg.ELINE (f, l, e) ->
		(try Irg.ELINE(f, l, check_expr e)
		with Irg.PreError m -> Irg.complete_error m f l)  
	| Irg.ITEMOF (t, id, idx) ->
		Irg.ITEMOF (t, id, check_expr idx)
	| Irg.BITFIELD (t, v, u, l) ->
		let v = check_expr v in
		let u = check_expr u in
		let l = check_expr l in
		let t = if t == Irg.ANY_TYPE then (check (Sem.get_type_expr v)) else t in
		Irg.BITFIELD (t, v, u, l)	
	| Irg.UNOP (t, op, e) ->
		if t == Irg.ANY_TYPE then
			let (t, e) = Sem.check_unop (check_expr e) op in
			Irg.UNOP (check t, op, e)
		else e	
	| _ -> e


(** Load an NML description either NMP, NML or IRG.
	@param 	path		Path of the file to read from.
	@raise	Sys_error	If there is an error during the read. *)
let load path =

	let run_lexer path lexbuf =
		Lexer.file := path;
		Lexer.line := 1;
		Lexer.line_offset := 0;
		Lexer.lexbuf := lexbuf;
		Parser.top Lexer.main lexbuf;
		let _ = Iter.get_insts () in
		() in		

	(* is it an IRG file ? *)
	if Filename.check_suffix path ".irg" then
		Irg.load path

	(* is it NML ? *)
	else if Filename.check_suffix path ".nml" then
		run_lexer path (Lexing.from_channel (open_in path))

	(* is it NMP ? *)
	else if Filename.check_suffix path ".nmp" then
		let input = run_nmp2nml path in
		begin
			run_lexer path (Lexing.from_channel input);
			match Unix.close_process_in input with
			| Unix.WEXITED n when n = 0 -> ()
			| _ -> raise (Sys_error "ERROR: preprocessing failed.")
		end

	(* else error *)
	else
		raise (Sys_error (Printf.sprintf "ERROR: unknown file type: %s\n" path))


(** Load the given that may be .NMP, .NML or .IRG
	and display message to standard error channel if an error is raised.
	@param path		Path of file to load. *)
let load_with_error_support path =
	try
		load path;
	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error"; exit 2
	| Irg.SyntaxError msg ->
		Lexer.display_error msg; exit 2
	| Irg.Error f ->
		output_string stderr "ERROR: ";
		f stderr;
		output_char stderr '\n';
		exit 2
	| Irg.PreError f ->
		Printf.fprintf stderr "ERROR:%s: " (Lexer.current_loc ());
		f stderr;
		output_char stderr '\n';
		exit 2		
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr); exit 2
