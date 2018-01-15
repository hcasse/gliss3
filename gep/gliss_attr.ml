(*
 * $Id: gliss-attr.ml,v 1.1 2009/09/15 07:50:48 casse Exp $
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

exception CommandError of string

(* library path *)
let paths = [
	Config.install_dir ^ "/lib/gliss/lib";
	Config.source_dir ^ "/lib";
	Sys.getcwd ()]


(* argument list *)
let out = ref "attr.c"
let template = ref ""
let do_func = ref false
let do_proc = ref false
let do_copy = ref false
let attr = ref ""
let def = ref "0"
let extends: string list ref = ref []
let options = [
	("-o", Arg.Set_string out, "output file");
	("-a", Arg.Set_string attr, "name of the attribute");
	("-t", Arg.Set_string template, "template file");
	("-f", Arg.Set do_func, "generate functions from expression");
	("-d", Arg.Set_string def, "default value");
	("-e", Arg.String (fun arg -> extends := arg::!extends), "extension files");
	("-p", Arg.Set do_proc, "generate functions from action attribute");
	("-c", Arg.Set do_copy, "generate the content of a string as-is (without quotes)");
]


(** Test if the statement is stateless.
	@param cc	Call context.
	@param s	Statement to test.
	@return		True if stateless, false else. *)
let rec stmt_stateless cc s =
	match s with
	| Irg.EVAL ("", id)
		-> sym_stateless cc id
	| Irg.NOP
	| Irg.EVAL _
	| Irg.ERROR _
	| Irg.LOCAL _
		-> true
	| Irg.SEQ (s1, s2)
		-> (stmt_stateless cc s1) && (stmt_stateless cc s2)
	| Irg.SET (l, e)
		-> (loc_stateless cc l) && (expr_stateless cc e)
	| Irg.CANON_STAT (_, args)
		-> List.for_all (expr_stateless cc) args
	| Irg.IF_STAT (e, s1, s2)
		-> (expr_stateless cc e) && (stmt_stateless cc s1) && (stmt_stateless cc s2)
	| Irg.SWITCH_STAT (e, cs, d)
		-> (expr_stateless cc e) && (stmt_stateless cc d) && List.for_all (stmt_stateless cc) (snd (List.split cs))
	| Irg.LINE (_, _, s)
		-> stmt_stateless cc s
	| Irg.FOR (v, uv, t, l, u, b)
		-> stmt_stateless cc b


(** Test if the expression is stateless.
	@param cc	Call context.
	@param e	Expression to test.
	@return		True if stateless, false else. *)
and expr_stateless cc e =
	match e with
	| Irg.NONE
	| Irg.CONST _
	| Irg.FIELDOF _
		-> true
	| Irg.COERCE (_, e)
	| Irg.UNOP (_, _, e)
	| Irg.ELINE (_, _, e)
	| Irg.CAST (_, e)
		-> expr_stateless cc e
	| Irg.ITEMOF (_, id, e)
		-> (sym_stateless cc id) && (expr_stateless cc e)
	| Irg.FORMAT (_, es)
	| Irg.CANON_EXPR (_, _, es)
		-> List.for_all (expr_stateless cc) es
	| Irg.BITFIELD (_, e1, e2, e3)
	| Irg.IF_EXPR (_, e1, e2, e3)
		-> List.for_all (expr_stateless cc) [e1; e2; e3]
	| Irg.BINOP(t, _, e1, e2)
		-> (expr_stateless cc e1) && (expr_stateless cc e2)
	| Irg.SWITCH_EXPR (_, e, cs, d)
		-> List.for_all (expr_stateless cc) (e::d::(snd (List.split cs)))
	| Irg.REF (_, id)
		-> sym_stateless cc id


(** Test if a symbol is stateless.
	For soundness purpose, UNDEF symbol is considered not stateless.
	@param cc	Call context.
	@param id	Symbol identifier.
	@return		True if symbol is stateless, false else. *)
and sym_stateless cc id =
	if List.mem id cc then true else
	let cc = id::cc in
	match Irg.get_symbol id with
	| Irg.UNDEF
		-> Printf.printf "DEBUG: undef %s\n" id; false
	| Irg.MEM _
	| Irg.REG _
		-> false
	| Irg.LET _
	| Irg.VAR _
	| Irg.PARAM (_, Irg.TYPE_EXPR _)
		-> true
	| Irg.PARAM (_, Irg.TYPE_ID id)
		-> sym_stateless cc id
	| Irg.TYPE _
		-> true
	| Irg.AND_MODE _
	| Irg.OR_MODE _
	| Irg.AND_OP _
	| Irg.OR_OP _
	| Irg.RES _
	| Irg.EXN _
	| Irg.CANON_DEF _
	| Irg.ATTR (Irg.ATTR_USES)
		-> failwith "meaningless with such symbol"
	| Irg.ATTR (Irg.ATTR_EXPR (_, e))
		-> expr_stateless cc e
	| Irg.ATTR (Irg.ATTR_STAT (_, s))
		-> stmt_stateless cc s
	| Irg.ATTR (Irg.ATTR_LOC (_, l))
		-> loc_stateless cc l

(** Test if a location is stateless.
	@param cc	Call context.
	@param l	Location to test.
	@return		True if stateless, false else. *)
and loc_stateless cc l =
	match l with
	| Irg.LOC_NONE
		-> true
	| Irg.LOC_REF (_, id, e1, e2, e3)
		-> (sym_stateless cc id) && (List.for_all (expr_stateless cc) [e1; e2; e3])
	| Irg.LOC_CONCAT (_, l1, l2)
		-> (loc_stateless cc l1) && (loc_stateless cc l2)


(** Perform the attribute generation of the given instruction.
	@param inst			Instruction to get syntax from.
	@param out			Output to use.
	@param info			Information about generation.
	@param stateless	True if the generation is stateless, false else.*)
let process inst out info stateless =
	info.Toc.out <- out;
	Toc.set_inst info inst;

	let error m =
		raise (Toc.Error (Printf.sprintf "attribute %s in %s %s" !attr info.Toc.iname m)) in

	let errorl m (s, l) =
		error (Printf.sprintf "at %s:%d %s" s l m) in

	let process e =

		(* constant attribute *)
		if not !do_func  then
			Irg.output_const info.Toc.out (Sem.eval_const e)

		(* function attribute *)
		else
			begin
				let params = Iter.get_params inst in
				Irg.param_stack params;
				let (s, e) = Toc.prepare_expr info Irg.NOP e in
				Toc.declare_temps info;
				Toc.gen_stat info s;
				output_string info.Toc.out "\treturn ";
				Toc.gen_expr info e true;
				output_string info.Toc.out ";\n";
				Toc.cleanup_temps info;
				Irg.param_unstack params
			end in

	let rec process_copy e =
		match e with
		| Irg.ELINE(_, _, e) ->
			process_copy e
		| Irg.CONST(_, Irg.STRING_CONST(v)) ->
			output_string info.Toc.out v
		| _ -> error "must be an action!" in
	
	try
		(* process a procedure *)
		if !do_proc then
			(match Iter.get_attr inst !attr with
			| Iter.EXPR _ -> error "must be an action!"
			| Iter.STAT s ->
				if stateless && not (stmt_stateless [] s)
				then errorl "is not stateless as it should be!"  (Irg.line_from_stat s)
				else Toc.gen_action info !attr)

		(* just copy *)
		else if !do_copy then
			(match Iter.get_attr inst !attr with
			| Iter.EXPR e ->
				if stateless && not (expr_stateless [] e)
				then errorl "is not stateless as it should be!"  (Irg.line_from_expr e)
				else process_copy e
			| _ -> error "must be a string constant!")

		(* process a function *)
		else
			(match Iter.get_attr inst !attr with
			| Iter.EXPR e ->
				if stateless && not (expr_stateless [] e)
				then errorl "is not stateless as it should be!" (Irg.line_from_expr e)
				else  process e
			| Iter.STAT _ -> error "must be an expression!")
		
	with Not_found ->
		output_string info.Toc.out !def
	| Irg.Error f | Irg.PreError f ->
		Irg.error (Irg.output [Irg.PTEXT (Printf.sprintf "%s not constant:" !attr); Irg.PFUN f])


let _ =
	try
		App.run
			options
			"SYNTAX: gep [options] NML_FILE\n\tGenerate code for a user attribute."
			(fun info ->

				(* download the extensions *)
				List.iter IrgUtil.load_with_error_support !extends;
				Iter.clear_insts ();
				ignore (Iter.get_insts ());

				(* perform generation *)
				if !template = "" then raise (CommandError "a template must specified with '-t'") else
				if !attr = "" then raise (CommandError "an attribute name must specified with '-a'") else
				let maker = App.maker () in
				maker.App.get_instruction <- (fun inst dict ->
					   (!attr, Templater.TEXT (fun out -> process inst out info false))
					:: (!attr ^ "!", Templater.TEXT (fun out -> process inst out info true))
					:: dict);
				let dict = App.make_env info maker in
				if not !App.quiet then (Printf.printf "creating \"%s\"\n" !out; flush stdout);
				Templater.generate_path dict !template !out;
			)
	with CommandError msg ->
		Printf.fprintf stderr "ERROR: %s\n" msg


