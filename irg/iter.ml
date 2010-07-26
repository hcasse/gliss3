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

(* Usefull when you want to use caml toplevel : *)
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
#load "templater.cmo";;
#load "parser.cmo";;
#load "irgUtil.cmo";;
#load "app.cmo";;
*)
(** Type of integrated instructions. *)
type inst = Irg.spec


(** Null instruction. *)
let null = Irg.UNDEF

type value = STAT of Irg.stat | EXPR of Irg.expr

let print_value v =
	match v with
	STAT(s) ->
		Irg.print_statement s
	| EXPR(e) ->
		Irg.print_expr e

(** Check if all SET or SETSPE statements in a spec are coerced if needed,
it happens when location and rvalue have different scalar types (card or int)
*)
let check_coerce spec =
	let rec check_stat sta =
		match sta with
		| Irg.SEQ(s1, s2) ->
			Irg.SEQ(check_stat s1, check_stat s2)
		| Irg.SET(l, e) ->
			Irg.SET(l, Sem.check_set_stat l e)
		| Irg.IF_STAT(e, s1, s2) ->
			Irg.IF_STAT(e, check_stat s1, check_stat s2)
		| Irg.SWITCH_STAT(e, es_l, s) ->
			Irg.SWITCH_STAT(e, List.map (fun (ex, st) -> (ex, check_stat st)) es_l, check_stat s)
		| Irg.SETSPE(l, e) ->
			Irg.SETSPE(l, Sem.check_set_stat l e)
		| Irg.LINE(s, i, st) ->
			Irg.LINE(s, i, check_stat st)
		| _ ->
			sta

	in
	let check_attr a =
		match a with
		Irg.ATTR_STAT(s, st) ->
			Irg.ATTR_STAT(s, check_stat st)
		| _ ->
			a
	in
	match spec with
	Irg.AND_OP(s, st_l, a_l) ->
		Irg.param_stack st_l;
		let res = Irg.AND_OP(s, st_l, List.map check_attr a_l)
		in
			Irg.param_unstack st_l;
			res
	| _ ->
		(* shouldn't happen *)
		spec

(* structure containing the specifications of all instantiated instructions,
initialised with something meaningless to help determine type of ref *)
let instr_set = ref [Irg.UNDEF]
(** List of instruction names sorted by ascending order of call number 
	This list is initialize if -p option is activated when calling GEP *)
let instr_stats : (string list ref) = ref [];;


(** return an attr from an instruction or mode specification
	@param instr	spec of the instrution or the mode
	@param name	name of the attr to return *)
let get_attr instr name =
	let rec search_attr_in_list n a_l =
		match a_l with
		[] -> 
		(* if attr not found => means an empty attr (?) *)
			raise Not_found
		| (Irg.ATTR_STAT(nm, s))::t ->
			if (String.compare nm n) == 0 then
				STAT(s)
			else
				search_attr_in_list n t
		| (Irg.ATTR_EXPR(nm, e))::t ->
			if nm = n then
				EXPR(e)
			else
				search_attr_in_list n t
		| _::t -> search_attr_in_list n t
	in
	match instr with
	Irg.AND_OP(_, _, a_l) ->
		search_attr_in_list name a_l
	| Irg.AND_MODE(_, _, _, a_l) ->
		search_attr_in_list name a_l
	| _ ->
		assert false

(** return true if the instruction is a branch *)
let is_branch_instr instr =
	try
		let _ = get_attr instr "set_attr_branch" 
		in
		   true
	with Not_found -> false

(* return the ID of the given instruction spec, 0 is for unknown instr
	@param instr	the spec of the instruction whose ID is needed *)
let get_id instr =
	let rec search_in_list l i num =
		match l with
		[] -> (* return 0 if instr not found (unknown) *)
			0
		| a::b ->
			if a = i then
				num
			else
				search_in_list b i (num+1)
	in
	search_in_list !instr_set instr 1


(* name cache *)
module HashInst =
struct
	type t = Irg.spec
	let equal (s1 : t) (s2 : t) = s1 == s2
	let hash (s : t) = Hashtbl.hash s
end
module NameTable = IdMaker.Make(HashInst)


(**
 * Get C identifier for the current instruction.
 * This name may be used to build other valid C names.
 * @param instr		Instruction to get name for.
 * @return			C name for the instruction.
 *)
let get_name instr =
	let rec to_string e =
		match e with
		  Irg.FORMAT(s, e_l) -> s
		| Irg.CONST(Irg.STRING, Irg.STRING_CONST(str, false, _)) -> str
		| Irg.ELINE(_, _, e) -> to_string e
		| Irg.IF_EXPR (_, _, e, _) -> to_string e
		| Irg.SWITCH_EXPR (_, _, cases, def) ->
			to_string (if (List.length cases) >= 1 then snd (List.hd cases) else def)
		| _ -> failwith "unsupported operator in syntax" in

	
	let syntax = match get_attr instr "syntax" with
		  EXPR(e) -> to_string e
		| _ -> failwith "syntax does not reduce to a string" in
	NameTable.make instr syntax


(** return the params (with their types) of an instruction specification
	@param instr	spec of the instrution *)
let get_params instr =
	match instr with
	Irg.AND_OP(_, param_list, _) ->
		param_list
	| _ ->
		assert false

(** Return the number of params of an instruction *)
let get_params_nb instr =
	match instr with
	Irg.AND_OP(_, param_list, _) ->
		List.length param_list
	| _ ->
		assert false


(* instantiate all known vars in a given expr
	@param instr	the spec whose params will give the vars to instantiate
	@param e	the expr to reduce *)
let reduce instr e =
	Instantiate.instantiate_in_expr e (get_params instr)

(** return the type of a symbol appearing in the spec of an instruction
	@param instr	spec of the instruction 
	@param var_name	name of the symbol whose type is required *)
let get_type instr var_name =
	let rec search_param_list nam p_l =
	match p_l with
	[] ->
		raise Not_found
	| (str, t)::q ->
		if (String.compare str nam) == 0 then
			t
		else
			search_param_list nam q
	in
	match instr with
	Irg.AND_OP(_, p_l, _) ->
		search_param_list var_name p_l
	| _ ->
		assert false

(** *)
let rec sort_instr_set instr_list stat_list = match stat_list with
  | []       -> []
  | name::q  -> 
	try
		let inst = List.find (fun a -> (get_name a) = name) instr_list 
		in
			(sort_instr_set instr_list q)  @ [inst]
	with Not_found -> failwith "Profiled file instructions statistics doesn't match current instruction generation"
			
(* iterator (or fold) on the structure containing all the instructions specs
	@param fun_to_iterate	function to apply to each instr with an accumulator as 1st param
	@param init_val		the accumulator, initial value 
     val iter : ('a -> Irg.spec -> 'a) -> 'a -> 'a 
*)
let iter fun_to_iterate init_val =
	let initialise_instrs =
		if !instr_set = [Irg.UNDEF] then
			instr_set :=  List.map check_coerce (Instantiate.instantiate_instructions "instruction")
		else
			()
	in
	(* if a profiling file is loaded instructions are sorted with the loaded profile_stats *)
	let _ = 
		if List.length !instr_stats = 0
		then  ()
		else
			 instr_set := sort_instr_set !instr_set !instr_stats
	in
  
	let rec rec_iter f init instrs params_to_unstack attrs_to_unstack =
		match instrs with
		[] ->
			init
		| a::b ->
			match a with
			Irg.AND_OP(_, param_l, attr_l) ->
			
				Irg.param_unstack params_to_unstack;
				Irg.attr_unstack attrs_to_unstack;
			
				Irg.param_stack param_l;
				Irg.attr_stack attr_l;
				
				rec_iter f (f init a) b param_l attr_l;
			| _ ->
				failwith "we should have only AND OP spec at this point (Iter)"	
	in
	begin
	initialise_instrs;
	rec_iter fun_to_iterate init_val !instr_set [] []
	end
	  
(** Compute the maximum params numbers of all instructions 
	from the current loaded IRG 
*)
let get_params_max_nb () = 
	let aux acc i =
		let nb_params = get_params_nb i 
		in 
		  if nb_params > acc 
		  then nb_params 
		  else acc
	in
	  iter aux 0  
