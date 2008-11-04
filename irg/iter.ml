module Iter = struct

type t = Irg.spec
type value = STAT of Irg.stat | EXPR of Irg.expr


(* structure containing the specifications of all instantiated instructions,
initialised with something meaningless to help determine type of ref *)
let instr_set = ref [Irg.UNDEF]


(* iterator (or fold) on the structure containin all the instructions specs
	@param fun_to_iterate	function to apply to each instr with an accumulator as 1st param
	@param init_val		the accumulator, initial value *)
let iter fun_to_iterate init_val =
	let initialise_instrs =
		if !instr_set = [Irg.UNDEF] then
			instr_set := Irg.instantiate_instructions "instruction"
		else
			()
	in
	let rec rec_iter f init instrs =
		match instrs with
		[] ->
			init
		| a::b ->
			rec_iter f (f init a) b
	in
	begin
	initialise_instrs;
	rec_iter fun_to_iterate init_val !instr_set
	end

(** return an attr from an instruction specification
	@param instr	spec of the instrution
	@param name	name of the attr to return *)
let get_attr instr name =
	let rec search_attr_in_list n a_l =
		match a_l with
		[] -> 
		(* if attr not found => means an empty attr (?) *)
			raise Not_found
		| (Irg.ATTR_STAT(nm, s))::t ->
			if nm = n then
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
	| _ ->
		assert false

(* let get_id instr =

let get_name instr = *)

(** return the params (with their types) of an instruction specification
	@param instr	spec of the instrution *)
let get_params instr =
	match instr with
	Irg.AND_OP(_, param_list, _) ->
		param_list
	| _ ->
		assert false

(* let reduce instr e = *)

(** return the type of a symbol appearing in the spec of an instruction
	@param instr	spec of the instruction 
	@param var_name	name of the symbol whose type is required *)
let get_type instr var_name =
	let rec search_param_list nam p_l =
	match p_l with
	[] ->
		raise Not_found
	| (str, t)::q ->
		if str = nam then
			t
		else
			search_param_list nam q
	in
	match instr with
	Irg.AND_OP(_, p_l, _) ->
		search_param_list var_name p_l
	| _ ->
		assert false

end
