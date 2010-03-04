open Irg


(* function dealing with instruction instantiation *)
(* !!WARNING!! ELINE expr are not needed anymore, we simplify them as we treat them *)


(**
check if a statement attribute of a given instruction is recursive,
ie, if it calls itself
	@param	sp	the spec of the instruction
	@param	name	the name of the statement attribute to check
	@return		true if the attribute is recursive,
			false otherwise
*)
let is_attr_recursive sp name =
	let get_attr sp n =
		let rec aux al =
			match al with
			[] ->
				failwith ("attribute " ^ name ^ " not found or not a statement (instantiate.ml::is_attr_recursive)")
			| ATTR_STAT(nm, s)::t ->
				if (String.compare nm n) == 0 then
					s
				else
					aux t
			| _::t -> aux t
		in
		match sp with
		AND_OP(_, _, attrs) ->
			aux attrs
		| AND_MODE(_, _, _, a_l) ->
			aux a_l
		| _ ->
			failwith "shouldn't happen (instantiate.ml::is_attr_recursive::get_attr)"
	in
	(* return true if there is a call to a stat attr whose name is str in the st stat,
	we look for things like 'EVAL(str)' *)
	let rec find_occurence str st =
		match st with
		NOP ->
			false
		| SEQ(s1, s2) ->
			(find_occurence str s1) || (find_occurence str s2)
		| EVAL(s) ->
			((String.compare s str) == 0)
		| EVALIND(n, attr) ->
			(* recursivity occurs only when we refer to oneself, 'EVALIND' always refers to another spec *)
			false
		| SET(l, e) ->
			false
		| CANON_STAT(n, el) ->
			false
		| ERROR(s) ->
			false
		| IF_STAT(e, s1, s2) ->
			(find_occurence str s1) || (find_occurence str s2)
		| SWITCH_STAT(e, es_l, s) ->
			(find_occurence str s) || (List.exists (fun (ex, st) -> find_occurence str st) es_l)
		| SETSPE(l, e) ->
			false
		| LINE(s, i, st) ->
			find_occurence str st
		| INLINE _ ->
			false
	in
	let a = get_attr sp name
	in
	find_occurence name a


(** get the statement associated with attribute name_attr of the spec sp *)
let get_stat_from_attr_from_spec sp name_attr =
	let rec get_attr n a =
		match a with
		[] ->
		(* if attr not found => means an empty attr (?) *)
			NOP
		| ATTR_STAT(nm, s)::t ->
			if (String.compare nm n) == 0 then
				s
			else
				get_attr n t
		| _::t -> get_attr n t
	in
		match sp with
		  AND_OP(_, _, attrs) ->
			get_attr name_attr attrs
		| AND_MODE(_, _, _, attrs) ->
			get_attr name_attr attrs
		| _ ->
			failwith ("trying to access attribute " ^ name_attr ^ " of spec " ^ (name_of sp) ^ " which is neither an OP or a MODE (instantiate.ml::get_stat_from_attr_from_spec)")


(* symbol substitution is needed *)

(* different possibilities for var instantiation,
prefix the new name by the old name,
replace the old name by the new name,
keep the old name *)
(*type var_inst_type =
	Prefix_name
	| Replace_name
	| Keep_name
*)
(** look a given spec (mode or op), return true if the new vars from a substition of a var (with the given name) of this spec type should be prefixed,
false if the var name can simply be replaced,
top_params is the list of params of a spec in which we want to instantiate a var of sp type (string * typ list)

sp = 1 param mode => Keep_name (unicity in the spec to instantiate => we can simply keep the name)
sp = * param mode => Prefix_name
sp = 1 param op   => Replace_name, or Prefix_name if more than 1 param of sp type in the top spec or if the new name would conflict
sp = * param op   => Replace_name, or Prefix_name if more than 1 param of sp type in the top spec or if one of the new names would conflict
*)
(*let check_if_prefix_needed sp name top_params =
	let given_spec_name =
		match sp with
		AND_OP(n, _, _) ->
			n
		| AND_MODE(n, _, _, _) ->
			n
		| _ ->
			failwith "shouldn't happen? (instantiate.ml::check_if_prefix_needed::spec_name)"
	in
	let is_param_of_given_type p =
		match p with
		(_, t) ->
			(match t with
			TYPE_ID(n) ->
				n = given_spec_name
			| _ ->
				false
			)
	in
	let rec count_same_type_params p_l =
		match p_l with
		[] ->
			0
		| a::b ->
			if is_param_of_given_type a then
				1 + (count_same_type_params b)
			else
				count_same_type_params b
	in
	let name_of_param p =
		match p with
		(n, _) ->
			n
	in
	let sp_param_names =
		match sp with
		AND_OP(_, p_l, _) ->
			List.map name_of_param p_l
		| AND_MODE(_, p_l, _, _) ->
			List.map name_of_param p_l
		| _ ->
			failwith "shouldn't happen? (instantiate.ml::check_if_prefix_needed::get_sp_param_names)"
	in*)
	(* return true if a param whose name is in n_l is existing in the given param list *)
	(*let rec is_name_conflict p_l n_l =
		match p_l with
		[] ->
			false
		| a::b ->
			((name_of_param a) = n) || (is_conflict_with_name b)
	in*)
	(*match sp with
	AND_OP(_, params, _) ->
		if ((count_same_type_params top_params) > 1) || (is_conflict_with_name top_params) then
				Prefix_name
			else
				Replace_name
	| AND_MODE(_, params, _, _) ->
		if List.length params > 1 then
			Prefix_name
		else
			Keep_name
	| _ ->
		failwith "shouldn't happen? (instantiate.ml::check_if_prefix_needed)"
*)



(** get the expression associated with attribute name_attr of the OP op *)
let get_expr_from_attr_from_op_or_mode sp name_attr =
	let rec get_attr n a =
		match a with
		[] ->
		(* if attr not found => means an empty attr (?) *)
		(* !!DEBUG!! *)
		(*print_string ("attr of name " ^ name_attr ^ " not found in spec=\n");
		print_spec sp;*)
			NONE
		| ATTR_EXPR(nm, e)::t ->
			if (String.compare nm n) == 0 then
				(* !!DEBUG!! *)
				(*(print_string "res = [[";
				Irg.print_expr e;
				print_string "]]\n";*)
				e
			else
				get_attr n t
		| _::t -> get_attr n t
	in
		(* !!DEBUG!! *)
		(*print_string ("get_expr_from_attr, name_attr=" ^ name_attr ^ ", spec=" ^ (Irg.name_of sp) ^ "\n");*)
		match sp with
		AND_OP(_, _, attrs) ->
			get_attr name_attr attrs
		| AND_MODE(_, _, _, a_l) ->
			get_attr name_attr a_l
		| _ ->
		(* !!DEBUG!! *)
		(*print_string name_attr;*)
			failwith "cannot get an expr attribute from not an AND OP or an AND MODE (instantiate.ml::get_expr_from_attr_from_op_or_mode)"


let rec substitute_in_expr name op ex =
	let is_and_mode sp =
		match sp with
		AND_MODE(_, _, _, _) -> true
		| _ -> false
	in
	let get_mode_value sp =
		match sp with
		AND_MODE(_, _, v, _) -> v
		| _-> NONE
	in
	match ex with
	NONE ->
		NONE
	| COERCE(te, e) ->
		COERCE(te, substitute_in_expr name op e)
	| FORMAT(s, e_l) ->
		(* done by another function which replace everything in one shot *)
		(* FORMAT(s, e_l) *)
		(* !!TODO!! apply same treatment to any expr, then reduce formats *)
		FORMAT(s, List.map (substitute_in_expr name op) e_l)
	| CANON_EXPR(te, s, e_l) ->
		CANON_EXPR(te, s, List.map (substitute_in_expr name op) e_l )
	| REF(s) ->
		(* change if op is a AND_MODE and s refers to it *)
		if (name=s)&&(is_and_mode op) then
			get_mode_value op
		else
			(* change also if s refers to an ATTR_EXPR of the same spec, does it have this form ? *)
			REF(s)
	| FIELDOF(te, s1, s2) ->
	(* !!DEBUG!! *)
	(*print_string ("subst in expr:\nname =[" ^ name ^ "]\nexpr =[");
	print_expr ex;
	print_string "]\nop =[";
	print_spec op; print_string "]\n";
	print_string "res = ["; print_expr (get_expr_from_attr_from_op_or_mode op s2); print_string "]\n\n"; flush stdout;*)
		if (String.compare s1 name) == 0 then
			(* !!DEBUG!! *)
			begin
			(*print_string "subst_expr spec={{\n";
			Irg.print_spec op;
			print_string "}}\nsubst_expr (then) [[";
			Irg.print_expr ex;
			print_string "]] => [[";
			print_expr (get_expr_from_attr_from_op_or_mode op s2);
			print_string "]]\n";
			*)
			get_expr_from_attr_from_op_or_mode op s2
			end
		else
			(* !!DEBUG!! *)
			begin
			(*print_string "subst_expr (else) [[";
			Irg.print_expr ex;
			print_string "]]\n";
			*)
			FIELDOF(te, s1, s2)
			end
	| ITEMOF(te, e1, e2) ->
		ITEMOF(te, e1, substitute_in_expr name op e2)
	| BITFIELD(te, e1, e2, e3) ->
		BITFIELD(te, substitute_in_expr name op e1, substitute_in_expr name op e2, substitute_in_expr name op e3)
	| UNOP(te, un_op, e) ->
		UNOP(te, un_op, substitute_in_expr name op e)
	| BINOP(te, bin_op, e1, e2) ->
		BINOP(te, bin_op, substitute_in_expr name op e1, substitute_in_expr name op e2)
	| IF_EXPR(te, e1, e2, e3) ->
		IF_EXPR(te, substitute_in_expr name op e1, substitute_in_expr name op e2, substitute_in_expr name op e3)
	| SWITCH_EXPR(te, e1, ee_l, e2) ->
		SWITCH_EXPR(te, substitute_in_expr name op e1, List.map (fun (x,y) -> (substitute_in_expr name op x, substitute_in_expr name op y)) ee_l, substitute_in_expr name op e2)
	| CONST(te, c)
		-> CONST(te, c)
	| ELINE (file, line, e) ->
		(* ELINE useless here *)
		substitute_in_expr name op e
	| EINLINE _ ->
		ex



let rec change_name_of_var_in_expr ex var_name new_name =
	let get_name_param s =
		if (String.compare s var_name) == 0 then
			new_name
		else
			s
	in
	match ex with
	NONE ->
		NONE
	| COERCE(t_e, e) ->
		COERCE(t_e, change_name_of_var_in_expr e var_name new_name)
	| FORMAT(s, e_l) ->
		FORMAT(s, List.map (fun x -> change_name_of_var_in_expr x var_name new_name) e_l)
	| CANON_EXPR(t_e, s, e_l) ->
		CANON_EXPR(t_e, s, List.map (fun x -> change_name_of_var_in_expr x var_name new_name) e_l)
	| REF(s) ->
		REF (get_name_param s)
	| FIELDOF(t_e, e, s) ->
		FIELDOF(t_e, (*change_name_of_var_in_expr*) get_name_param e (*var_name new_name*), s)
	| ITEMOF(t_e, e1, e2) ->
		ITEMOF(t_e, e1, change_name_of_var_in_expr e2 var_name new_name)
	| BITFIELD(t_e, e1, e2, e3) ->
		BITFIELD(t_e, change_name_of_var_in_expr e1 var_name new_name, change_name_of_var_in_expr e2 var_name new_name, change_name_of_var_in_expr e3 var_name new_name)
	| UNOP(t_e, u, e) ->
		UNOP(t_e, u, change_name_of_var_in_expr e var_name new_name)
	| BINOP(t_e, b, e1, e2) ->
		BINOP(t_e, b, change_name_of_var_in_expr e1 var_name new_name, change_name_of_var_in_expr e2 var_name new_name)
	| IF_EXPR(t_e, e1, e2, e3) ->
		IF_EXPR(t_e, change_name_of_var_in_expr e1 var_name new_name, change_name_of_var_in_expr e2 var_name new_name, change_name_of_var_in_expr e3 var_name new_name)
	| SWITCH_EXPR(te, e1, ee_l, e2) ->
		SWITCH_EXPR(te, change_name_of_var_in_expr e1 var_name new_name, List.map (fun (x,y) -> (change_name_of_var_in_expr x var_name new_name, change_name_of_var_in_expr y var_name new_name)) ee_l, change_name_of_var_in_expr e2 var_name new_name)
	| CONST(t_e, c) ->
		CONST(t_e, c)
	| ELINE(file, line, e) ->
		(* ELINE no more useful here *)
		change_name_of_var_in_expr e var_name new_name
	| EINLINE _ ->
		ex


let rec change_name_of_var_in_location loc var_name new_name =
	match loc with
	| LOC_NONE -> loc
	| LOC_REF(t, s, i, l, u) ->
		LOC_REF (
			t,
			(if (String.compare s var_name) == 0 then new_name else s),
			change_name_of_var_in_expr i var_name new_name,
			change_name_of_var_in_expr l var_name new_name,
			change_name_of_var_in_expr u var_name new_name)
	| LOC_CONCAT(t, l1, l2) ->
		LOC_CONCAT(t, change_name_of_var_in_location l1 var_name new_name, change_name_of_var_in_location l2 var_name new_name)

let rec substitute_in_location name op loc =
(*print_string ("subst_location\n\tname=" ^ name);		(* !!DEBUG!! *)
print_string "\n\tloc="; print_location loc;		(* !!DEBUG!! *)
print_string "\nspec ="; print_spec op; flush stdout;			(* !!DEBUG!! *)*)
	let get_mode_value sp =
		match sp with
		AND_MODE(_, _, v, _) -> v
		| _-> NONE
	in
	let is_and_mode sp =
		match sp with
		AND_MODE(_, _, _, _) -> true
		| _ -> false
	in
	match loc with
	LOC_NONE ->
		loc
	| LOC_REF(t, s, i, l, u) ->
		let rec subst_mode_value mv =
			match mv with
			REF(n) ->
				LOC_REF(t, n, substitute_in_expr name op i, substitute_in_expr name op l, substitute_in_expr name op u)
			| ITEMOF(typ, n, idx) ->
				(* can replace only if loc is "simple" (ie i = NONE), we can't express n[idx][i] *)
				if i = NONE then
					LOC_REF(typ, n, idx, substitute_in_expr name op l, substitute_in_expr name op u)
				else
					failwith "cannot substitute a var here (ITEMOF) (instantiate.ml::substitute_in_location)"
			| BITFIELD(typ, n, lb, ub) ->
				if i=NONE then
					if u=NONE && l=NONE then
						(match n with
						REF(nn) ->
							LOC_REF(typ, nn, NONE, lb, ub)
						| _ ->
							failwith "cannot substitute here (BITFIELD 1), loc_expr removed (instantiate.ml::substitute_in_location)"
							(*LOC_EXPR(mv)*)
						)
					else
						failwith "cannot substitute here (BITFIELD 2), loc_expr removed (instantiate.ml::substitute_in_location)"
						(*LOC_EXPR(BITFIELD(t, mv, substitute_in_expr name op l, substitute_in_expr name op u))*)
				else
					(* we can't express n<lb..ub>[i], it is meaningless *)
					failwith "cannot substitute a var here (BITFIELD 3) (instantiate.ml::substitute_in_location)"
			| ELINE(str, lin, e) ->
				subst_mode_value e
			| _ ->
				if i=NONE then
					if u=NONE && l=NONE then
						failwith "cannot substitute here (_ 1), loc_expr removed (instantiate.ml::substitute_in_location)"
						(*LOC_EXPR(mv)*)
					else
						failwith "cannot substitute here (_ 2), loc_expr removed (instantiate.ml::substitute_in_location)"
						(*LOC_EXPR(BITFIELD(t, mv, substitute_in_expr name op l, substitute_in_expr name op u))*)
				else
					(* how could we express stg like (if .. then .. else .. endif)[i]<l..u>, it would be meaningless most of the time *)
					failwith "cannot substitute a var here (_ 3) (instantiate.ml::substitute_in_location)"
		in
		(* change if op is a AND_MODE and s refers to it *)
		(* as mode values, we will accept only those "similar" to a LOC_REF (REF, ITEMOF, BITFIELD, (FIELDOF)) *)
		(* NO!! we must accept any expression!! *)
		if (name=s)&&(is_and_mode op) then
		begin
		(* !!DEBUG!! *)
		(*print_string "========res=";
		print_location (subst_mode_value (get_mode_value op));
		print_string "\n\n\n";*)
			subst_mode_value (get_mode_value op) end
		else begin
		(* !!DEBUG!! *)
		(*print_string "========res=";
		print_location (LOC_REF(t, s, substitute_in_expr name op i, substitute_in_expr name op l, substitute_in_expr name op u));
		print_string "\n\n\n";*)
			LOC_REF(t, s, substitute_in_expr name op i, substitute_in_expr name op l, substitute_in_expr name op u) end
	| LOC_CONCAT(t, l1, l2) ->
		(*print_string "========res=";
		print_location (LOC_CONCAT(t, substitute_in_location name op l1, substitute_in_location name op l2));
		print_string "\n\n\n";*)
		LOC_CONCAT(t, substitute_in_location name op l1, substitute_in_location name op l2)


(** search the symbol name in the given statement,
the symbol is supposed to stand for a variable of type given by op,
all occurrences of names are translated to refer to the op *)
let rec substitute_in_stat name op statement =
(*print_string ("subst_stat name=" ^ name);		(* !!DEBUG!! *)
print_string "\n\tstat="; print_statement statement;	(* !!DEBUG!! *)
print_string "spec ="; print_spec op;	*)		(* !!DEBUG!! *)
	match statement with
	NOP ->
		NOP
	| SEQ(s1, s2) ->
		SEQ(substitute_in_stat name op s1, substitute_in_stat name op s2)
	| EVAL(s) ->
		EVAL(s)
	| EVALIND(n, attr) ->
		if (String.compare n name) == 0 then
		begin
			(* !!DEBUG!! *)
			(*print_string "subst_stat (then) [[";
			Irg.print_statement statement;
			print_string "]] => [[";
			if (String.compare attr "action") == 0 then
				print_string "<ACTION>"
			else
				Irg.print_statement (get_stat_from_attr_from_spec op attr);
			print_string "]]\n";*)
			
			if is_attr_recursive op attr then
				(*  transform x.action into x_action (this will be a new attr to add to the final spec) *)
				EVAL(n ^ "_" ^ attr)
			else
				get_stat_from_attr_from_spec op attr
		end
		else
			(* !!DEBUG!! *)
			(*print_string "subst_stat (else) [[";
			Irg.print_statement statement;
			print_string "]]\n";*)
			
			EVALIND(n, attr)
	| SET(l, e) ->
		(* !!DEBUG!! *)
		(*print_string "substitute_in_stat, SET,\nloc="; print_location l; print_string "\nexpr="; print_expr e; print_string "\n";flush stdout;*)
		SET(substitute_in_location name op l, substitute_in_expr name op e)
	| CANON_STAT(n, el) ->
		CANON_STAT(n, el)
	| ERROR(s) ->
		ERROR(s)
	| IF_STAT(e, s1, s2) ->
		IF_STAT(substitute_in_expr name op e, substitute_in_stat name op s1, substitute_in_stat name op s2)
	| SWITCH_STAT(e, es_l, s) ->
		SWITCH_STAT(substitute_in_expr name op e, List.map (fun (ex, st) -> (ex, substitute_in_stat name op st)) es_l, substitute_in_stat name op s)
	| SETSPE(l, e) ->
		(* !!DEBUG!! *)
		(*print_string "substitute_in_stat, SETSPE,\nloc="; print_location l; print_string "\nexpr="; print_expr e; print_string "\n";flush stdout;*)
		SETSPE(substitute_in_location name op l, substitute_in_expr name op e)
	| LINE(s, i, st) ->
		LINE(s, i, substitute_in_stat name op st)
	| INLINE _ ->
		statement



let rec change_name_of_var_in_stat sta var_name new_name =
	match sta with
	NOP ->
		NOP
	| SEQ(s1, s2) ->
		SEQ(change_name_of_var_in_stat s1 var_name new_name, change_name_of_var_in_stat s2 var_name new_name)
	| EVAL(str) ->
		EVAL(str)
	| EVALIND(v, attr_name) ->
		if (String.compare v var_name) == 0 then
			EVALIND(new_name, attr_name)
		else
			EVALIND(v, attr_name)
	| SET(l, e) ->
		SET(change_name_of_var_in_location l var_name new_name, change_name_of_var_in_expr e var_name new_name)
	| CANON_STAT(str, e_l) ->
		CANON_STAT(str, List.map (fun x -> change_name_of_var_in_expr x var_name new_name) e_l)
	| ERROR(str) ->
		ERROR(str) (* ??? *)
	| IF_STAT(e, s1, s2) ->
		IF_STAT(change_name_of_var_in_expr e var_name new_name, change_name_of_var_in_stat s1 var_name new_name, change_name_of_var_in_stat s2 var_name new_name)
	| SWITCH_STAT(e, es_l, s) ->
		SWITCH_STAT(change_name_of_var_in_expr e var_name new_name, List.map (fun (x,y) -> (change_name_of_var_in_expr x var_name new_name, change_name_of_var_in_stat y var_name new_name)) es_l, change_name_of_var_in_stat s var_name new_name)
	| SETSPE(l, e) ->
		SETSPE(change_name_of_var_in_location l var_name new_name, change_name_of_var_in_expr e var_name new_name)
	| LINE(str, n, s) ->
		LINE(str, n, change_name_of_var_in_stat s var_name new_name)
	| INLINE _ ->
		sta


let change_name_of_var_in_attr a var_name new_name =
	match a with
	ATTR_EXPR(str, e) ->
		ATTR_EXPR(str, change_name_of_var_in_expr e var_name new_name)
	| ATTR_STAT(str, s) ->
		ATTR_STAT(str, change_name_of_var_in_stat s var_name new_name)
	| ATTR_USES ->
		ATTR_USES

let prefix_attr_var a param pfx =
	match param with
	(str, _) ->
		change_name_of_var_in_attr a str (pfx^"_"^str)

let rec prefix_all_vars_in_attr a p_l pfx =
	match p_l with
	h::q ->
		prefix_all_vars_in_attr (prefix_attr_var a h pfx) q pfx
	| [] ->
		a

let prefix_var_in_mode_value mode_value param pfx =
	match param with
	(str, _) ->
		change_name_of_var_in_expr mode_value str (pfx^"_"^str)

let rec prefix_all_vars_in_mode_value mode_value p_l pfx =
	match p_l with
	h::q ->
		prefix_all_vars_in_mode_value (prefix_var_in_mode_value mode_value h pfx) q pfx
	| [] ->
		mode_value

(* prefix the name of every param of the given spec by a given prefix,
every occurrence in every attr must be prefixed *)
let rec prefix_name_of_params_in_spec sp pfx =
	let prefix_param param =
		match param with
		(name, t) ->
			(pfx^"_"^name, t)
	in
	match sp with
	AND_OP(name, params, attrs) ->
		AND_OP(name, List.map prefix_param params, List.map (fun x -> prefix_all_vars_in_attr x params pfx) attrs)
	| AND_MODE(name, params, mode_val, attrs) ->
		AND_MODE(name, List.map prefix_param params, prefix_all_vars_in_mode_value mode_val params pfx, List.map (fun x -> prefix_all_vars_in_attr x params pfx) attrs)
	| _ ->
		sp








let get_param_of_spec s =
	match s with
	AND_OP(_, l, _) -> l
	| AND_MODE(_, l, _, _) -> l
	| _ -> []

(*  *)
let replace_param_list p_l =
	let prefix_name prfx param =
		match param with
		(name, t) ->
			(prfx^"_"^name, t)
	in
	let replace_param param =
		match param with
		(nm , TYPE_ID(s)) ->
			(* prefix si on va vers plsrs params de type op (au moins 2 de type op),
			ou si on va d'un mode vers ses params (>1),
			remplace si d'un op vers un autre op,
			ou d'un mode vers un seul param *)
			List.map (prefix_name nm) (get_param_of_spec (get_symbol s))
		| (_, _) ->
			[param]
	in
	List.flatten (List.map replace_param p_l)



let rec regexp_list_to_str_list l =
	match l with
	[] -> []
	| Str.Text(txt)::b -> txt::(regexp_list_to_str_list b)
	| Str.Delim(txt)::b -> txt::(regexp_list_to_str_list b)

let string_to_regexp_list s =
	let process_double_percent e =
		match e with
		Str.Text(t) ->
			e
		| Str.Delim(t) ->
			if (String.compare t "%%") == 0 then
				Str.Text("%%")
			else
				e
	in
	let res = List.map process_double_percent (Str.full_split (Str.regexp "%[0-9]*[dbxsf]\|%%") s)
	in
	(* !!DEBUG!! *)
	(*print_string ("string_to_regexp_list(" ^ s ^ ")=[");
	List.iter
		(fun x ->
			match x with
			Str.Text(t) -> print_string ("[T]"^t)
			| Str.Delim(t)-> print_string ("[D]"^t) )
		res;
	print_string "]\n";*)
	res
	

let str_list_to_str l =
	String.concat "" l

let print_reg_exp e =
	match e with
	Str.Text(t) ->
		print_string t
	| Str.Delim(t) ->
		print_string t

let rec print_reg_list e_l =
	match e_l with
	[] ->
		()
	| a::b ->
		begin
		print_reg_exp a;
		print_string ":";
		print_reg_list b
		end



let rec simplify_format_expr ex =
	let rec reduce f params =
		(* !!DEBUG!! *)
		(*print_string "simpl_frmt_expr, str_list=[";
		List.iter
			(fun x ->
				match x with
				Str.Text(t) -> print_string ("[T]"^t)
				| Str.Delim(t)->print_string ("[D]"^t) )
			f;
		print_string "] params=[";
		List.iter (fun x -> Irg.print_expr x; print_string "; " ) params;
		print_string "]\n";*)
		
		match f with
		[] ->
			if params = [] then
				([], [])
			else
				failwith "too much params here (instantiate.ml::simplify_format_expr::reduce)"
		| f1::f_l ->
			(match f1 with
			Str.Text(t) ->
				(* simple text *)
				let (a, b) = reduce f_l params
				in
				(* !!DEBUG!! *)
				(*print_string ("s_f_e(f1=text) ["^t^"]\n");*)
				(f1::a, b)
			| Str.Delim(t) ->
				(* format *)
				(match params with
				[] ->
					failwith "not enough params here (instantiate.ml::simplify_format_expr::reduce)"
				| p1::p_l ->
					(match p1 with
					FORMAT(s, e_l) ->
						(* reduce the format, check compatibility type between format and param later *)
						let new_f1 = string_to_regexp_list s
						in
						let (a, b) = reduce f_l p_l
						in
						(* !!DEBUG!! *)
						(*print_string "s_f_e(f1=delim)(p1(frmt)=[[";
						print_expr p1;
						print_string ("]]) ["^t^"]\n");*)
						
						(new_f1 @ a, e_l @ b)
					| _ ->
						let (a, b) = reduce f_l p_l
						in
						(* !!DEBUG!! *)
						(*print_string "s_f_e(f1=delim)(p1(_)=[[";
						print_expr p1;
						print_string ("]]) ["^t^"]\n");*)
						(f1::a, p1::b)
					)
				)
			)
	in
	match ex with
	FORMAT(s, e_l) ->
		let str_format =
		(* !!DEBUG!! *)
		(*print_string ("simplify_format, frmt(" ^ s ^ "; ");
		List.iter (fun x -> Irg.print_expr x; print_string "; ") e_l;
		print_string ")\n";*)
		
		string_to_regexp_list s
		in
		let simpl_e_l = List.map simplify_format_expr e_l
		in
		let (new_s, new_e_l) = reduce str_format simpl_e_l
		in
		FORMAT((str_list_to_str (regexp_list_to_str_list new_s)), new_e_l)
	| NONE ->
		NONE
	| COERCE(t_e, e) ->
		COERCE(t_e, simplify_format_expr e)
	| CANON_EXPR(t_e, s, e_l) ->
		CANON_EXPR(t_e, s, List.map simplify_format_expr e_l)
	| REF(s) ->
		REF (s)
	| FIELDOF(t_e, e, s) ->
		FIELDOF(t_e, e, s)
	| ITEMOF(t_e, e1, e2) ->
		ITEMOF(t_e, e1, simplify_format_expr e2)
	| BITFIELD(t_e, e1, e2, e3) ->
		BITFIELD(t_e, simplify_format_expr e1, simplify_format_expr e2, simplify_format_expr e3)
	| UNOP(t_e, u, e) ->
		UNOP(t_e, u, simplify_format_expr e)
	| BINOP(t_e, b, e1, e2) ->
		BINOP(t_e, b, simplify_format_expr e1, simplify_format_expr e2)
	| IF_EXPR(t_e, e1, e2, e3) ->
		IF_EXPR(t_e, simplify_format_expr e1, simplify_format_expr e2, simplify_format_expr e3)
	| SWITCH_EXPR(te, e1, ee_l, e2) ->
		SWITCH_EXPR(te, simplify_format_expr e1, List.map (fun (x,y) -> (simplify_format_expr x, simplify_format_expr y)) ee_l, simplify_format_expr e2)
	| CONST(t_e, c) ->
		CONST(t_e, c)
	| ELINE(file, line, e) ->
		simplify_format_expr e
	| EINLINE _ ->
		ex


let rec remove_const_param_from_format f =
	let get_length_from_format regexp =
		let f =
			match regexp with
			Str.Delim(t) -> t
			| _ -> failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::get_length_from_format::f)"
		in
		let l = String.length f in
		let new_f =
			if l<=2 then
			(* shouldn't happen, we should have only formats like %[0-9]*b, not %d or %f *)
				"0"
			else
				String.sub f 1 (l-2)
		in
		Scanf.sscanf new_f "%d" (fun x->x)
	in
	let rec int32_to_string01 i32 size accu =
		let bit = Int32.logand i32 Int32.one
		in
		let res = (Int32.to_string bit) ^ accu
		in
		if size > 0 then
			int32_to_string01 (Int32.shift_right_logical i32 1) (size - 1) res
		else
			accu
	in
	let rec int64_to_string01 i64 size accu =
		let bit = Int64.logand i64 Int64.one
		in
		let res = (Int64.to_string bit) ^ accu
		in
		if size > 0 then
			int64_to_string01 (Int64.shift_right_logical i64 1) (size - 1) res
		else
			accu
	in
	let is_string_format regexp =
		match regexp with
		Str.Delim(t) ->
			(String.compare t "%s") == 0
		| _ ->
			failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::is_string_format)"
	in
	let is_integer_format regexp =
		match regexp with
		Str.Delim(t) ->
			(String.compare t "%d") == 0
		| _ ->
			failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::is_integer_format)"
	in
	let is_binary_format regexp =
		match regexp with
		Str.Delim(t) ->
			if (String.length t) <= 2 then
				false
			else
				t.[(String.length t) - 1] = 'b'
		| _ ->
			failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::is_binary_format)"
	in
	let is_float_format regexp =
		match regexp with
		Str.Delim(t) ->
			(String.compare t "%f") == 0
		| _ ->
			failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::is_float_format)"
	in
	let replace_const_param_in_format_string regexp param =
		match param with
		CONST(t, c) ->
			(match c with
			CARD_CONST(i) ->
				if is_integer_format regexp then
					Str.Text(Int32.to_string i)
				else
					if is_binary_format regexp then
						Str.Text(int32_to_string01 i (get_length_from_format regexp) "")
					else
						failwith "bad format, a 32 bit integer constant can be displayed only with \"%d\" and \"%xxb\" (instantiate.ml::remove_const_param_from_format::replace_const_param_in_format_string)"
			| CARD_CONST_64(i) ->
				if is_integer_format regexp then
					Str.Text(Int64.to_string i)
				else
					if is_binary_format regexp then
						Str.Text(int64_to_string01 i (get_length_from_format regexp) "")
					else
						failwith "bad format, a 64 bit integer constant can be displayed only with \"%d\" and \"%xxb\" (instantiate.ml::remove_const_param_from_format::replace_const_param_in_format_string)"
			| STRING_CONST(s) ->
				if is_string_format regexp then
					Str.Text(s)
				else
					failwith "bad format, a string constant can be displayed only with \"%s\" (instantiate.ml::remove_const_param_from_format::replace_const_param_in_format_string)"
			| FIXED_CONST(f) ->
				if is_float_format regexp then
					(* TODO: check if the output is compatible with C representation of floats *)
					Str.Text(string_of_float f)
				else
					failwith "bad format, a float constant can be displayed only with \"%f\" (instantiate.ml::remove_const_param_from_format::replace_const_param_in_format_string)"
			| NULL ->
				(* wtf!? isn't that supposed to be an error ? in the doubt... let it through *)
				regexp
			)
		| _ ->
			regexp
	in
	let replace_const_param_in_param regexp param =
		match param with
		CONST(t, c) ->
			[]
		| _ ->
			[param]
	in
	(* simplify the regexp list *)
	let rec r_aux r_l p_l =
		match r_l with
		[] ->
			[]
		| a::b ->
			(match a with
			Str.Text(t) ->
				a::(r_aux b p_l)
			| Str.Delim(d) ->
				(match p_l with
				[] ->
					(* not enough params ! *)
					failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::r_aux)"
				| t::u ->
					(replace_const_param_in_format_string a t)::(r_aux b u)
				)
			)
	in
	(* simplify the param list *)
	let rec p_aux r_l p_l =
		match r_l with
		[] ->
			if p_l = [] then
				[]
			else
				(* not enough formats ! *)
				failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::p_aux)"
		| a::b ->
			(match a with
			Str.Text(t) ->
				p_aux b p_l
			| Str.Delim(d) ->
				(match p_l with
				[] ->
					(* not enough params ! *)
					failwith "shouldn't happen (instantiate.ml::remove_const_param_from_format::p_aux)"
				| t::u ->
					(replace_const_param_in_param a t) @ (p_aux b u)
				)
			)
	in
	(* !!DEBUG!! *)
	(*print_string "debug remove_const_param_from_format, f = ";
	print_expr f;
	print_string "\n\n";*)
	match f with
	FORMAT(s, p) ->
		let r_l = string_to_regexp_list s
		in
		(* !!DEBUG!! *)
		(*let rec print_expr_list e_l =
			match e_l with
			[] -> ()
			| a::b -> print_string " [["; print_expr a; print_string "]]\n"; print_expr_list b
		in
		print_string ("remove_const_param_from_format, s=[["^s^"]]----p=\n");
		print_expr_list p;
		print_char '\n';*)
		let new_s = str_list_to_str (regexp_list_to_str_list (r_aux r_l p))
		in
		let new_p = p_aux r_l p
		in
		if new_p = [] then
			(* format with no arg => simplified into a string constant *)
			CONST(STRING, STRING_CONST(new_s))
		else
			FORMAT(new_s, new_p)
		(*FORMAT(str_list_to_str (regexp_list_to_str_list (r_aux r_l p)), p_aux r_l p)*)
	| ELINE (file, line, e) ->
			(match e with
			ELINE(_, _, _) ->
				remove_const_param_from_format e
			| _ ->
				remove_const_param_from_format e
			)
	| _ ->
		f





(* replace the type by the spec if the param refers to an op or mode,
the param is dropped if it is of a simple type *)
let rec string_typ_list_to_string_spec_list l =
	match l with
	[] ->
		[]
	| a::b ->
		(match a with
		(name, TYPE_ID(t)) ->
			(name, get_symbol t)::(string_typ_list_to_string_spec_list b)
		| _ ->
			string_typ_list_to_string_spec_list b
		)


let rec instantiate_in_stat sta param_list =
	match param_list with
	[] ->
		sta
	| (name, TYPE_ID(t))::q ->
		instantiate_in_stat (substitute_in_stat name (prefix_name_of_params_in_spec (get_symbol t) name) sta) q
	| (name, TYPE_EXPR(e))::q ->
		instantiate_in_stat sta q

let rec instantiate_in_expr ex param_list =
	let rec aux e p_l =
		match p_l with
		[] ->
			e
		| (name, TYPE_ID(t))::q ->
			(* !!DEBUG!! *)
			(*print_string "===call subs_in_expr, name = ["; print_string name;
			print_string "]\nparam = ["; print_param (name, TYPE_ID(t));
			print_string "]\ne = ["; print_expr e;
			print_string "]\nspec = ["; print_spec (prefix_name_of_params_in_spec (get_symbol t) name);
			print_string "]\n===\n";*)
			aux (substitute_in_expr name (prefix_name_of_params_in_spec (get_symbol t) name) e) q
		| (name, TYPE_EXPR(ty))::q ->
			aux e q
	in
	match ex with
	(*FORMAT(_, _) ->*)
		(* change_format_attr ex param_list *)
	| ELINE(a, b, e) ->
		instantiate_in_expr e param_list
	| _ ->
	(* !!DEBUG!! *)
	(*print_string "inst_in_expr , expr = [";
	print_expr ex;
	print_string "]\nparams = [";
	print_param_list param_list;
	print_string "]\n"; flush stdout;*)
		aux ex param_list


let instantiate_param param =
	let rec aux p =
		match p with
		(name, TYPE_ID(typeid))::q ->
			(match get_symbol typeid with
			OR_OP(_, str_l) ->
				(List.flatten (List.map (fun x -> aux [(name, TYPE_ID(x))]) str_l)) @ (aux q)
			| OR_MODE(_, str_l) ->
				(List.flatten (List.map (fun x -> aux [(name, TYPE_ID(x))]) str_l)) @ (aux q)
			| AND_OP(_, _, _) ->
				p @ (aux q)
			| AND_MODE(_, _, _, _) ->
				p @ (aux q)
			| _ ->
				p @ (aux q) (* will this happen? *)
			)
		| [] ->
			[]
		| _ ->
			p
	in
	match param with
	(name, TYPE_EXPR(te)) ->
		[param]
	| (_ , _) ->
		aux [param]


let rec cross_prod l1 l2 =
	match l1 with
	[] ->
		[]
	| a::b ->
		if l2=[] then
			[]
		else
			(List.map (fun x -> [a;x]) l2) @ (cross_prod b l2)

let rec list_and_list_list_prod l ll =
	match l with
	[] ->
		[]
	| a::b ->
		if ll=[] then
			[]
		else
			(List.map (fun x -> a::x) ll) @ (list_and_list_list_prod b ll)

let list_prod p_ll =
	let rec expand l =
		match l with
		[] ->
			[]
		| a::b ->
			[a]::(expand b)
	in
	match p_ll with
	[] ->
		[]
	| a::b ->
		(match b with
		[] ->
			expand a
		| c::d ->
			if List.length d = 0 then
				cross_prod a c
			else
				List.fold_left (fun x y -> list_and_list_list_prod y x) (cross_prod a c) d
		)

let instantiate_param_list p_l =
	let a = List.map instantiate_param p_l
	in
	list_prod a

let instantiate_attr sp a params=
	match a with
	ATTR_EXPR(n, e) ->
		ATTR_EXPR(n, remove_const_param_from_format (simplify_format_expr (instantiate_in_expr e params)))
	| ATTR_STAT(n, s) ->
		ATTR_STAT(n, instantiate_in_stat s params)
	| ATTR_USES ->
		ATTR_USES

(* when instantiating the given param in the given spec
we must add to the spec the attribute of the param which are not in the given spec *)
let add_attr_to_spec sp param =
	let get_attrs s =
		match s with
		AND_OP(_, _, a_l) -> a_l
		| AND_MODE(_, _, _, a_l) -> a_l
		| _ -> []
	in
	let name_of_param p =
		match p with
		(name, TYPE_ID(s)) ->
			name
		| (_, TYPE_EXPR(_)) ->
			failwith "shouldn't happen (instantiate.ml::add_attr_to_spec::name_of_param)"
	in
	let spec_of_param p =
	(* !!DEBUG!! *)
	(*print_string "add_attr_to_sepc::spec_of_param, param = ["; print_param p; print_string "]\n";
	print_string "spec = ["; print_spec sp; print_string "]\n\n";
	flush stdout;*)
		match p with
		(name, TYPE_ID(s)) ->
			get_symbol s
		| (_, TYPE_EXPR(_)) ->
			failwith "shouldn't happen (instantiate.ml::add_attr_to_spec::spec_of_param)"
	in
	(* prefix the name of the attr and all the recursive calls to itself (EVAL(name) => EVAL(pfx ^ name) *)
	let prefix_recursive_attr a pfx =
		let rec aux st name =
			match st with
			NOP ->
				NOP
			| SEQ(s1, s2) ->
				SEQ(aux s1 name, aux s2 name)
			| EVAL(str) ->
				if (String.compare str name) == 0 then
					EVAL(pfx ^ "_" ^ name)
				else
					EVAL(str)
			| EVALIND(v, attr_name) ->
				EVALIND(v, attr_name)
			| SET(l, e) ->
				SET(l, e)
			| CANON_STAT(str, e_l) ->
				CANON_STAT(str, e_l)
			| ERROR(str) ->
				ERROR(str)
			| IF_STAT(e, s1, s2) ->
				IF_STAT(e, aux s1 name, aux s2 name)
			| SWITCH_STAT(e, es_l, s) ->
				SWITCH_STAT(e, List.map (fun (x,y) -> (x, aux s name)) es_l, aux s name)
			| SETSPE(l, e) ->
				SETSPE(l, e)
			| LINE(str, n, s) ->
				LINE(str, n, aux s name)
			| INLINE _ ->
				st
		in
		match a with
		ATTR_EXPR(n, at) ->
			failwith "shouldn't happen (instantiate.ml::add_attr_to_spec::prefix_recursive_attr::ATTR_EXPR)"
		| ATTR_STAT(n, at) ->
			ATTR_STAT(pfx ^ "_" ^ n, aux at n)
		| ATTR_USES ->
			failwith "shouldn't happen (instantiate.ml::add_attr_to_spec::prefix_recursive_attr::ATTR_USES)"
	in
	let compare_attrs a1 a2 =
		match a1 with
		ATTR_EXPR(n, _) ->
			(match a2 with
			ATTR_EXPR(nn, _) ->
				if (String.compare nn n) == 0 then
					true
				else
					false
			| _ -> false
			)
		| ATTR_STAT(n, _) ->
			(match a2 with
			ATTR_STAT(nn, _) ->
				if (String.compare nn n) == 0 then
					true
				else
					false
			| _ -> false
			)
		| ATTR_USES ->
			if a2 = (ATTR_USES) then
				true
			else
				false
	in
	(* returns the attr in param not present in sp (or present but recursive in param and not in sp, a new fully renamed attr must be produced for sp) *)
	let rec search_in_attrs a_l a_l_param =
		match a_l_param with
		[] -> []
		| a::b ->
			if List.exists (fun x -> compare_attrs a x) a_l then
				match a with
				ATTR_STAT(n, _) ->
					begin
					if is_attr_recursive (spec_of_param param) n then
						(prefix_recursive_attr a (name_of_param param))::(search_in_attrs a_l b)
					else
						search_in_attrs a_l b
					end
				| _ ->
					search_in_attrs a_l b
			else
				a::(search_in_attrs a_l b)
	in
	let attr_spec =
		get_attrs sp
	in
	let attr_param =
		match param with
		(name, TYPE_ID(s)) ->
			get_attrs (prefix_name_of_params_in_spec (get_symbol s) name)
		| (name, TYPE_EXPR(t)) ->
			[]
	in
	(* !!DEBUG!! *)
	let get_attr_name a =
		match a with
		ATTR_EXPR(n, _) ->
			n
		| ATTR_STAT(n, _) ->
			n
		| ATTR_USES ->
			"<ATTR_USES>"
	in
	(* !!DEBUG!! *)
	let rec print_attr_list_name a_l =
		match a_l with
		[] -> ()
		| a::b -> print_string ((get_attr_name a) ^ " "); print_attr_list_name b
	in
	(* !!DEBUG!! *)
	(*print_string "adding attrs\n\tattr_spec = "; print_attr_list_name attr_spec;
	print_string "\n\tattr_param = "; print_attr_list_name attr_param;
	print_string "\n";*)
		match sp with
		AND_OP(name, p_l, a_l) ->
			(match spec_of_param param with
			(* we shouldn't add attributes from a mode to an op spec *)
			AND_MODE(_, _, _, _) ->
				sp
			| _ ->
				AND_OP(name, p_l, a_l@(search_in_attrs attr_spec attr_param))
			)
		| AND_MODE(name, p_l, e, a_l) ->
			AND_MODE(name, p_l, e, a_l@(search_in_attrs attr_spec attr_param))
		| _ -> sp

(* add the attrs present in the params' specs but not in the main spec
to the main spec *)
let rec add_new_attrs sp param_list =
	match param_list with
	[] ->
		sp
	| a::b ->
		(match a with
		(_, TYPE_ID(_)) ->
			add_new_attrs (add_attr_to_spec sp a) b
		| (name, TYPE_EXPR(t)) ->
			add_new_attrs sp b
		)


let instantiate_spec sp param_list =
	let is_type_def_spec sp =
		match sp with
		TYPE(_, _) ->
			true
		| _ ->
			false
	in
	(* replace all types by basic types (replace type definitions) *)
	let simplify_param p =
		match p with
		(str, TYPE_ID(n)) ->
			(* we suppose n can refer only to an OP or MODE, or to a TYPE *)
			let sp = get_symbol n in
			if is_type_def_spec sp then
				(match sp with
				TYPE(_, t_e) ->
					(str, TYPE_EXPR(t_e))
				| _ ->
					p
				)
			else
				p
		| (_, _) ->
			p
	in
	let simplify_param_list p_l =
		List.map simplify_param p_l
	in
	let new_param_list = simplify_param_list param_list
	in
	match sp with
	AND_OP(name, params, attrs) ->
	(*  !!DEBUG!! *)
	(*print_string "################################################\ninstantiate_spec, new params=";
	print_param_list new_param_list;
	print_string "\tspec=\n";
	print_spec sp;
	print_string "################\n";*)
		add_new_attrs (AND_OP(name, replace_param_list new_param_list, List.map (fun x -> instantiate_attr sp x new_param_list) attrs)) new_param_list
	| _ ->
		UNDEF



let instantiate_spec_all_combinations sp =
	let new_param_lists = instantiate_param_list (get_param_of_spec sp)
	in
		List.map (fun x -> instantiate_spec sp x) new_param_lists


let is_instantiable sp =
	let is_param_instantiable p =
		match p with
		(_, TYPE_ID(t)) ->
			true
		| (_, TYPE_EXPR(e)) ->
			false
	in
	let test param_list =
		List.exists is_param_instantiable param_list
	in
	match sp with
	AND_OP(_, p_l, _) ->
		test p_l
	| _ -> false


let rec instantiate_spec_list s_l =
	match s_l with
	[] ->
		[]
	| a::b ->
		if is_instantiable a then
			(instantiate_spec_all_combinations a)@(instantiate_spec_list b)
		else
			a::(instantiate_spec_list b)




(* this function instantiate all possible instructions given by the spec name in the hashtable *)
let instantiate_instructions name =
	(* some AND_OP specs with empty attrs and no params are output, remove them
	TODO : see where they come from, this patch is awful.
	here, we assume we have an empty spec as soon as the syntax or the image is void *)
	let is_void_attr a =
		match a with
		ATTR_EXPR(n, e) ->
			(match e with
			ELINE(_, _, ee) ->
				if (String.compare n "syntax") == 0 && ee = NONE then
					true
				else
					if (String.compare n "image") == 0 && ee = NONE then
						true
					else
						false
			| _ ->
				false
			)
		| _ ->
			false
	in
	let is_void_spec sp =
		match sp with
		AND_OP(_, _, attrs) ->
			List.exists is_void_attr attrs
		| _ ->
			false
	in
	let rec clean_attr a =
		match a with
		ATTR_EXPR(n, e) ->
			(match e with
			ELINE(f, l, ee) ->
				(match ee with
				ELINE(_, _, eee) ->
					clean_attr (ATTR_EXPR(n, eee))
				| _ ->
					ATTR_EXPR(n, ee)
				)
			| _ ->
				a
			)
		| _ ->
			a
	in
	(* a try to fix over imbrication of ELINE *)
	let rec clean_eline s =
		match s with
		AND_OP(n, st_l, al) ->
			AND_OP(n, st_l, List.map clean_attr al)
		| _ ->
			s
	in
	let rec clean_instructions s_l =
		match s_l with
		[] ->
			[]
		| h::t ->
			if is_void_spec h then
				clean_instructions t
			else
				(clean_eline h)::(clean_instructions t)
	in
	let rec aux s_l =
		if List.exists is_instantiable s_l then
			aux (instantiate_spec_list s_l)
		else
			s_l
	in
	clean_instructions (aux [get_symbol name])


(* a few testing functions *)


let test_instant_spec name =
	let rec print_spec_list l =
		match l with
		[] ->
			()
		| a::b ->
			begin
			print_spec a;
			print_string "\n";
			print_spec_list b
			end
	in
	print_spec_list (instantiate_instructions name)

let test_instant_param p =
	let rec print_param_list_list l =
	begin
		match l with
		[] ->
			()
		| h::q ->
			begin
			print_string "[";
			print_param_list h;
			print_string "]";
			print_param_list_list q
			end
	end
	in
	print_param_list_list (instantiate_param_list [("z",TYPE_EXPR(CARD(5))); ("x",TYPE_ID("_A")); ("y",TYPE_ID("_D"))])

