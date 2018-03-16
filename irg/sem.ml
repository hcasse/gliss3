(*
 * GLISS2 -- semantics check
 * Copyright (c) 2011, IRIT - UPS <casse@irit.fr>
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

(** This module is in charge of semantics verification of IRG code.
	Because of the structure of SimNML, type checking and symbol verification
	is performed is two phases: at parsing time and at instantiation time.
	Missing symbols or types in first phase can be ignored but leads to errors
	in the second phase.

	In addition, this module provide facilities to perform constant expression
	evaluation.

	{2 IRG Building}

	This functions are used to build IRG structures from parsing results. In addition,
	they perform semantics analysis.
	- {!build_canonical_expr}
	- {!build_canonical_stat}
	- {!get_add_sub}
	- {!get_all_ref}
	- {!get_bin}
	- {!get_binop}
	- {!get_compare}
	- {!get_logic}
	- {!get_mult_div_mod}
	- {!get_shift}
	- {!get_unop}
	- {!make_access_loc}
	- {!make_bitfield}
	- {!make_coerce}
	- {!make_concat_loc}
	- {!make_if_expr}
	- {!make_set}
	- {!make_switch_expr}
	- {!num_auto_coerce}
	- {!to_bool}
	- {!to_card}
	- {!to_cond}

	It includes also useful constants:
	- {!false_const}
	- {!true_const}

	{2 Type Checking}

	Some functions are used perform type checking on different structures:
	- {!build_format}
	- {!check_alias}
	- {!check_attr_inst}
	- {!check_constant_type}
	- {!check_expr_inst}
	- {!check_if_expr}
	- {!check_image}
	- {!check_loc_inst}
	- {!check_param_exists}
	- {!check_params}
	- {!check_set_stat}
	- {!check_spec_inst}
	- {!check_stat_inst}
	- {!check_switch_expr}
	- {!check_unop}
	- {!coerce_to_float} ensure coercion of expression to float
	- {!coerce_to_int} ensure coercion of expression to integer
	- {!coerce_to_string} ensure coercion of expression to string
	- {!get_expr_from_type} to get type expression from a type description (resolve symbolic type).
	- {!get_field_type}
	- {!get_length_from_expr}
	- {!get_loc_ref_type}
	- {!get_loc_type}
	- {!get_type_expr}
	- {!get_type_ident}
	- {!get_type_length}
	- {!interval_of}
	- {!is_IEEE754_float}
	- {!is_loc_mode}
	- {!is_loc_spe}
	- {!is_location}
	- {!raise_type_error_two_operand}
	- {!test_canonical}
	- {!test_data}
	- {!type_from_id}
	- {!type_of_field}

	{2 Evaluation Functions}
	This function allows to statically evaluates expressions:
	- {!eval_binop}
	- {!eval_binop_card}
	- {!eval_binop_fixed}
	- {!eval_binop_string}
	- {!eval_coerce}
	- {!eval_const}
	- {!eval_unop}
	- {!is_true}
	- {!rotate_left}
	- {!rotate_right}
	- {!select}
	- {!to_bool}
	- {!to_int}
	- {!to_int32}
	- {!to_string}

	{2 Utility Functions}

	- {!attr_int} obtain an integer from an expression attribute.
	- {!warn} display a warning.

	{2 Functions that should move}
	- {!change_string_dependences} to iter module.
	- {!get_data_expr_attr} to irg module
	- {!image_escape_size} to iter module
	- {!split_image} to irg module
	- {!split_syntax} to irg module
	*)

open Irg
open Printf

(** Display a warning at the current source line.
	@param f	Function to display warning. *)
let warn f =
	Printf.fprintf stderr "WARNING:%s:%d: " !(Lexer.file) !(Lexer.line);
	f stderr


(** False value. *)
let false_const = CARD_CONST Int32.zero

(** True value. *)
let true_const = CARD_CONST Int32.one

(** Set to true to activate compatibility GLISS1. It includes mainly:
	- Parameter are assignable. *)
let gliss1_compat = ref true


(** Return the minimum number of bits to represent value n.
	@param n	Considered value (n must be positive).
	@return		Number of required value. *)
let bits_for n =
	int_of_float (ceil ((log (float (Int32.to_int n))) /. (log 2.)))


(** Give the bit length of a type expression
	@param t		the type expression of which we want the size
	@return 		the bit-length of the expression (as an iteger)
	@raise Failure	this exception is raised when it is not possible
					to determine the length (for expressions of type NO_TYPE,
					STRING or UNKNOW_TYPE) *)
let get_type_length t =
	match t with
	| BOOL -> 1
	| INT n -> n
	| CARD n -> n
	| FIX (n,m) -> n + m
	| FLOAT (n,m) -> n + m
	| ENUM l ->
		let i = List.length l in
		int_of_float (ceil ((log (float i)) /. (log 2.)))
	| RANGE (l, u) ->
		if (Int32.compare l Int32.zero) >= 0 then bits_for u else
		let l = Int32.abs l in
		if (Int32.compare l u) < 0 then (bits_for u) + 1 else
		let s = bits_for l in
		if l = (Int32.shift_left Int32.one (s - 1)) then s else s + 1
	| NO_TYPE
	| STRING
	| ANY_TYPE ->
		failwith "sem: length unknown"


(** Convert from OCAML boolean to SimNML boolean.
	@param v	OCAML boolean.cstcst
	@return		SimNML boolean. *)
let to_bool v = if v then true_const else false_const


(** Convert constant to int32.
	@param c		Constant to convert.
	@return			Matching int32 value.
	@raise SemError	If the constant is not convertible. *)
let to_int32 c =
	match c with
	  CARD_CONST v -> v
	| _ -> pre_error "should evaluate to an int."


(** Convert a constant to int64.
	@param c		Constant to convert.
	@return			Constant in int64.
	@raise PreError	If the conversion cannot be done. *)
let to_int64 c =
	match c with
	| CARD_CONST n		-> Int64.of_int32 n
	| CARD_CONST_64 n	-> n
	| _ 				-> pre_error "cannot be converted to 64-bits integer"


(** Convert an expression to a string.
	@param e		Expression to convert.
	@return			Converted to string.
	@raise SemError	If the conversion cannot be done. *)
let rec to_string e =
	match e with
	| Irg.CONST (_, Irg.STRING_CONST (s)) -> s
	| Irg.ELINE (_, _, e) -> to_string e
	| _ -> pre_error "should evaluate to a string"

(** Convert constant to int.
	@param c		Constant to convert.
	@return			Matching int value.
	@raise SemError	If the constant is not convertible. *)
let to_int c =
	Int32.to_int (to_int32 c)


(** Mask 32-bits integer on n bits.
	@param i	Integer to mask.
	@param n	Number of bits of the mask.
	@return		n lowest bits of i. *)
let mask32 i n =
	Int32.logand i (Int32.pred (Int32.shift_left Int32.one n))


(** Mask 64-bits integer on n bits.
	@param i	Integer to mask.
	@param n	Number of bits of the mask.
	@return		n lowest bits of i. *)let mask64 i n =
	Int64.logand i (Int64.pred (Int64.shift_left Int64.one n))


(** Mask the given integer on n bits.
	@param i	Integer to mask.
	@param n	Number of bits to keep.
	@return		Constant with lowest n bits of i. *) 
let mask i n =
	match i with
	| CARD_CONST i 		when n < 32 	-> CARD_CONST (mask32 i n)
	| CARD_CONST _						-> i
	| CARD_CONST_64 i	when n < 64 	-> CARD_CONST_64 (mask64 i n)
	| CARD_CONST_64 _					-> i 
	| _									-> failwith "mask: unsupported type to mask"


(** Test if a constant is true.
	@param c	Constant to test.
	@return		True if constant is true. *)
let is_true c =
	match c with
	  NULL -> false
	| CARD_CONST v -> v <> Int32.zero
	| CARD_CONST_64 v-> v <> Int64.zero
	| STRING_CONST(v) -> v <> ""
	| FIXED_CONST v -> v <> 0.0
	| CANON _ -> false


(** Rotate an int32 to the left.
	@param x	Value to rotate.
	@param y	Amount of rotation.
	@return		Result of the rotation.*)
let rotate_left x y =
	let s = Int32.to_int (Int32.logand y (Int32.of_int 0x1f)) in
	Int32.logor (Int32.shift_left x s) (Int32.shift_right x (32 - s))


(** Rotate an int32 to the right.
	@param x	Value to rotate.
	@param y	Amount of rotation.
	@return		Result of the rotation.*)
let rotate_right x y =
	let s = Int32.to_int (Int32.logand y (Int32.of_int 0x1f)) in
	Int32.logor (Int32.shift_right x s) (Int32.shift_left x (32 - s))


(** Evaluate an unary operator.
	@param op	Unary operator.
	@param c	Constant to apply the operator to.
	@return		Result of the operation. *)
let rec eval_unop op (_, c) =
	match (op, c) with
	  (NOT, _) ->
	  	CARD_CONST (if (is_true c) then Int32.zero else Int32.one)
	| (BIN_NOT, CARD_CONST v) ->
		CARD_CONST (Int32.lognot v)
	| (NEG, CARD_CONST v) ->
		CARD_CONST (Int32.neg v)
	| (NEG, FIXED_CONST v) ->
		FIXED_CONST (-. v)
	| _ ->
		pre_error (sprintf "e. bad type operand for '%s'" (string_of_unop op))

(** Evaluate a concatenation.
	@param t1	Type of operand 1.
	@param v1	Value of operand 1.
	@param t2	Type of operand 2.
	@param v2	Value of operand 2.
	@return		Result of concatenation. *)
let eval_concat t1 v1 t2 v2 =
	let s2 = get_type_length t2 in
	let r = Int32.logor (Int32.shift_left v1 s2) v2 in
	r


(** Evaluate a binary operation.
	@param op	Operation.
	@param v1	First operand.
	@param v2	Second operand.
	@return 	Result. *)
let eval_binop_card op (t1, v1) (t2, v2) =
	match op with
	  ADD	 	-> CARD_CONST (Int32.add v1 v2)
	| SUB 		-> CARD_CONST (Int32.sub v1 v2)
	| MUL 		-> CARD_CONST (Int32.mul v1 v2)
	| DIV		-> CARD_CONST (Int32.div v1 v2)
	| MOD		-> CARD_CONST (Int32.rem v1 v2)
	| EXP		-> CARD_CONST (Int32.of_float ((Int32.to_float v1) ** (Int32.to_float v2)))
	| LSHIFT	-> CARD_CONST (Int32.shift_left v1 (Int32.to_int v2))
	| RSHIFT	-> CARD_CONST (Int32.shift_right v1 (Int32.to_int v2))
	| LROTATE	-> CARD_CONST (rotate_left v1 v2)
	| RROTATE	-> CARD_CONST (rotate_right v1 v2)
	| LT		-> to_bool (v1 < v2)
	| GT		-> to_bool (v1 > v2)
	| LE		-> to_bool (v1 <= v2)
	| GE		-> to_bool (v1 >= v2)
	| EQ		-> to_bool (v1 = v2)
	| NE		-> to_bool (v1 <> v2)
	| AND		-> if (v1 <> Int32.zero) && (v2 <> Int32.zero) then true_const else false_const
	| OR		-> if (v1 <> Int32.zero) || (v2 <> Int32.zero) then true_const else false_const
	| BIN_AND	-> CARD_CONST (Int32.logand v1 v2)
	| BIN_OR	-> CARD_CONST (Int32.logor v1 v2)
	| BIN_XOR	-> CARD_CONST (Int32.logxor v1 v2)
	| CONCAT	-> CARD_CONST (eval_concat t1 v1 t2 v2)


(** Evaluate a fixed binary operation.
	@param op	Operation.
	@param v1	First operand.
	@param v2	Second operand.
	@return 	Result. *)
let eval_binop_fixed op (_, v1) (_, v2) =
	match op with
	  ADD		-> FIXED_CONST (v1 +. v2)
	| SUB		-> FIXED_CONST (v1 -. v2)
	| MUL		-> FIXED_CONST (v1 *. v2)
	| DIV		-> FIXED_CONST (v1 /. v2)
	| EXP		-> FIXED_CONST (v1 ** v2)
	| LT		-> to_bool (v1 < v2)
	| GT		-> to_bool (v1 > v2)
	| LE		-> to_bool (v1 <= v2)
	| GE		-> to_bool (v1 >= v2)
	| EQ		-> to_bool (v1 = v2)
	| NE		-> to_bool (v1 <> v2)
	| AND		-> if v1 <> 0. && v2 <> 0. then true_const else false_const
	| OR		-> if v1 <> 0. || v2 <> 0. then true_const else false_const
	| _ 		-> pre_error (sprintf "c. bad type operand for '%s'" (string_of_binop op))


(** Evaluate a string binary operation.
	@param op	Operation.
	@param v1	First operand.
	@param v2	Second operand.
	@return		Result. *)
let eval_binop_string op (_, v1) (_, v2) =
	match op with
	  LT 		-> to_bool (v1 < v2)
	| GT		-> to_bool (v1 > v2)
	| LE		-> to_bool (v1 <= v2)
	| GE		-> to_bool (v1 >= v2)
	| EQ		-> to_bool (v1 = v2)
	| NE		-> to_bool (v1 <> v2)
	| CONCAT	-> STRING_CONST(v1 ^ v2)
	| _ 		-> pre_error (sprintf "b. bad type operand for '%s'" (string_of_binop op))



(** Evaluate a binary operator.
	@param op	Binary operator.
	@param a1	First operand.
	@param a2	Second operand.
	@return		Result of the operation. *)
let eval_binop op (t1, c1) (t2, c2) =
	match (c1, c2) with
  	| (Irg.CARD_CONST v1, Irg.CARD_CONST v2) 		-> eval_binop_card op (t1, v1) (t2, v2)
	| (Irg.FIXED_CONST v1, Irg.CARD_CONST v2)		-> eval_binop_fixed op (t1, v1) (t1, Int32.to_float v2)
	| (Irg.CARD_CONST v1, Irg.FIXED_CONST v2) 		-> eval_binop_fixed op (t2, Int32.to_float v1) (t2, v2)
	| (Irg.FIXED_CONST v1, Irg.FIXED_CONST v2) 		-> eval_binop_fixed op (t1, v1) (t2, v2)
	| (Irg.STRING_CONST(v1), Irg.STRING_CONST(v2))	-> eval_binop_string op (t1, v1) (t2, v2)
	| _												-> pre_error (sprintf "a. bad type operand for '%s'" (string_of_binop op))


(** Perform the coercition function on the given value.
	@param t		Type to coerce to.
	@param v		Value to coerce.
	@return			Result of coercion.
	@raise SemError	If the coercion is not supported. *)
let eval_coerce t (_, v) =
	match t, v with
	| _, NULL 									-> NULL
	| BOOL, CARD_CONST i 						-> CARD_CONST (if i = Int32.zero then Int32.zero else Int32.one)
	| BOOL, CARD_CONST_64 i						-> CARD_CONST (if i = Int64.zero then Int32.zero else Int32.one)

	| INT n, CARD_CONST i 		when n < 32 	-> CARD_CONST (mask32 i n)
	| INT n, CARD_CONST i 		when n = 32 	-> v
	| INT n, CARD_CONST i 		when n < 64		-> CARD_CONST_64 (Int64.of_int32 i)
	| INT n, CARD_CONST i 		when n = 64 	-> CARD_CONST_64 (Int64.of_int32 i)
	| INT n, CARD_CONST_64 i	when n < 32 	-> CARD_CONST (mask32 (Int64.to_int32 i) n)
	| INT n, CARD_CONST_64 i 	when n = 32 	-> CARD_CONST (Int64.to_int32 i)
	| INT n, CARD_CONST_64 i 	when n < 64 	-> CARD_CONST_64 (mask64 i n)
	| INT n, CARD_CONST_64 i 	when n = 64 	-> v
	| INT n, FIXED_CONST i 		when n <= 32 	-> CARD_CONST (Int32.of_float i)
	| INT n, FIXED_CONST i 		when n <= 64 	-> CARD_CONST_64 (Int64.of_float i)

	| CARD n, CARD_CONST i 		when n < 32 	-> CARD_CONST (mask32 i n)
	| CARD n, CARD_CONST i 		when n = 32 	-> v
	| CARD n, CARD_CONST i 		when n < 64 	-> CARD_CONST_64 (mask64 (Int64.of_int32 i) n)
	| CARD n, CARD_CONST i 		when n = 64 	-> CARD_CONST_64 (Int64.of_int32 i)
	| CARD n, CARD_CONST_64 i	when n < 32 	-> CARD_CONST (mask32 (Int64.to_int32 i) n)
	| CARD n, CARD_CONST_64 i 	when n = 32 	-> CARD_CONST (Int64.to_int32 i)
	| CARD n, CARD_CONST_64 i 	when n < 64 	-> CARD_CONST_64 (mask64 i n)
	| CARD n, CARD_CONST_64 i 	when n = 64 	-> v
	| CARD n, FIXED_CONST i 	when n <= 32 	-> CARD_CONST (Int32.of_float (abs_float i))
	| CARD n, FIXED_CONST i 	when n <= 64 	-> CARD_CONST_64 (Int64.of_float (abs_float i))

	| FLOAT _, CARD_CONST i -> FIXED_CONST (Int32.to_float i)
	| FLOAT _, CARD_CONST_64 i -> FIXED_CONST (Int64.to_float i)

	| _ -> pre_error "unsupported constant coerction"


(**
 * Evaluate a constant bitfield operation.
 * @param v		Value to apply bitfield on.
 * @param l		Lower bit.
 * @param u		Upper bit.
 * @return		Result of bitfield application. *)
let eval_bitfield v u l =
	let v = to_int32 v in
	let u = Int32.to_int (to_int32 u) in
	let l = Int32.to_int (to_int32 l) in
	if l <= u
	then	(* without inversion *)
		(	Irg.CARD (u - l + 1),
			Irg.CARD_CONST
				(if (u - l + 1) = 32 then v else
				Int32.logand (Int32.shift_right v l) (Int32.pred (Int32.shift_left Int32.one (u - l + 1)))))
	else	(* with inversion *)
		pre_error "unsupported bitfield"


(** Perform the expression switch.
	@param c		Condition.
	@param cs		Cases of the switch.
	@param d		Default value. *)
let rec select c cs d =
	let ec = eval_typed_const c in
	match cs with
	| [] -> eval_typed_const d
	| (cp, e)::_ when (eval_typed_const cp) = ec -> eval_typed_const e
	| _::t -> select c t d


(** Evaluate an expression to constant.
	@param expr			Expression to evaluate.
	@return				(type, constant)
	@raise SemError		If the expression is not constant. *)
and eval_typed_const expr =
	match expr with
	| CONST (t,cst) ->
		(t, cst)
	| UNOP (t, op, e) ->
		(t, eval_unop op (eval_typed_const e))
	| BINOP (CARD(n), op, e1, e2) when n <> 32 && n <> 64 ->
		(CARD(n), mask (eval_binop op (eval_typed_const e1) (eval_typed_const e2)) n)
	| BINOP (t, op, e1, e2) ->
		(t, eval_binop op (eval_typed_const e1) (eval_typed_const e2))
	| IF_EXPR(tt, c, t, e) ->
		if is_true (snd (eval_typed_const c)) then eval_typed_const t else eval_typed_const e
	| SWITCH_EXPR (tt, c, cases, def) ->
		select c cases def
	| REF (_, id) ->
		(match get_symbol id with
		| LET (_, t, cst) -> (t, cst)
		| _ -> pre_error (id ^ " is not a constant symbol"))
	| BITFIELD (t, e, u, l) ->
		eval_bitfield (snd (eval_typed_const e)) (snd (eval_typed_const u)) (snd (eval_typed_const l))
	| ELINE (_, _, e) ->
		eval_typed_const e
	| COERCE (t, e) ->
		(t, eval_coerce t (eval_typed_const e))
	| _ ->
		pre_error "this expression should be constant"


(** Evaluate an expression to constant.
	@param expr			Expression to evaluate.
	@return				Constant result of evaluation.
	@raise SemError		If the expression is not constant. *)
let eval_const expr =
	snd (eval_typed_const expr)


(** Find a type by its identifier.
	@param id		Identifier of the looked type.
	@return			Type matching the identifier.
	@raise SemError	If the identifier does not exists or if the named item is
not a type. *)
let type_from_id id =
	try
		match StringHashtbl.find syms id with
		  TYPE (_, te) -> te
		| _ ->	pre_error (sprintf "%s does not named a type" id)
	with Not_found ->
		pre_error (sprintf "unknown identifier \"%s\"" id)


(** Check the matching of a type and a constant.
	@param t	Type to check.
	@param c	Constant to check.
	@return		True if they match, false else. *)
let check_constant_type t c =
	match (t, c) with
	  (BOOL, CARD_CONST v) ->
		(v = Int32.zero) || (v = Int32.one)
	| (CARD _, CARD_CONST _)
	| (INT _, CARD_CONST _)
	| (FIX _, FIXED_CONST _)
	| (FLOAT _, FIXED_CONST _) ->
		true
	| (RANGE (l, u), CARD_CONST v) ->
		((Int32.compare v l) >= 0) && ((Int32.compare v u) <= 0)
	| _ ->
		false


(** Test if a float number respects the IEE754 specification
    @param f          a nml float
    @return   true if the float is an IEEE754 float, false otherwise
    bit sign is first bit of the mantisse *)
let is_IEEE754_float f = match f with
	(FLOAT(m,e)) ->(match(m+e) with
			 32 -> (e =8)&&(m=24)
			| 64 -> (e = 11)&&(m =53)
			| 80 -> (e = 15)&&(m = 65)
			| _  -> pre_error "float number doesn't follow IEEE754 specification ")
	| _ -> pre_error "function expect float number but parameter is not "


(** Get the type associated with an identifiant.
	@param id	The identifiant to type.
	@return A type.
	@raise SemError if the keyword is not defined. *)
let rec get_type_ident id=
	let symb= get_symbol id
	in

	if symb=UNDEF
	then
		pre_error (sprintf "The keyword \"%s\" not defined" id)
	else

	match symb with
	| LET (_, _, c)->
			(match c with
			| NULL				-> NO_TYPE
			| CARD_CONST _		->CARD 32
			| CARD_CONST_64 _	->CARD 64
			| STRING_CONST _	-> STRING
			| FIXED_CONST _		-> FLOAT (24,8)
			| CANON _ 			-> ANY_TYPE)
	| TYPE (_,t)->(match t with
			ENUM l->(let i = List.length l in
				CARD (int_of_float (ceil ((log (float i)) /. (log 2.)))))
			|_->t)
	| MEM (_,_,t,_)->t
	| REG (_,_,t,_)->t
	| VAR (_,_,t, _)->t
	| AND_MODE (_,l,e,_)->	(
				 param_stack l;
				 let t= get_type_expr e
				 in
				 param_unstack l;
				 t
				)

	| OR_MODE _ -> ANY_TYPE

	| PARAM (n,t)->( rm_symbol n;
			let type_res=(
				match t with
				 TYPE_ID idb->get_type_ident idb
				|TYPE_EXPR tb->tb)	(* ??? *)
			in
			add_param (n,t);
			type_res)
	| ATTR (ATTR_EXPR (_, expr)) -> get_type_expr expr
	| ATTR _ -> NO_TYPE
	| _ ->NO_TYPE

(** Get the type of an expression
	@param exp 	Expression  to evaluate
	@return		the type of the parameter exp *)
and get_type_expr exp =
	match exp with
	| NONE								-> NO_TYPE
	| COERCE(t, _)						-> t
	| FORMAT (_, _)						-> STRING
	| CANON_EXPR (t, _, _)				-> t
	| REF (t, _)						-> t
	| FIELDOF (t, _, _)					-> t
	| ITEMOF (t, _, _)					-> t
	| BITFIELD (FLOAT(n, m), _, _, _) 	-> CARD(n + m)
	| BITFIELD (t, _, _, _) 			-> t
	| UNOP (t, _, _) 					-> t
	| BINOP (t, _, _, _)				-> t
	| IF_EXPR (t, _, _, _)				-> t
	| SWITCH_EXPR (t, _, _, _)			-> t
	| CONST (t,_)						-> t
	| ELINE (_, _, e) 					-> get_type_expr e
	| CAST(t, _) 						-> t


(** Generator for getting unique name of local variables. *)
let local_uniq = ref 0

(** Map for local renaming *)
let local_map : string StringHashtbl.t = StringHashtbl.create 211

(** If the given identifier designs a local variable, return its
	unique name. Else return the identifier as is. *)
let unalias_local id =
	try
		StringHashtbl.find local_map id
	with Not_found ->
		id

(** Reset local management. *)
let reset_local _ =
	clean_local ();
	StringHashtbl.clear local_map


(** Give the bit lenght of an expression
	@param e	the expression of wich we want the size
	@return 	the bit-length of the expression (as an iteger)
	@raise Failure	this exception is raised when it is not possible to determine the length (for expressions of type NO_TYPE, STRING or UNKNOW_TYPE)
*)
let get_length_from_expr e=
	get_type_length (get_type_expr e)


(** Check an unary operation.
	@param e			Operand.
	@param uop			Unary operation.
	@return				(result type, actual operand).
	@raise	Irg.Error	In case of error. *)
let check_unop e uop =
	let t = get_type_expr e in

	match (uop, t) with
	| (_, ANY_TYPE) -> (ANY_TYPE, e)
	| (NOT, BOOL)
	| (NOT, CARD _)
	| (NOT, INT _)
	| (NOT, FIX _)
	| (NOT, FLOAT _)
	| (NEG, CARD _)
	| (NEG, INT _)
	| (NEG, FIX _)
	| (NEG, FLOAT _)
	| (BIN_NOT, CARD _)
	| (BIN_NOT, INT _)
	| (BIN_NOT, FIX _)
	| (BIN_NOT, FLOAT _) -> (t, e)
	| _ ->
		error_with_msg [PTEXT "bad operand type "; PTYPE t; PTEXT " for "; PEXPR (UNOP (NO_TYPE, uop, e))]


(** Make a unary operation with a correct type in function of its operand.
	@param e	Operand.
	@param uop	Operation to apply.
	@return		(result type, operand)
	@raise SemErrorWithFun	Raised when the type of the operand is not compatible with the operation
*)
let get_unop e uop =
	let (t, e) = check_unop e uop in
	UNOP (t, uop, e)


(** Build an extended type, either card, or int
	containing the given range.
	@param l	Low value of range.
	@param u	Upper value of range.
	@return		Matching int or card type. *)
let extend_range l u =
	if (Int32.compare l Int32.zero) < 0
	then INT (get_type_length (RANGE (l, u)))
	else CARD (get_type_length (RANGE (l, u)))


(** Build an extended type, either card, or int
	containing the given enumerated values.
	@param l	List of enumerated values.
	@return		Extended int or card type. *)
let extend_enum l =
	extend_range (List.nth l 0) (List.nth l ((List.length l) - 1))


(** Ensure that the given expression is coerced to int (if it could be).
	@param e		Expression to coerce to.
	@param t		Preferred type for coercition.
	@return			(natural coercition, coerced expression to int or NONE (if coercition is impossible)). *)
let coerce_to_int e t =
	match get_type_expr e with
	| NO_TYPE
	| STRING		-> (false, NONE)
	| BOOL
	| FIX _
	| FLOAT _		-> (false, COERCE(t, e))
	| CARD _
	| INT _
	| ANY_TYPE		-> (true, e)
	| RANGE (l, u)	-> (true, COERCE(extend_range l u, e))
	| ENUM l		-> (true, COERCE(extend_enum l, e))


(** Ensure that the given expression is coerced to float (if it could be).
	@param e		Expression to coerce to.
	@param t		Preferred type for coercition.
	@return			(natural coercition, coerced expression to int or NONE (if coercition is impossible)). *)
let coerce_to_float e t =
	match get_type_expr e with
	| NO_TYPE
	| STRING		-> (false, NONE)
	| BOOL
	| INT _
	| CARD _
	| ENUM _
	| RANGE _		-> (false, COERCE(t, e))
	| FLOAT _
	| FIX _
	| ANY_TYPE		-> (true, e)


(** Ensure that the given expression is coerced to string (if it could be).
	@param e		Expression to coerce to.
	@return			(natural coercition, coerced expression to int or NONE (if coercition is impossible)). *)
let coerce_to_string e =
	match get_type_expr e with
	| NO_TYPE		-> (false, NONE)
	| STRING
	| ANY_TYPE		-> (true, e)
	| BOOL
	| INT _
	| CARD _
	| ENUM _
	| RANGE _
	| FLOAT _
	| FIX _			-> (false, NONE)


(** Perform automatic-coercition between numeric types, coercing to bigger type.
	If coercition is not possible, result type is NO_TYPE.
	@param e1	First expression.
	@param e2	Second expression.
	@return		(result type, coerced first expression, coerced second expression) *)
let rec num_auto_coerce e1 e2 =
	let t1 = get_type_expr e1 in
	let t2 = get_type_expr e2 in

	match t1, t2 with
	(* any type support *)
	| ANY_TYPE, _
	| _, ANY_TYPE					-> (ANY_TYPE, e1, e2)

	(* BOOL base *)
	| BOOL, INT _
	| BOOL, CARD _
	| BOOL, FLOAT _
	| BOOL, RANGE _					-> (t2, COERCE(t2, e1), e2)

	(* INT base *)
	| INT _, BOOL 					-> (t1, e1, COERCE(t1, e2))
	| INT n1, INT n2 when n1 > n2 	-> (t1, e1, COERCE(t1, e2))
	| INT n1, INT n2				-> (t2, COERCE(t2, e1), e2)
	| INT n1, CARD n2 when n1 >= n2	-> (t1, e1, COERCE(t1, e2))
	| INT n1, CARD n2				-> (INT(n2), COERCE(INT(n2), e1), COERCE(INT(n2), e2))
	| INT _, FLOAT _				-> (t2, COERCE(t2, e1), e2)
	| INT _, RANGE (l, u)			-> num_auto_coerce e1 (COERCE(extend_range l u, e2))

	(* CARD base *)
	| CARD _, BOOL 					-> (t1, e1, COERCE(t1, e2))
	| CARD n1, INT n2 when n1 <= n2 -> (t2, COERCE(t2, e1), e2)
	| CARD n1, INT n2			 	-> (INT(n1), COERCE(INT(n1), e1), COERCE(INT(n1), e2))
	| CARD n1, CARD n2 when n1 < n2 -> (t2, COERCE(t2, e1), e2)
	| CARD n1, CARD n2				-> (t1, e1, COERCE(t1, e2))
	| CARD _, FLOAT _				-> (t2, COERCE(t2, e1), e2)
	| CARD _, RANGE (l, u)			-> num_auto_coerce e1 (COERCE(extend_range l u, e2))

	(* FLOAT base *)
	| FLOAT _, BOOL
	| FLOAT _, INT _
	| FLOAT _, CARD _				-> (t1, e1, COERCE(t1, e2))
	| FLOAT (n1, m1), FLOAT (n2, m2)
		when n1 + m1 > n2 + m2 		-> (t1, e1, COERCE(t1, e2))
	| FLOAT (n1, m1), FLOAT (n2, m2)
	 								-> (t2, COERCE(t2, e1), e2)
	| FLOAT _, RANGE _				-> (t1, e1, COERCE(t1, e2))

	(* range base type *)
	| RANGE(l, u), INT _
	| RANGE(l, u), CARD _
	| RANGE(l, u), FLOAT _			-> num_auto_coerce (COERCE(extend_range l u, e1)) e2
	| RANGE(l, u), RANGE(l', u')	-> num_auto_coerce (COERCE(extend_range l u, e1)) (COERCE(extend_range l' u', e2))

	(* enum type *)
	| ENUM l1, _					-> num_auto_coerce (COERCE (extend_enum l1, e1)) e2
	| _, ENUM l2					-> num_auto_coerce e1 (COERCE (extend_enum l2, e2))

	(* incompatible case *)
	| _								-> 	if t1 = t2 then (t1, e1, e2) else (NO_TYPE, NONE, NONE)


(** Display type error for two-operand operation.
	@param op	Operand display.
	@parma e1	First expression.
	@param e2	Second expression. *)
let error_two_operands op e1 e2 =
	error (output [
		PTEXT "cannot coerce operands for '"; PTEXT op; PTEXT "' with";
		PTEXT "\noperand 1 type:"; PTYPE (get_type_expr e1);
		PTEXT "'\noperand 2 type:"; PTYPE (get_type_expr e2); PLN])


(** Create an add/sub with a correct type in function of its operands.
	This function is used in get_binop.
	@param e1	First operand.
	@param e2	Second operand.
	@param bop	ADD/SUB
	@return	An ADD/SUB expression
	@raise Failure	Raised when the type of the operands are not compatible with the operation *)
let get_add_sub e1 e2 bop =
	let (t, e1, e2) = num_auto_coerce e1 e2 in
	if t <> NO_TYPE then BINOP (t, bop, e1, e2) else
	error_two_operands (string_of_binop bop) e1 e2


(** Create a mult/div/mod with a correct type in function of its operands.
	This function is used in get_binop.
	@param e1	First operand.
	@param e2	Second operand.
	@param bop	MUL/DIV/MOD
	@return	A MUL/DIV/MOD expression
	@raise Failure	Raised when the type of the operands are not compatible with the operation
*)
let get_mult_div_mod e1 e2 bop =
	let (t, e1, e2) = num_auto_coerce e1 e2 in
	if t <> NO_TYPE then BINOP (t, bop, e1, e2) else
	error_two_operands (string_of_binop bop) e1 e2


(** Convert given type to raw card.
	@param e	Expression to convert. *)
let to_card e =
	match get_type_expr e with
	| FLOAT _
	| FIX _ -> CAST(get_type_expr e, e)
	| _ -> e


(** Create a concat with a correct type in function of its operands.
	This function is used in get_binop.
	@param e1	First operand.
	@param e2	Second operand.
	@return	A CONCAT expression
	@raise Failure	Raised when the type of the operands are not compatible with the operation
*)
let rec get_concat e1 e2 =
	(* TODO convert arguments to card *)
	try
		let length = (get_length_from_expr e1) + (get_length_from_expr e2) in
		Irg.BINOP (CARD length, CONCAT, to_card e1, to_card e2)
	with Failure "sem: length unknown" ->
		Irg.BINOP (ANY_TYPE, CONCAT, to_card e1, to_card e2)


(** Check types in comparison.
	@param bop	Operator.
	@param e1	First operand.
	@param e2	Second operand.
	@return		Checked expression. *)
let get_compare bop e1 e2 =
	let (t, e1, e2) = num_auto_coerce e1 e2 in
	if t <> NO_TYPE then BINOP (BOOL, bop, e1, e2) else
	error_two_operands (string_of_binop bop) e1 e2


(** Convert an expression to boolean.
	@param e	Expression to convert.
	@return		Converted expression. *)
let to_bool e =
	match get_type_expr e with
	| ANY_TYPE
	| BOOL -> e
	| _ -> COERCE(BOOL, e)


(** Convert an expression to condition.
	@param e	Expression to convert.
	@return		Converted expression. *)
let to_cond e =
	match get_type_expr e with
	| ANY_TYPE
	| _ -> e


(** Check type for a logic operation.
	@param bop	Operator.
	@param e1	First operand.
	@param e2	Second operand.
	@return		Checked expression. *)
let get_logic bop e1 e2 =
	let t1 = get_type_expr e1
	and t2 = get_type_expr e2 in
	match (t1,t2) with
	| ANY_TYPE, _
	| _, ANY_TYPE ->
		BINOP (ANY_TYPE, bop, e1, e2)
	| _, _ ->
		BINOP (BOOL, bop, to_cond e1, to_cond e2)


(** Check type for a binary operation.
	@param bop	Operator.
	@param e1	First operand.
	@param e2	Second operand.
	@return		Checked expression. *)
let get_bin bop e1 e2 =
	let t1 = get_type_expr e1
	and t2 = get_type_expr e2 in
	match (t1,t2) with
	| ANY_TYPE, _
	| _, ANY_TYPE ->
		BINOP (ANY_TYPE, bop, e1, e2)
	| NO_TYPE, _
	| _, NO_TYPE -> BINOP(NO_TYPE, bop, e1, e2)
	| _, _ ->
		let s1 = get_type_length t1 in
		let s2 = get_type_length t2 in
		let e1, e2 =
			if t1 = t2 then (to_card e1, to_card e2) else
			if t1 < t2 then (COERCE(CARD s2, e1), e2)
			else (to_card e1, COERCE(CARD s1, to_card e2)) in
		BINOP(CARD(max s1 s2), bop, e1, e2)


(** Check type for a shift operations.
	@param bop	Operator.
	@param e1	First operand.
	@param e2	Second operand.
	@return		Checked expression. *)
let get_shift bop e1 e2 =
	(* TODO convert arguments to card *)
	let t1 = get_type_expr e1
	and t2 = get_type_expr e2 in
	match (t1,t2) with
	| ANY_TYPE, _
	| _, ANY_TYPE ->
		BINOP (ANY_TYPE, bop, e1, e2)
	| _, _ ->
		let s = get_type_length t1 in
		BINOP(CARD(s), bop, to_card e1, e2)


(** Create a binary operation with a correct type in function of its operands.
	@param e1	First operand.
	@param e2	Second operand.
	@param bop	Operation to apply.
	@return	A BINOP expression.
	@raise SemErrorWithFun	Raised when the type of the operands is not compatible with the operation
*)
let rec get_binop e1 e2 bop =
	try
		match bop with
 		| ADD
 		| SUB -> get_add_sub e1 e2 bop
		| MUL
		| DIV
		| MOD -> get_mult_div_mod  e1 e2 bop
		| EXP -> BINOP (get_type_expr e1, bop, e1, e2)
		| LSHIFT
		| RSHIFT
		| LROTATE
		| RROTATE -> get_shift bop e1 e2
		| LT
		| GT
		| LE
		| GE
		| EQ
		| NE -> get_compare bop e1 e2
		| AND
		| OR -> get_logic bop e1 e2
		| BIN_AND
		| BIN_OR
		| BIN_XOR -> get_bin bop e1 e2
		| CONCAT-> get_concat e1 e2
	with PreError f ->
		error (output
			[PFUN f;
			PEXPR e1; PTEXT ": "; PTYPE (get_type_expr e1);
			PTEXT " "; PTEXT (string_of_binop bop); PTEXT " ";
			PEXPR e2; PTEXT ": "; PTYPE (get_type_expr e2)])


(** coerce, eventually, the rvalue in a SET or SETSPE statement if needed,
    we deal with int and card
    @param	l	the location of the SET(SPE)
    @param	e	the rvalue expression to be coerced if needed
    @return		the rvalue expression coerced or not
*)
let check_set_stat l e =
	let t2 = get_type_expr e
	in
	let t1 =
		match l with
		LOC_NONE ->
			NO_TYPE
		| LOC_REF(t, _, _, _, _) ->
			t
		| LOC_CONCAT(t, _, _) ->
			t
	in
			(* !!DEBUG!! *)
			(*print_string "check_set_stat\n";
			print_string "\t(loc) "; print_type_expr t1; print_string " : "; print_location l; print_char '\n';
			print_string "\t(exp) "; print_type_expr t2; print_string " : "; print_expr e; print_char '\n';*)
			match t1 with
			INT(n1) ->
				(match t2 with
				INT(n2) ->
					if n1 > n2 then
						begin
						(* !!DEBUG!! *)
						(*Printf.printf "\tcoerce e -> INT(%d)\n" n1;*)
						COERCE(INT(n1), e)
						end
					else
						(* unlike with binops, we can't coerce here *)
						e
				| _ ->
					e
				)
			| CARD(n1) ->
				(match t2 with
				INT(n2) ->
					if n1 > n2 then
						begin
						(* !!DEBUG!! *)
						(*Printf.printf "\tcoerce e -> INT(%d)\n" n1;*)
						COERCE(INT(n1), e)
						end
					else
						e
				| _ ->
					e
				)
			| _ ->
				e


(** Get the interval value of an integer type.
	@param t	Type to get interval of.
	@return		(minimum, maximum) or (0, 0) for non integer type. *)
let interval_of t =
	match t with
	| BOOL -> (0, 1)
	| CARD(n) -> (0, (1 lsl n) - 1)
	| INT(n) -> (-(1 lsl (n - 1)), (1 lsl (n - 1)) - 1)
	| _ -> (0, 0)


(** Check if the given parameters of a switch expression give a valid switch expression.
	It check that all the cases are of the same type than the condition,
	that all the cases and the default return an expression of the same type,
	that all pssibilites are covered by the cases.

	TODO : Allow compatibles types (instead of strictly the same type) to be presents in the conditional part of the cases

	@param test	the condition of the switch.
	@param list_case	the couple list of the cases.
	@param default	the default of the switch.(NONE if no default)
	@return the type of the switch
	@raise SemError	Raised when the parameters are incorrect
*)
let check_switch_expr test list_case default =
	let zero = Int64.zero in
	let one = Int64.one in
	let top n = Int64.sub (Int64.shift_left one n) one in
	let bot n = Int64.neg (Int64.shift_left one n) in
	let shl = Int64.shift_left in
	let of_int32 = Int64.of_int32 in
	let of_int = Int64.of_int in
	let sub = Int64.sub in
	let succ = Int64.succ in
	let in_range lo up x =
		((Int64.compare lo x) <= 0) && ((Int64.compare x up) <= 0) in
	let in_set set x = List.mem x set in
	let t = get_type_expr test in

	let rec check_case isin lst (c, _) =
		let v = to_int64 (eval_const c) in
		if List.mem v lst 
		then pre_error (Printf.sprintf "case value %Ld is used several times" v)
		else if not (isin v) 
		then pre_error (Printf.sprintf "case %Ld is out of range of condition values" v)
		else v::lst in

	let check_cases _ =
		let isin, cnt =
			match t with
			| BOOL
				-> (in_range zero one, succ one)
			| CARD n
				-> (in_range zero (top n), shl one n)
			| INT n
				-> (in_range (bot (n - 1)) (top (n - 1)), shl one n)
			| RANGE (l, u)
				-> (in_range (of_int32 l) (of_int32 u), succ (sub (of_int32 u) (of_int32 l)))
			| ENUM set 
				-> (in_set (List.map of_int32 set), of_int (List.length set))
			| ANY_TYPE
				-> ((fun _ -> true), zero)
			| t
				-> error (output [PTEXT "condition of type "; PTYPE t; PTEXT " cannot be used in a switch"]) in
		let cases = List.fold_left (check_case isin) [] list_case in
		if (default == NONE) && ((of_int (List.length cases)) <> cnt)
		then pre_error "some cases in this functionnal switch are missing!"
		else () in
	
	let check_switch_all_possibilities =
		if (not (default = NONE)) then true
		else
			let min, max = interval_of (get_type_expr test) in
			if (min, max) = (0, 0) then
				pre_error "bad type for 'switch' test: only integer types supported"
			else
				let vals = List.sort compare
					(List.map (fun (case, _) -> to_int (eval_const case)) list_case) in
				let vals = List.map
					(fun v ->
						if v >= min || v <= max then v else
						pre_error (sprintf "%d out of switch bounds" v))
					vals in
				let rec test i l =
					if i > max then true
					else if l = [] || i <> (List.hd l) then
						pre_error (sprintf "uncomplete switch: %d is lacking" i)
					else
						test (i + 1) (List.tl l) in
				test min vals in

	check_cases ();
	if not check_switch_all_possibilities then
		error (output [
				PTEXT "the cases of a functional switch must cover all possibilities or contain a default entry:\n";
				PTEXT "switch type is "; PTYPE (get_type_expr test)])
	else if (get_type_expr default != NO_TYPE)
		then get_type_expr default
		else get_type_expr (snd (List.hd list_case))



(** Check is the given id refer to a valid memory location
	To allow compatibility with older versions, is_loc_mode and is_loc_spe must be used in conjunction with this function
	@param id	the id to check
	@return True if the id is a valid memory location, false otherwise *)
let rec is_location id =

	let rec scan_expr e =
		match e with
		| NONE
		| COERCE _
		| FORMAT _
		| CANON_EXPR _
		| FIELDOF _
		| UNOP _
		| BINOP _
		| IF_EXPR _
		| SWITCH_EXPR _
		| CONST _
		| CAST _
			-> false
		| REF (_, id)
		| ITEMOF (_, id, _)
			-> is_location id
		| BITFIELD(_, e, _, _)
		| ELINE (_, _, e)
			-> scan_expr e

	and scan_mode id =
		match get_symbol id with
		| AND_MODE (_, p, e, a)	-> in_context p a (fun _ -> scan_expr e)
		| OR_MODE (_, l)		-> List.for_all scan_mode l
		| _ -> false

	and scan_param n t =
		match t with
		| TYPE_EXPR _ -> false
		| TYPE_ID idb -> scan_mode idb in (*rm_symbol n; let v = is_location idb in add_param (n,t); v*)

	match Irg.get_symbol id with
	| UNDEF -> true
	| MEM _
	| REG _
	| VAR _ -> true
	| PARAM (n, t)-> scan_param n t
	| LET _
	| TYPE _
	| AND_MODE _
	| OR_MODE _
	| AND_OP _
	| OR_OP _
	| RES _
	| EXN _
	| ATTR _
	| CANON_DEF _ -> false


(** Check is the given id refer to a MODE.
	This is needed for compatibility with some versions of GLISS v1 where assignements in modes where used.
	This function is defined to be used in complement of is_location
	@param id	the id to check
	@return True if the id refer to a MODE false otherwise
*)
let rec is_loc_mode id =
	let sym=Irg.get_symbol id
	and is_location_param id=
		let sym=Irg.get_symbol id
		in
		match sym with
			(AND_MODE(_,_,_,_)|OR_MODE(_,_))->true
			|_->false
	in
	match sym with
	PARAM (n,t)->	(rm_symbol n;
			let value=(match t with
			 TYPE_ID idb-> is_location_param idb
			| TYPE_EXPR _->false
			)
			in
			add_param (n,t);value)
	|_->false

(** Check is the given id refer to a parameter.
	This is needed for compatibility with some versions of GLISS v1 where
	assignements to parameters (namely in the predecode attribute) was allowed
	This function is defined to be used in complement of is_location
	@param id	the id to check
	@return		True if the id refer to a parameter false otherwise
*)
let rec is_loc_spe id =
	if not !gliss1_compat then false else
	let sym = Irg.get_symbol id in
	
	let is_location_param id =
		let sym=Irg.get_symbol id in
		match sym with
		| TYPE _	-> true
		|_			-> false  in
	
	let process_param n t =
		rm_symbol n;
		let value = (match t with
			| TYPE_ID tid -> is_location_param tid
			| TYPE_EXPR _ -> true) in
		add_param (n, t);
		value in
	
	match sym with
	| PARAM (n, t)	->
		let r = process_param n t in
		if r then Printf.fprintf stderr
			"WARNING:%s:%d: assignment to parameter \"%s\" is supported for compatibility purpose but is now deprecated!\n"
			!Lexer.file !Lexer.line n;
		r
	| _				->
		false


(** this function is used to find all references to parameters in a string passed to a format
	@param str	The string to treat
	@return 	A list of string matching reg_exp
 *)
let get_all_ref str =
	let rec temp str_l res_l=
		match str_l with
		| [] -> res_l
		| (Str.Text _)::l -> temp l res_l
		| (Str.Delim s)::l -> temp l (s::res_l) in
	temp (Irg.split_format_string str) []


(** Create a FORMAT operation and check if it is well written
	@param fmt			The string to print
	@param exp_list		The list of parameters to be used as variables in str (reversed order).
	@return				Format expression.
	@raise SemError 	Raised when the parameters are incorrect
*)
let build_format fmt exp_list =

	let rec check_arg n fmt arg =
		let (nat, arg) =
			match Str.last_chars fmt 1 with
			| "d" 	-> coerce_to_int arg (INT(32))
			| "u" 	-> coerce_to_int arg (CARD(32))
			| "s"	-> coerce_to_string arg
			| "b" 	-> (true, arg)
			| "x" 	-> coerce_to_int arg (CARD(32))
			| "f" 	-> coerce_to_float arg ieee754_64
			| "l"	(* deprecated *)
			| "@"	-> coerce_to_int arg (INT(32))
			| _		-> failwith "internal error : build_format" in
		if arg == NONE then
			error (output [
					PTEXT (sprintf "incorrect type at argument %d in format \"%s\".\n" n fmt);
					PTEXT "\tArgument "; PEXPR arg; PTEXT " of type "; PTYPE (get_type_expr arg);
					PTEXT " does not match format "; PTEXT fmt])
		else begin
			(if not nat then
				warn (output [PTEXT "possible inconsistency between format '"; PTEXT fmt; PTEXT "' and argument "; PEXPR arg]));
			arg
		end in

	let rec check_args n escs args =
		match (escs, args) with
		| [], []		-> []
		| [], _ 		-> pre_error "too many arguments in format"
		| _, []			-> pre_error "not enought arguments in format"
		| e::et, a::at	-> (check_arg n e a) :: (check_args (n + 1) et at) in

	let ref_list = get_all_ref fmt in
	FORMAT (fmt, List.rev (check_args 1 ref_list exp_list))


(** This function check if the paramters of a canonical function are correct
	@param name	The name of the canonical function
	@param	params	The list of parameters given to the canonical function
	@return 		Fixed list of parameters (possibly with casting).
*)
let check_canon_params name params =

	let check_type p t =
		let pt = get_type_expr p in
		if t = pt then p else
		match (t, pt) with
		| _, NO_TYPE
		| STRING, _
		| _, STRING -> pre_error "bad argument type"
		| _, ANY_TYPE -> p
		| _, _ -> Irg.COERCE(t, p) in

	let check_no_type p =
		if (get_type_expr p) = NO_TYPE
		then pre_error "bad argument type" in

	let canon = get_canon name in
	if canon.name = UNKNOW then (List.iter check_no_type params; params) else
	try
		List.map2 check_type params canon.type_param
	with Invalid_argument _ ->
		pre_error "bad number of arguments"


(** This function build a canonical expression, after checked if the parameters are corrects.
	@param name		The name of the canonical function
	@param params	The list of parameters given to the canonical function
	@return 		A CANON_EXPR expression
	@raise SemError Raised when the parameters are incorrects
*)
let build_canonical_expr name params =
	let e = get_canon name in
	CANON_EXPR(e.type_res, name, check_canon_params name params)


(** This function build a canonical statement, after checked if the parameters are corrects.
	If the function have a return type different of NO_TYPE (except of course UNKNOW_TYPE, when the function is unknow), then a warning is displayed

	@param name	The name of the canonical function
	@param list_param	The list of parameters given to the canonical function
	@return a CANON_STAT expression
	@raise SemError 	Raised when the parameters are incorrects
*)
let build_canonical_stat name param =
	let e = get_canon name in
	if e.type_res <> NO_TYPE && e.type_res <> ANY_TYPE then
		Lexer.display_warning (Printf.sprintf "the result of the canonical function %s is not used" name);
	CANON_STAT (name , check_canon_params name param)


(** Get type of a location.
	@param loc	Location to get type of.
	@return		Type of the location. *)
let get_loc_type loc =
	match loc with
	| LOC_NONE -> NO_TYPE
	| LOC_REF (FLOAT (n, m), _, _, l, _) when l <> NONE -> CARD(n + m)
	| LOC_REF (t, _, _, _, _) -> t
	| LOC_CONCAT (t, _, _) -> t


(** Get the type of location reference.
	@param name			Location reference name.
	@return				(real name, type of the matching location)
	@raise SemError		Raised when the reference does not exist,
						or is not a location. *)
let get_loc_ref_type name =

	let look t =
		match t with
		| TYPE_ID _		-> ANY_TYPE
		| TYPE_EXPR e	-> e in

	match Irg.get_symbol name with
	| UNDEF 			-> pre_error (name ^ " is undefined")
	| MEM (n, _, t, _) 	-> t
	| REG (n, _, t, _) 	-> t
	| VAR (n, _, t, _) 	-> t
	| PARAM (n, t)		-> if not !gliss1_compat then ANY_TYPE else look t
	| _					-> pre_error (name ^ " is not a location")


(** Check if the alias atribute matches the current entity.
	@param mem	Memory entity to test (may be REG, VAR or MEM).
	@return		None if it matches, Some id if it doesn't match with id the problematic memory. *)
let check_alias mem =
	let attrs = Irg.attrs_of mem in

	let rec test loc =
		match loc with
		| LOC_NONE
			-> ()
		| LOC_CONCAT (_, l1, l2)
			-> (test l1; test l2)
		| Irg.LOC_REF (_, id, _, _, _) ->
			(match (mem, get_symbol id) with
			| (MEM _, MEM _)
			| (REG _, REG _)
			| (VAR _, VAR _)
				-> ()
			| (_, _)
				-> error_symbol (name_of mem) (asis (Printf.sprintf "unconsistant alias to %s" id))) in

	if not !gliss1_compat then  test (Irg.attr_loc "alias" attrs LOC_NONE);
	mem


(* list of undefined canonical type *)
let undef_canons: string list ref = ref []


(** Test if a canonical function is defined.
	Display an error if it not defined.
	@param name	Name of the canonical function. *)
let test_canonical name =
	if not (Irg.is_defined_canon name)
	&& not (List.mem name !undef_canons)
	then
		begin
			undef_canons := name :: !undef_canons;
			Lexer.display_warning
				(Printf.sprintf "the canonical function %s is not defined" name)
		end


(** Test if the symbol exists and is a data symbol
	(constant, memory, variable, register, parameter)
	@param name		Name of the symbol.
	@param indexed	Test if the data is indexed.
	@raise	SemError if either the symbol does not exists,
					or it is not data. *)
let test_data name indexed =
	let v = Irg.get_symbol name in
	match v with
	| Irg.UNDEF -> pre_error (sprintf "the identifier \"%s\" is undefined" name)

	(* never indexed *)
	| Irg.LET _
	| Irg.PARAM _
	| Irg.ATTR _

	(* may be indexed *)
	| Irg.MEM _				(* for compatibility with GLISS v1 *)
	| Irg.REG _
	| Irg.VAR _ -> ()
	| _ -> pre_error (sprintf "the idenfifier \"%s\" does not design a data" name)


(**	Used to return the expression associated to an expr attr, Irg.NONE if no expression extractable
	@param	name	Name of the attribute, it is supposed to have been stacked before.
	@return			If "name" is an expression attribute, return its expression,
					Else, Irg.NONE is returned
*)
let get_data_expr_attr name =
	match Irg.get_symbol name with
	| Irg.ATTR a ->
		(match a with
		| Irg.ATTR_EXPR(_, e) -> e
		| _ -> NONE)
	| _ -> NONE


(** Build a set expression and check the types (possibly introducing
	casts).
	@param loc		Assigned location.
	@param expr		Assigned expression.
	@return			Built statement. *)
let make_set loc expr =
	let ltype = get_loc_type loc in
	let etype = get_type_expr expr in
	if ltype = etype then Irg.SET (loc, expr) else
	match ltype, etype with

	| _, NO_TYPE
	| NO_TYPE, _ -> assert false
	| _, ANY_TYPE
	| ANY_TYPE, _ -> SET (loc, expr)

	| BOOL, INT _
	| BOOL, CARD _

	| INT _, BOOL
	| INT _, INT _
	| INT _, CARD _

	| CARD _, BOOL
	| CARD _, INT _
	| CARD _, CARD _

	| FLOAT _, FLOAT _ ->
		Irg.SET (loc, Irg.CAST(ltype, expr))

	| FLOAT _, CARD _
	| FLOAT _, INT _
	| INT _, FLOAT _
	| CARD _, FLOAT _ ->
		Lexer.display_warning
			("assigning float/int to int/float is ambiguous.\n"
			^ "\tAs default, resolved to bit to bit assignment.\n"
			^ "\tUse instead either field notation for bit to bit assignment\n"
			^ "\tor explicit coerce for value conversion.");
			let res = Irg.SET (loc, Irg.CAST(ltype, expr)) in
			Irg.output_statement stderr res;
			output_char stderr '\n';
			res

	| _ ->
		error (output [ PTEXT "unsupported assignment";
				PTEXT "\nLHS type:"; PTYPE ltype;
				PTEXT "\nRHS type:"; PTYPE etype; ])


(** This function is used to modify parameters of a format called into a specific attribute
	to add the .attribute at all reference parameters (resolving to %s).
	@param a		Attribute name.
	@param e		Expression to transform.
	@return			Transformed expression. *)
let change_string_dependences a e =

	let rec process e = match e with
		| REF (_, name) -> FIELDOF (STRING, name, a)
		| ELINE (f, l, e) -> ELINE (f, l, process e)
		| _ -> e in

	let rec temp r_l e_l =
		match r_l with
		| []->[]
		| r::l -> if (Str.last_chars r 1 = "s")
				then (process (List.hd e_l))::(temp l (List.tl e_l))
				else (List.hd e_l)::(temp l (List.tl e_l)) in

	let rec look e =
		match e with
		| ELINE (f, l, e) -> ELINE (f, l, look e)
		| FORMAT (f, args) ->
			let r_list = List.rev (get_all_ref f) in
			FORMAT (f, temp r_list args)
		| IF_EXPR (tp, c, t, e) ->
			IF_EXPR (tp, c, look t, look e)
		| SWITCH_EXPR (tp, c, cs, d) ->
			SWITCH_EXPR (tp, c, List.map (fun (c, a) -> (c, look a)) cs, look d)
		| _ -> e in

	look e


(** Get an attribute and evaluates to integer.
	@param id		Attribute ID.
	@param attrs	Attribute list.
	@param def		Default value if attribute is not defined.
	@return			Integer value of the attribute, false else.
	@raise IrgError	If attribute exists but is not of good type.
	@raise SemError	If the attribute is a not a constant integer. *)
let attr_int id attrs def =
	let e = Irg.attr_expr id attrs Irg.NONE in
	if e = Irg.NONE then def else
	let cst = eval_const e in
	to_int cst


(** Add a specification to the IRG representation.
	@param name			Name of the specification.
	@param spec			Specification to add.
	@raise Irg.Error	If the symbol already exists. *)
let add_spec name spec  =
	if Irg.is_defined name
	then raise (Irg.Error (fun out -> Printf.fprintf out "%s: symbol %s already defined at %s" (Lexer.current_loc ()) name (pos_of name)))
	else Irg.add_symbol name spec


(** Check if the image contains references to all parameters.
	@param id		Identifier of the specification item.
	@param params	List of parameters.
	@raise SemError	If a parameter is not in the image. *)
let check_image id params =

	let e = get_data_expr_attr "image" in
	let names = fst (List.split params) in

	let rec make l =
		match l with
		| [] -> ""
		| [n] -> n
		| h::t -> h ^ ", " ^ (make t) in

	let rec remove n l =
		match l with
		| [] -> []
		| h::t when h = n -> t
		| h::t -> h::(remove n t) in

	let rec check l e =
		match e with
		| Irg.NONE
		| Irg.CONST _
			-> l
		| Irg.COERCE (_, e)
		| Irg.BITFIELD (_, e, _, _)
		| Irg.UNOP (_, _, e)
		| Irg.ELINE (_, _, e)
		| Irg.CAST (_, e)
			-> check l e
		| Irg.FORMAT (_, es)
		| Irg.CANON_EXPR (_, _, es)
			-> List.fold_left (fun l e -> check l e) l es
		| Irg.REF (_, n)
		| Irg.FIELDOF (_, n, _)
		| Irg.ITEMOF (_, n, _)
			 -> remove n l
		| Irg.BINOP (_, _, e1, e2)
			-> check (check l e1) e2
		| Irg.IF_EXPR (_, e1, e2, e3)
			-> check (check (check l e1) e2) e3
		| Irg.SWITCH_EXPR (_, e, es, d)
			-> List.fold_left (fun l (_, e) -> check l e) (check (check l e) d) es in

	if e <> Irg.NONE then begin
		let r = check names e in
		if r = [] then ()
		else pre_error (sprintf "parameters %s of %s are not used in the image" (make r) id)
	end


(** Check if the given parameter is already declared.
	@param name			Name to test.
	@raise SemError		If the parameter already exists. *)
let check_param_exists name =
	match Irg.get_symbol name with
	| Irg.PARAM _ -> pre_error (sprintf "parameter %s declared twice." name)
	| _ -> ()


(** Get type expression from a simple type.
		@param t  Type to convert.
		@return   Matching type expression. *)
let get_expr_from_type t =
	match t with
	| TYPE_ID id -> get_type_ident id
	| TYPE_EXPR t -> t


(** Split an image format according escape sequences.
		@param image Image to split.
		@return      List of image pieces made of Str.split_result. *)
let split_image image =
	Str.full_split (Str.regexp "%[0-9]+b") image


(** Compute size in bits of image escape sequence "%[0-9]+b".
		@param esc Escape to measure.
		@return    Escape size in bits. *)
let image_escape_size esc =
	if (String.length esc) <= 2
	then 1
	else int_of_string (String.sub esc 1 ((String.length esc) - 2))


(** Split an syntax format according % sequences.
		@param syntax Syntax to split.
		@return       List of syntax pieces made of Str.split_result. *)
let split_syntax syntax =
	Str.full_split (Str.regexp "%[0-9]+l*dxXuoOs") syntax


(** Look for a field type.
	@param spec			Specification to look in.
	@param id			Field identifier.
	@return				Field type (possibly UNKNOWN_TYPE for non-unique OR_MODE/OP).
	@raise  Not_found	If the field cannot be found. *)
let rec get_field_type spec id =

	let join t1 t2 =
		match (t1, t2) with
		| (t1, t2) when t1 = t2	-> t1
		| (ANY_TYPE, _)
		| (_, ANY_TYPE) 	-> ANY_TYPE
		| (NO_TYPE, t)
		| (t, NO_TYPE) 		-> t
		| _ 					-> ANY_TYPE in

	let rec find params attrs =
		let e = attr_expr id attrs NONE in
		if e = NONE then raise Not_found else
		param_stack params;
		let tt = get_type_expr e in
		param_unstack params;
		tt in

	let rec collect syms tt =
		if tt = ANY_TYPE then tt else
		match syms with
		| [] 	-> tt
		| h::t	-> collect t (join tt (get_field_type (get_symbol h) id)) in

	match spec with
	| AND_MODE (_, params, _, attrs)
	| AND_OP (_, params, attrs)	-> find params attrs
	| OR_MODE (_, syms) 		-> collect syms NO_TYPE
	| _ 						-> raise Not_found


(** Build and type a field expression.
	@param pid		Parent identifier.
	@param cid		Child identifier.
	@return			Built field expression. *)
let type_of_field pid cid =
	if is_defined pid then
		match Irg.get_symbol pid with
		| PARAM(_, t) ->
			(match t with
			| TYPE_ID(name) ->
				(try get_field_type (get_symbol name) cid
				with Not_found -> ANY_TYPE)
			| _ -> pre_error (Printf.sprintf "%s cannot have a %s attribute\n" pid cid))
		| _ -> pre_error (sprintf "%s can not have a %s attribute\n" pid cid)
	else
		pre_error (sprintf "the identifier %s is undefined\n" pid)


(** Build a concatenated location.
	@param l1	First location.
	@param l2	Second location.
	@return		Built location.
	*)
let make_concat_loc l1 l2 =
	let length =
		(get_type_length (get_loc_type l1)) +
		(get_type_length (get_loc_type l2)) in
	LOC_CONCAT (CARD length, l1, l2)


(** Build an access location, i.e. access to a memory with an index
	and a bit field.
	@param id	Resource identifier.
	@param idx	Index.
	@param up	Upper bit.
	@param lo	Lower bit.
	@return 	Built location . *)
let make_access_loc id idx up lo =
	let id = unalias_local id in
	if (is_location id) || (is_loc_spe id)
	then begin LOC_REF (get_loc_ref_type id, id, idx, up, lo) end
	else begin pre_error (Printf.sprintf "'%s' is not a valid location" id) end


(** Build a coercition expression.
	@param t			Type to coerce to.
	@param expr			Coerced expression.
	@return				Built expression.
	@raise Irg.Error	If coercition is impossible or expr contains an error.*)
let make_coerce t expr =
	if t = STRING then pre_error "unable to coerce a string into another expression type" else
	if (get_type_expr expr) = STRING then pre_error "unable to an expression coerce into a string" else
	COERCE (t, expr)


(** Build a bitfield expression.
	@param base			Base expression.
	@param up			Upper bit.
	@param lo			Lower bit.
	@return				Built expression.
	@raise Irg.PreError	If error. *)
let make_bitfield base up lo =
	try
		let v1 = Int32.to_int (to_int32 (eval_const up)) in
		let v2 = Int32.to_int (to_int32 (eval_const lo)) in
		let v1, v2 = if v1 <= v2 then v1, v2 else v2, v1 in
		(* !!TODO!! check type (only scalar allowed) and length if possible *)
		begin
			let r = BITFIELD (CARD (v2 - v1 + 1), base, up, lo) in
			r
		end
	with PreError _ ->
		BITFIELD (get_type_expr base, base, up, lo)


(** Build an IF expression.
	@param cond		Condition.
	@param e1				Then expression.
	@param e2				Else expression.
	@return					Built expression.
	@raise Irg.PreError		If an error is found. *)
let make_if_expr cond e1 e2 =
	let t1 = get_type_expr e1 in
	let t2 = get_type_expr e2 in
	if t1 = ANY_TYPE || t2 = ANY_TYPE then IF_EXPR (ANY_TYPE, cond, e1, e2) else
	if t1 = t2 then IF_EXPR (t1, cond, e1, e2) else
	let (t, e1, e2) = num_auto_coerce e1 e2 in
	if t <> NO_TYPE then IF_EXPR (t, cond, e1, e2) else
	error_two_operands "if" e1 e2


(** Build a switch expression.
	@param cond				Switch values.
	@param cases			Cases of the switch.
	@param def				Default expression.
	@raise Irg.PreError		If an error is found. *)
let make_switch_expr cond cases def =
	SWITCH_EXPR (check_switch_expr cond cases def, cond, cases, def)


(** Check existence of data and return its type and its size.
	Return (type, false, expression) if the symbol is an attribute
	or (type, _, NONE) if the symbol is a valid data item.
	@param id		Date identifier.
	@param idx		True for indexed resource, false else.
	@return			(type, indexed, expression for attributes)
	@raise PreError	If the symbol does not exist or is not data. *)
let get_data_info id =

	let named_type pid id =
		match get_symbol id with
		| TYPE (_, t) 	-> t
		| UNDEF
		| AND_MODE _
		| OR_MODE _ 	-> ANY_TYPE
		| AND_OP _
		| OR_OP _		-> error (asis (sprintf "symbol '%s' is not a data item" pid))
		| RES _
		| EXN _
		| CANON_DEF _
		| LET _
		| REG _
		| VAR _
		| MEM _
		| PARAM _
		| ATTR _ 		-> error (asis (sprintf "invalid type for '%s'" pid)) in

	match get_symbol id with
	| UNDEF							-> error (asis (sprintf "symbol '%s' is undefined" id))
	| LET (_, t, _) 				-> (t, false, NONE)
	| REG (_, n, t, _) 				-> (t, n > 1, NONE)
	| VAR (_, n, t, _)			 	-> (t, n > 1, NONE)
	| MEM (_, _, t, _)				-> (t, true, NONE)
	| PARAM (_, TYPE_EXPR t) 		-> (t, false, NONE)
	| PARAM (_, TYPE_ID tid) 		-> (named_type id tid, false, NONE)
	| ATTR (ATTR_EXPR (_, e))		-> (get_type_expr e, false, e)
	| ATTR _
	| TYPE _
	| AND_MODE _
	| OR_MODE _
	| AND_OP _
	| OR_OP _
	| RES _
	| EXN _
	| CANON_DEF _					-> error (asis (sprintf "symbol '%s' is not a data item" id))


(** Build a reference and possibly reduce to the matching attribute..
	@param id		Identifier of the reference.
	@return			Built expression. *)
let make_ref id =
	let id = unalias_local id in
	let (t, i, e) = get_data_info id in
	if e = NONE then REF (t, id) else e


(** Build a let specification.
	@param id			Let identifier.
	@param t			Type of the let (possibly NO_TYPE)
	@param e			Expression of the constant.
	@return				Built specification.
	@raise PreError		If there is a typing error or something is not computable. *)
let make_let id t e =
	let t = if t = NO_TYPE then get_type_expr e else t in
	Irg.LET (id, t, eval_const e)


(** Check type of expression after instruction instanciation.
	@param expr	Expression to check type for.
	@return		Fully typed expression (no more UNKNOWN_TYPE). *)
let rec check_expr_inst expr =

	let rec is_canon e =
		match e with
		| CANON_EXPR _ -> true
		| ELINE (_, _, e) -> is_canon e
		| _ -> false in

	let r =
		match expr with
		| NONE
		| FIELDOF _
		| CONST _ ->
			expr
		| REF (t, id) ->
			if t <> ANY_TYPE then expr else (check_expr_inst (make_ref id))
		| COERCE (t, e) ->
			let e' = check_expr_inst e in
			if e == e' then expr else make_coerce t e'
		| FORMAT (fmt, args) ->
			let args' = List.map check_expr_inst args in
			if args = args' then expr else FORMAT(fmt, args')
		| CANON_EXPR (t, id, args) ->
			let args' = List.map check_expr_inst args in
			if args = args' then expr else CANON_EXPR(t, id, args')
		| ITEMOF (t, id, idx) ->
			let idx' = check_expr_inst idx in
			if idx == idx' then expr else ITEMOF (t, id, idx')
		| BITFIELD (t, e1, e2, e3) ->
			let e1', e2', e3' = check_expr_inst e1, check_expr_inst e2, check_expr_inst e3 in
			if t <> ANY_TYPE && e1 == e1' && e2 == e2' && e3 == e3' then expr else make_bitfield e1' e2' e3'
		| UNOP (t, op, e) ->
			let e' = check_expr_inst e in
			if t <> ANY_TYPE && e == e' then expr else get_unop e' op
		| BINOP (t, op, e1, e2) ->
			let e1', e2' = check_expr_inst e1, check_expr_inst e2 in
			if t <> ANY_TYPE && e1 == e1' && e2 == e2' then expr else get_binop e1' e2' op
		| IF_EXPR (t, cond, e1, e2) ->
			let cond', e1', e2' = check_expr_inst cond, check_expr_inst e1, check_expr_inst e2 in
			if t <> ANY_TYPE && cond == cond' && e1 == e1' && e2 == e2' then expr else make_if_expr cond' e1' e2'
		| SWITCH_EXPR (t, cond, cases, def) ->
			let cond', def' = check_expr_inst cond, check_expr_inst def in
			let cases' = List.map (fun (c, e) -> (c, check_expr_inst e)) cases in
			if t <> ANY_TYPE && cond == cond' && def == def' && cases = cases' then expr else make_switch_expr cond' cases' def'
		| CAST (t, e) ->
			let e' = check_expr_inst e in
			if t <> ANY_TYPE && e == e' then expr else CAST(get_type_expr e', e')
		| ELINE (file, line, e) ->
			handle_error file line (fun _ ->
				let e' = check_expr_inst e in
				if e == e' then expr else ELINE (file, line, e')) in

	assert ((get_type_expr r) <> ANY_TYPE || is_canon r);
	r


(** Check type of a location after instruction instanciation.
	@param loc	Location to check type for.
	@return		Fully typed location (no more UNKNOWN_TYPE). *)
let rec check_loc_inst loc =
	let r = match loc with
			| LOC_NONE -> loc
			| LOC_REF(t, id, e1, e2, e3) ->
				let e1', e2', e3' = check_expr_inst e1, check_expr_inst e2, check_expr_inst e3 in
				if t <> ANY_TYPE && e1 == e1' && e2 == e2' && e3 = e3' then loc else
				make_access_loc id e1' e2' e3'
			| LOC_CONCAT(t, l1, l2) ->
				let l1', l2' = check_loc_inst l1, check_loc_inst l2 in
				if t <> ANY_TYPE && l1 == l1' && l2 == l2' then loc else
				make_concat_loc l1' l2' in
	(*assert ((get_loc_type r) <> ANY_TYPE);*)
	if (get_loc_type r) = ANY_TYPE then
	begin
		assert false
	end;
	r


(** Check type of statements after instruction instanciation.
	@param stat	Statement to check type for.
	@return		Fully typed statement (no more ANY_TYPE). *)
let rec check_stat_inst stat =
	match stat with
	| NOP
	| EVAL _
	| ERROR _
	| LOCAL _ ->
		stat
	| SEQ (s1, s2) ->
		let s1', s2' = check_stat_inst s1, check_stat_inst s2 in
		if s1 == s1' && s2 == s2' then stat else SEQ(s1', s2')
	| SET (loc, expr) ->
		let loc', expr' = check_loc_inst loc, check_expr_inst expr in
		if expr == expr' && loc == loc' then stat else make_set loc' expr'
	| CANON_STAT (id, args) ->
		let args' = List.map check_expr_inst args in
		if args = args' then stat else CANON_STAT (id, args')
	| IF_STAT (cond, s1, s2) ->
		let cond', s1', s2' = check_expr_inst cond, check_stat_inst s1, check_stat_inst s2 in
		if cond == cond' && s1 == s1' && s2 == s2' then stat else IF_STAT(cond', s1', s2')
	| SWITCH_STAT (cond, cases, def) ->
		let cond', def' = check_expr_inst cond, check_stat_inst def in
		let cases' = List.map (fun (c, s) -> (c, check_stat_inst s)) cases in
		if cond == cond' && def == def' && cases = cases' then stat else SWITCH_STAT (cond', cases', def')
	| LINE (file, line, stat) ->
		let stat' = check_stat_inst stat in
		if stat == stat' then stat else LINE (file, line, stat')
	| FOR(v, uv, t, l, u, b) ->
		let b' = check_stat_inst b in
		if b = b' then stat else FOR(v, uv, t, l, u, b')


(** Check type of attribute after instruction instanciation.
	@param attr	Attribute to check type for.
	@return		Fully typed attribute (no more UNKNOWN_TYPE). *)
let check_attr_inst attr =
	match attr with
	| ATTR_EXPR (id, expr) ->
		let expr' = check_expr_inst expr in
		if expr == expr' then attr else ATTR_EXPR(id, expr')
	| ATTR_STAT (id, stat) ->
		let stat' = check_stat_inst stat in
		if stat == stat' then attr else ATTR_STAT(id, stat')
	| ATTR_USES ->
		attr
	| ATTR_LOC (id, loc) ->
		attr
	| ATTR_LINE_INFO _ ->
		attr


(** Check type of specification after instruction instanciation.
	@param spec	Specification to check type for.
	@return		Fully typed specification (no more UNKNOWN_TYPE). *)
let check_spec_inst spec =
	match spec with
	| AND_OP (id, params, attrs) ->
		Irg.param_stack params;
		Irg.attr_stack attrs;
		let nattrs = List.map check_attr_inst attrs in
		Irg.attr_unstack attrs;
		Irg.param_unstack params;
		AND_OP(id, params, nattrs)
	| _ -> failwith "Sem.check_spec_inst: bad specification"


(** Enumerate the different values between low and up bounds (inclusive).
	@param l	Lower bound.
	@param u	Upper bound.
	@return		List of values between l and u. *)
let enum_values l u =
	let rec make i u =
		if i = u then [i] else
		i :: (make (Int32.add i Int32.one) u) in

	if (Int32.compare l u) > 0
	then error (fun out -> Printf.fprintf out  "in enum range %ld '..' %ld, lower value must precede upper value" l u)
	else make l u


(** In a sorted list, ensures that each value is unique.
	@param l	List to process.
	@return		Result list. *)
let rec uniq l =
	match l with
	| []
	| [_]					-> l
	| a::b::t when a = b	-> uniq (b::t)
	| h::t					-> h::(uniq t)


(** Add a local variable definition.
	@param id	Local variable identifier.
	@param e	Expression initializing the variable.
	@return		Matching instruction. *)
let make_local id e =
	let t = get_type_expr e in
	if t = NO_TYPE || t == STRING then 
		error (output [ PTEXT "type "; PTYPE t; PTEXT " unsupported for local variable"])
	else
		begin
			let uid = Printf.sprintf "__gliss_%d_%s" !local_uniq id in
			local_uniq := !local_uniq + 1;
			handle_local uid t;
			StringHashtbl.add local_map id uid;
			SEQ(LOCAL (uid, id, t), SET(LOC_REF (t, uid, NONE, NONE, NONE), e))
		end


(** Prepare a for-instruction, mainly, declare the local instruction.
	@param v	Variable identifier.
	@param t	Type of identifier (possibly NO_TYPE).
	@param l	Lower value (constant expression).
	@param u	Upper value (constant expression). *)
let prepare_for v t l u =
	let (lt, lv) = eval_typed_const l in
	let (ut, uv) = eval_typed_const u in
	let t =
		if t = STRING then error (asis "induction variable cannot be of type string")
		else if t = NO_TYPE then lt
		else t in
	let lv = if lt = t then lv else eval_coerce t (lt, lv) in
	let uv = if ut = t then uv else eval_coerce t (ut, uv) in
	let vv = Printf.sprintf "__gliss_%d_%s" !local_uniq v in
	local_uniq := !local_uniq + 1;
	handle_local vv t;
	StringHashtbl.add local_map v vv;
	add_symbol vv (VAR (vv, 1, t, []));
	(v, vv, t, lv, uv)


(** Build a for statement.
	@param v	Variable identifier.
	@param uv	Unique variable name.
	@param t	Type of identifier (possibly NO_TYPE).
	@param l	Lower value (constant expression).
	@param u	Upper value (constant expression).
	@param b	For body. *)
let make_for (v, uv, t, l, u) b =
	FOR(v, uv, t, l, u, b)


(** Make a local variable definition with its own type.
	@param id	Local variable identifier.
	@param t	Declared type.
	@param e	Expression initializing the variable.
	@return		Matching instruction. *)
let make_typed_local id t e =
	if t = NO_TYPE || t == STRING then 
		error (output [ PTEXT "type "; PTYPE t; PTEXT " unsupported for local variable"])
	else
		begin
			let uid = Printf.sprintf "__gliss_%d_%s" !local_uniq id in
			local_uniq := !local_uniq + 1;
			handle_local uid t;
			StringHashtbl.add local_map id uid;
			SEQ(LOCAL (uid, id, t), make_set (LOC_REF (t, uid, NONE, NONE, NONE)) e)
		end


