(*
 * $Id: sem.ml,v 1.2 2008/06/30 07:50:00 pascalie Exp $
 * Copyright (c) 2007, IRIT - UPS <casse@irit.fr>
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
 * along with Foobar; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

open Irg
(*#use "irg.ml" *)
exception SemError of string
exception ManyDefaultsInSwitch


(** False value. *)
let false_const = Irg.CARD_CONST Int32.zero
(** True value. *)
let true_const = Irg.CARD_CONST Int32.one


(** Convert from OCAML boolean to SimNML boolean.
	@param v	OCAML boolean.
	@return		SimNML boolean. *)
let to_bool v = if v then true_const else false_const


(** Convert constant to int32.
	@param c		Constant to convert.
	@return			Matching int32 value.
	@raise SemError	If the constant is not convertible. *)
let to_int32 c =
	match c with
	  CARD_CONST v -> v
	| _ -> raise (SemError "should evaluate to an int")


(** Convert constant to int.
	@param c		Constant to convert.
	@return			Matching int value.
	@raise SemError	If the constant is not convertible. *)
let to_int c =
	Int32.to_int (to_int32 c)


(** Test if a constant is true.
	@param c	Constant to test.
	@return		True if constant is true. *)
let is_true c =
	match c with
	  NULL -> false
	| CARD_CONST v -> v <> Int32.zero
	| STRING_CONST v -> v <> ""
	| FIXED_CONST v -> v <> 0.0


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
let eval_unop op c =
	match (op, c) with
	  (Irg.NOT, _) ->
	  	Irg.CARD_CONST (if (is_true c) then Int32.zero else Int32.one)
	| (Irg.BIN_NOT, Irg.CARD_CONST v) ->
		Irg.CARD_CONST (Int32.lognot v)
	| (Irg.NEG, Irg.CARD_CONST v) ->
		Irg.CARD_CONST (Int32.neg v)
	| (Irg.NEG, Irg.FIXED_CONST v) ->
		Irg.FIXED_CONST (-. v)
	| _ ->
		raise (SemError (Printf.sprintf "bad type operand for '%s'"
			(string_of_unop op))) 


(** Evaluate a binary operation.
	@param op	Operation.
	@param v1	First operand.
	@param v2	Second operand.
	@param 		Result. *)
let eval_binop_card op v1 v2 =
	match op with
	  Irg.ADD		->
	  	Irg.CARD_CONST (Int32.add v1 v2)
	| Irg.SUB		->
		Irg.CARD_CONST (Int32.sub v1 v2)
	| Irg.MUL		->
		Irg.CARD_CONST (Int32.mul v1 v2)
	| Irg.DIV		->
		Irg.CARD_CONST (Int32.div v1 v2)
	| Irg.MOD		->
		Irg.CARD_CONST (Int32.rem v1 v2)
	| Irg.EXP		->
		Irg.CARD_CONST (Int32.of_float ((Int32.to_float v1) ** (Int32.to_float v2)))
	| Irg.LSHIFT	->
		Irg.CARD_CONST (Int32.shift_left v1 (Int32.to_int v2))
	| Irg.RSHIFT	->
		Irg.CARD_CONST (Int32.shift_right v1 (Int32.to_int v2))
	| Irg.LROTATE	->
		Irg.CARD_CONST (rotate_left v1 v2)
	| Irg.RROTATE	->
		Irg.CARD_CONST (rotate_right v1 v2)
	| Irg.LT		->
		to_bool (v1 < v2)
	| Irg.GT		->
		to_bool (v1 > v2)
	| Irg.LE		->
		to_bool (v1 <= v2)
	| Irg.GE		->
		to_bool (v1 >= v2)
	| Irg.EQ		->
		to_bool (v1 = v2)
	| Irg.NE		->
		to_bool (v1 <> v2)
	| Irg.AND		->
		if (v1 <> Int32.zero) && (v2 <> Int32.zero) then true_const else false_const
	| Irg.OR		->
		if (v1 <> Int32.zero) || (v2 <> Int32.zero) then true_const else false_const
	| Irg.BIN_AND	->
		Irg.CARD_CONST (Int32.logand v1 v2)
	| Irg.BIN_OR	->
		Irg.CARD_CONST (Int32.logor v1 v2)
	| Irg.BIN_XOR	->
		Irg.CARD_CONST (Int32.logxor v1 v2)
	| _ ->
		raise (SemError (Printf.sprintf "bad type operand for '%s'"
			(string_of_binop op))) 


(** Evaluate a fixed binary operation.
	@param op	Operation.
	@param v1	First operand.
	@param v2	Second operand.
	@param 		Result. *)
let eval_binop_fixed op v1 v2 =
	match op with
	  ADD		->
		Irg.FIXED_CONST (v1 +. v2)
	| SUB		->
		Irg.FIXED_CONST (v1 -. v2)
	| MUL		->
		Irg.FIXED_CONST (v1 *. v2)
	| DIV		->
		Irg.FIXED_CONST (v1 /. v2)
	| EXP		->
		Irg.FIXED_CONST (v1 ** v2)
	| LT		->
		to_bool (v1 < v2)
	| GT		->
		to_bool (v1 > v2)
	| LE		->
		to_bool (v1 <= v2)
	| GE		->
		to_bool (v1 >= v2)
	| EQ		->
		to_bool (v1 = v2)
	| NE		->
		to_bool (v1 <> v2)
	| AND		->
		if v1 <> 0. && v2 <> 0. then true_const else false_const
	| OR		->
		if v1 <> 0. || v2 <> 0. then true_const else false_const
	| _ ->
		raise (SemError (Printf.sprintf "bad type operand for '%s'"
			(string_of_binop op))) 


(** Evaluate a string binary operation.
	@param op	Operation.
	@param v1	First operand.
	@param v2	Second operand.
	@param 		Result. *)
let eval_binop_string op v1 v2 =
	match op with
	  LT 		-> to_bool (v1 < v2)
	| GT		-> to_bool (v1 > v2)
	| LE		-> to_bool (v1 <= v2)
	| GE		-> to_bool (v1 >= v2)
	| EQ		-> to_bool (v1 = v2)
	| NE		-> to_bool (v1 <> v2)
	| CONCAT	-> STRING_CONST (v1 ^ v2)
	| _ ->
		raise (SemError (Printf.sprintf "bad type operand for '%s'"
			(string_of_binop op))) 


(** Evaluate a binary operator.
	@param op	Binary operator.
	@param c1	First operand.
	@param c2	Second operand.
	@return		Result of the operation. *)
let eval_binop op c1 c2 =
	match (c1, c2) with
	  (Irg.CARD_CONST v1, Irg.CARD_CONST v2) ->
		eval_binop_card op v1 v2
	| (Irg.FIXED_CONST v1, Irg.CARD_CONST v2) ->
		eval_binop_fixed op v1 (Int32.to_float v2)
	| (Irg.CARD_CONST v1, Irg.FIXED_CONST v2) ->
		eval_binop_fixed op (Int32.to_float v1) v2
	| (Irg.FIXED_CONST v1, Irg.FIXED_CONST v2) ->
		eval_binop_fixed op v1 v2
	| (Irg.STRING_CONST v1, Irg.STRING_CONST v2) ->
		eval_binop_string op v1 v2
	| _ ->
		raise (SemError (Printf.sprintf "bad type operand for '%s'"
			(string_of_binop op))) 


(** Perform the expression switch.
	@param c		Condition.
	@param cases	Cases of the switch.
	@param def		Default value. *)
let rec select c cases def =
	  match cases with
	    [] -> eval_const def
	  | (cp, e)::_ when cp = c -> eval_const e
	  | _::t -> select c t def

(** Evaluate an expression to constant.
	@param expr		Expression to evaluate. *)
and eval_const expr =
	match expr with
	  CONST cst ->
	  	cst
	| UNOP (op, e) ->
		eval_unop op (eval_const e)
	| BINOP (op, e1, e2) ->
		eval_binop op (eval_const e1) (eval_const e2)
	| IF_EXPR(c, t, e) ->
		if is_true (eval_const c) then eval_const t else eval_const e
	| SWITCH_EXPR (c, cases, def) ->
		select (eval_const c) cases def
	| REF id ->
		(match get_symbol id with
		  LET (_, cst) -> cst
		| _ -> raise (SemError "this expression should be constant")) 
	| _ -> 
		raise (SemError "this expression should be constant")


(** Find a type by its identifier.
	@param id		Identifier of the looked type.
	@return			Type matching the identifier.
	@raise SemError	If the identifier does not exists or if the named item is
	not a type. *)
let type_from_id id =
	try
		match StringHashtbl.find syms id with
		  TYPE (_, te) -> te
		| _ ->
			raise (SemError (Printf.sprintf "%s does not named a type" id))
	with Not_found ->
		raise (SemError (Printf.sprintf "unknown identifier \"%s\"" id))


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


(** Give the size of a memory location
   @author PJ
   @param  loc		a memory location
   @return  the number of bit available of the location
*)
let rec  get_location_size loc = 
	match loc with
	  LOC_REF id -> (match Irg.get_symbol id with
			UNDEF -> raise (SemError (Printf.sprintf "get_location_size : undeclared memory location :\"%s\"" id))
			|MEM(s,i,t,_)|REG(s,i,t,_)|VAR(s,i,t) ->( match t with
								 NO_TYPE |RANGE _ -> 8  (* maybe à modifier *)
								|INT t|CARD t -> t
								|FIX(n,l)|FLOAT(n,l) -> n+l
								| _ -> raise SemError "unexpected type"  )
			| _ ->raise (SemError (Printf.sprintf "get_location_size : identifier is not a memory location reference :\"%s\"" id)))

	| LOC_ITEMOF (loc,_) -> (get_location_size loc)
	| LOC_BITFIELD (_,e1,e2) ->(match ((eval_const e2),(eval_const e1))with
					 (CARD_CONST t,CARD_CONST v)-> (Int32.to_int t) - (Int32.to_int v)
					|(FIXED_CONST t,FIXED_CONST v) -> (int_of_float t)-(int_of_float v)
					|(CARD_CONST t,FIXED_CONST v) -> (Int32.to_int t)-(int_of_float v)
					|(FIXED_CONST t,CARD_CONST v) -> (int_of_float t)- (Int32.to_int v)
					|(STRING_CONST t,_)|(_,STRING_CONST t) -> raise (SemError (Printf.sprintf "get_location_size : uncompatible bitfield identifier :\"%s\"" t))
					|(NULL,_)|(_,NULL)->raise (SemError " memory location untyped "))
	| LOC_CONCAT (l1,l2) -> ((get_location_size l1)+(get_location_size l2))


(** make the implicit conversion a to b in b op a)
  @author PJ
  @param loc		location 
  @param expr_b		expression to cast
  @ return           expr_b casted to loc
*)

nml_cast a b = 
	match (a,b) with
	  (INT k,CARD(n,m)) ->  n+m
	| _ -> failwith


(** Test if a float number respects the IEE754 specification
    @param f          a nml float
    @return   true if the float is an IEEE754 float, false otherwise
    bit sign is first bit of the mantisse
*)
let is_IEEE754_float f = match f with
	(FLOAT(m,e)) ->(match(m+e) with
			 32 -> (e =8)&&(m=24) 
			| 64 -> (e = 11)&&(m =53)
			| 80 -> (e = 15)&&(m = 65)
			| _  -> raise (SemError "float number doesn't follow IEEE754 specification "))
	| _ -> raise SemError "function expect float number but parameter is not "
