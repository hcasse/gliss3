(*
 * $Id: irg.ml,v 1.39 2009/10/21 14:40:50 barre Exp $
 * Copyright (c) 2009, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of OGliss.
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

let asis str chan = output_string chan str

let range32 u l =
	Int32.add Int32.one (Int32.sub u l)
let mask32 n =
	Int32.sub (Int32.shift_left Int32.one (Int32.to_int n)) Int32.one
let maski n =
	Int32.sub (Int32.shift_left Int32.one n) Int32.one
let of_int32 i =
	{ Generic_int.length = 32; Generic_int.number = [i] }
let to_int32 i =
	List.nth i.Generic_int.number 0

let is_null g =
	let rec test l =
		match l with
		| [] -> true
		| f::t when (Int32.compare f Int32.zero) = 0 -> test t
		| _ -> false in
	test g.Generic_int.number

(** Test if two generic integer are equals, whatever their length.
	@param g1	First generic integer.
	@param g2	Second generic integer.
	@return		True if they are equals, false else. *)
let equals g1 g2 =
	let rec test l1 l2 =
		match l1, l2 with
		| [], [] -> true
		| h1::t1, h2::t2 when (Int32.compare h1 h2) = 0 -> test t1 t2
		| [], h::t when (Int32.compare h Int32.zero) = 0 -> test [] t
		| h::t, [] when (Int32.compare h Int32.zero) = 0 -> test t []
		| _ -> false in
	test g1.Generic_int.number g2.Generic_int.number


let add e1 e2 = Irg.BINOP (Sem.get_type_expr e1, Irg.ADD, e1, e2)
let sub e1 e2 = Irg.BINOP (Sem.get_type_expr e1, Irg.SUB, e1, e2)
let cst i32 = Irg.CONST (Irg.CARD(32), Irg.CARD_CONST i32)
let csta i = Irg.CONST (Irg.CARD(32), i)
let csti i = Irg.CONST (Irg.CARD(32), Irg.CARD_CONST (Int32.of_int i))
let shl e1 e2 = Irg.BINOP (Sem.get_type_expr e1, Irg.LSHIFT, e1, e2)
let shr e1 e2 = Irg.BINOP (Sem.get_type_expr e1, Irg.RSHIFT, e1, e2)
let and_ e1 e2 = Irg.BINOP(Sem.get_type_expr e1, Irg.BIN_AND, e1, e2)
let or_ e1 e2 = Irg.BINOP(Sem.get_type_expr e1, Irg.BIN_OR, e1, e2)
let getb e1 e2 e3 = Irg.BITFIELD (Sem.get_type_expr e1, e1, e2, e3)
let concat e1 e2 = Irg.BINOP(Irg.CARD ((Sem.get_length_from_expr e1) + (Sem.get_length_from_expr e2)), Irg.CONCAT, e1, e2)

(** A 32-bits integer with all bits to 1. *)
let all_ones = Int32.sub Int32.zero Int32.one


(** Build a mask of n bits (initialized to one).
	@param n	Number of bits.
	@return		Generic integer. *)
let mask n =
	let rec make n =
		if n = 0 then [] else
		if n < 32 then  [Int32.sub (Int32.shift_left Int32.one n) Int32.one] else
		all_ones :: (make (n - 32)) in
	{ Generic_int.length = n; Generic_int.number = make n }


(** Build a mask with ones from bit m to bit n.
	@param n	Upper mask bound.
	@param m	Lower mask bound.
	@return		Built mask. *)
let mask_range n m =
	Generic_int.shift_left (mask (n + 1 - m)) m


(** Get the mask of bits enclosing the given expression result.
	@param e	Expression to get mask for.
	@return		Matching mask. *)
let mask_of_expr e =
	mask (Sem.get_length_from_expr e)


(** Perform OR mask on the masks of the given triplet list (parameter, mask, expression).
	@param l	Triplet list.
	@param m	Initial mask.
	@return		All list mask ORed. *)
let rec or_masks l m =
	match l with
	| [] -> m
	| (_, m', _)::t -> or_masks t (Generic_int.logor m m')



(** Scan a decode argument and return the list of parameter assignment.
	@param e	argument expression
	@param m	current mask
	@param y	current reverse expression
	@return		list of triplets (operation parameter, maskn reverse expression).
	@throw		*)
let rec scan_decode_argument e m y =
	match e with

	| Irg.NONE -> failwith "scan_decode_argument"

	| Irg.CONST _ -> []

	| Irg.REF x ->
		(match Irg.get_symbol x with
		| Irg.PARAM _ -> [(x, m, y)]
		| _ -> raise (Toc.PreError (fun c -> Printf.fprintf c "unsupported symbol in image: %s" x)))

	| Irg.ELINE (file, line, e) ->
		(try scan_decode_argument e m y
		with Toc.PreError f -> raise (Toc.LocError (file, line, f)))

	| Irg.UNOP (t, Irg.BIN_NOT, e) ->
		scan_decode_argument e m (Irg.UNOP (t, Irg.BIN_NOT, y))

	| Irg.UNOP (t, Irg.NEG, e) ->
		scan_decode_argument e m (Irg.UNOP (t, Irg.NEG, y))

	| Irg.BITFIELD (t, b, u, l) ->
		let uc =
			try Sem.to_int32 (Sem.eval_const u)
			with Sem.SemError _ -> raise (Toc.PreError (asis "upper bitfield bound must be constant")) in
		let lc =
			try Sem.to_int32 (Sem.eval_const l)
			with Sem.SemError _ -> raise (Toc.PreError (asis "upper bitfield bound must be constant")) in
		scan_decode_argument
			b
			(Generic_int.logand m (mask_range (Int32.to_int uc) (Int32.to_int lc)))
			(shl (and_ y (cst (mask32 (range32 uc lc)))) (cst lc))

	| Irg.BINOP (t, Irg.ADD, e1, e2) ->
		(try
			let k = Sem.eval_const e2 in
			scan_decode_argument e1 m (sub y (csta k))
		with Sem.SemError _ ->
			try
				let k = Sem.eval_const e1 in
				scan_decode_argument e2 m (sub y (csta k))
			with Sem.SemError _ ->
				raise (Toc.PreError (asis "only forms as 'x + k' or 'k + x' are supported in image")))

	| Irg.BINOP (t, Irg.SUB, e1, e2) ->
		(try
			let k = Sem.eval_const e2 in
			scan_decode_argument e1 m (add y (csta k))
		with Sem.SemError _ ->
			try
				let k = Sem.eval_const e1 in
				scan_decode_argument e2 m (sub (csta k) y)
			with Sem.SemError _ ->
				raise (Toc.PreError (asis "only forms as 'x + k' or 'k + x' are supported in image")))

	| Irg.BINOP (t, Irg.LSHIFT, e1, e2) ->
		(try
			let k = Sem.to_int32 (Sem.eval_const e2) in
				scan_decode_argument e1 (Generic_int.shift_right_logical m (Int32.to_int k)) (shr y (cst k))
			with Sem.SemError _ ->
				raise (Toc.PreError (asis "only forms as 'x << k' are supported in image")))

	| Irg.BINOP (t, Irg.RSHIFT, e1, e2) ->
		(try
			let k = Sem.to_int32 (Sem.eval_const e2) in
				scan_decode_argument e1 (Generic_int.shift_left m (Int32.to_int k)) (shl y (cst k))
			with Sem.SemError _ ->
				raise (Toc.PreError (asis "only forms as 'x << k' are supported in image")))

	| Irg.BINOP (t, Irg.CONCAT, e1, e2) ->
		let s1 = Sem.get_length_from_expr e1 in
		let s2 = Sem.get_length_from_expr e2 in
		(scan_decode_argument e1 (Generic_int.logand m (mask s1)) (shr y (csti s2))) @
		(scan_decode_argument e2 (Generic_int.logand m (mask s2)) y)

	| Irg.BINOP (t, Irg.BIN_AND, e1, e2) ->
		(try
			let k = Sem.to_int32 (Sem.eval_const e2) in
			scan_decode_argument e1 (Generic_int.logand m (of_int32 k)) (and_ y (cst k))
		with Sem.SemError _ ->
			try
			let k = Sem.to_int32 (Sem.eval_const e1) in
			scan_decode_argument e2 (Generic_int.logand m (of_int32 k)) (and_ y (cst k))
			with Sem.SemError _ ->
				raise (Toc.PreError (asis "only forms as 'x & k' or 'k & x' are supported in image")))

	| Irg.BINOP (t, Irg.BIN_OR, e1, e2) ->
		let l1 = scan_decode_argument e1 m y in
		let l2 = scan_decode_argument e2 m y in
		let m1 = or_masks l1 Generic_int.zero in
		let m2 = or_masks l2 Generic_int.zero in
		let mr = Generic_int.logand m1 m2 in
		if Generic_int.is_equals mr Generic_int.zero then l1 @ l2
		else raise (Toc.PreError (asis "both parts of the OR must be independent in an image"))

	| _
		-> raise (Toc.PreError (asis "unsupported expression"))


(** Decode all arguments with the given initial values.
	@param args		Image arguments.
	@param vals		Value of the image arguments.
	@return			Triplet list of (parameter, mask, expression). *)
let scan_decode_arguments args vals =
	List.fold_left2
		(fun r a v -> r @ (scan_decode_argument a (mask_of_expr a) v))
		[] args vals


(** Build a list of pairs (parameter name, expression to decode it).
	@param params	List of parameter names.
	@param args		Argument value of the image format.
	@param vals		Expression to access the actual argument value.
	@return			Decoding pairs. *)
let decode_parameters params args vals =
	let t = scan_decode_arguments args vals in
	let rec process (p, m, e) (p', m', e') =
		if p <> p' then (p, m, e) else
		if is_null (Generic_int.logand m m')
		then (p, Generic_int.logor m m', or_ e (and_ e' (cst (to_int32 m'))))
		else raise (Toc.Error (Printf.sprintf "some parameter %s bits are redundant in image" p)) in
	List.map
		(fun p ->
			let (p, m, e) = List.fold_left process (p, Generic_int.zero, cst Int32.zero) t in
			let m' = mask (Sem.get_type_length (Sem.get_type_ident p)) in
			if equals m m' then  (p, e)
			else
				raise (Toc.Error (Printf.sprintf "some bits (%s) of parameter %s are missing (%s)"
					(Generic_int.to_string m') p (Generic_int.to_string m))))
		params


(*** testing part: comment it before archiving ***)
(*let test params args =
	Printf.printf "argument: ";
	List.iter (fun e -> Irg.print_expr e; print_string ", ") args;
	print_char '\n';
	let cnt = ref 0 in
	let vals = List.map (fun _ -> incr cnt; Irg.EINLINE (Printf.sprintf "p%d" !cnt)) args in
	let r = decode_parameters params args vals in
	List.iter
		(fun (n, e) ->
			Printf.printf "\tparameter %s: " n;
			Irg.print_expr e;
			print_char '\n')
		r;
	print_char '\n'

let c32 = Irg.CARD(16)
let ref_x = Irg.REF "x"
let ref_y = Irg.REF "y"

let _ =
	Irg.dump_type := false;
	Irg.add_symbol "x" (Irg.PARAM ("x", Irg.TYPE_EXPR c32));
	Irg.add_symbol "y" (Irg.PARAM ("y", Irg.TYPE_EXPR c32));
	test ["x"] [ref_x];
	test ["x"] [add ref_x (csti 1)];
	test ["x"] [shl ref_x (csti 4); getb ref_x (csti 15) (csti 12)];
	test ["x"] [shl (add ref_x (csti 1)) (csti 4); getb ref_x (csti 15) (csti 12)];
	test ["x"] [getb ref_x (csti 15) (csti 8); getb ref_x (csti 7) (csti 0)];
	test ["x"] [add (getb ref_x (csti 15) (csti 8)) (csti 3); getb ref_x (csti 7) (csti 0)];
	test ["x"; "y"] [concat ref_x ref_y];
	test ["x"; "y"] [concat (getb ref_x (csti 15) (csti 8)) ref_y; getb ref_x (csti 7) (csti 0)]
*)
