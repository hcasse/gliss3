(*
 * $Id: fetch.ml,v 1.11 2009/04/07 16:34:07 barre Exp $
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

(*
  Here is the module in charge of the generation of the fetch_table.h
*)
(*
  Useful list of dependencies in order to work with the interactive Ocaml toplevel :
  (Do not forget to do make to have the latest version of the cmo binaries)

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
*)

(* flag: output or not fetch tables stats *)
let output_fetch_stat    = ref false

(* return the string of a given Irg.expr which is supposed to be an image attribute *)
let rec get_str e =
	match e with
	| Irg.FORMAT(str, _) -> str
	| Irg.CONST(t_e, c) ->
		if t_e=Irg.STRING then
			match c with
			Irg.STRING_CONST(str, false, _) ->
				str
			| _ -> ""
		else
			""
	| Irg.ELINE(_, _, e) -> get_str e
	| _ -> ""


exception Bad_bit_image_order of string
(**
returns the bit_image_order defined in nmp sources,
used to know if images descriptions are written with bytes reversed compared to the memory disposition,
this happens with the tricore processor for example.
@return		0 if bit_image_order is 0 or undefined
		1 if bit_image_order is defined and not 0
@raise	Bad_bit_image_order	when incorrectly defined
 *)
let get_bit_image_order _ =
	try
		(match Irg.get_symbol "bit_image_order" with
		| Irg.LET(_, c) ->
			(match c with
			| Irg.CARD_CONST(i32) -> ((Int32.compare i32 Int32.zero) != 0)
			| _ -> raise (Bad_bit_image_order "bit_image_order can only be an boolean int constant.")
			)
		| _ -> raise (Bad_bit_image_order "bit_image_order must be defined as a let, if defined.")
		)
	with
	| Irg.Symbol_not_found _ -> false


(* return the length (in bits) of an argument whose param code (%8b e.g.) is given as a string *)
let get_length_from_format f =
	let l = String.length f in
	let new_f =
		if l<=2 then
		(* shouldn't happen, we should have only formats like %[0-9]+b, not %d or %f or %s *)
			failwith ("we shouldn't have something like [[" ^ f ^ "]] (fetch.ml::get_length_from_format)")
		else
			String.sub f 1 (l-2)
	in
	Scanf.sscanf new_f "%d" (fun x->x)


(* remove any space (space or tab char) in a string, return a string as result *)
let remove_space s =
	Str.global_replace (Str.regexp "[ \t]+") "" s


(** returns the mask of an op from its spec, the result will be a string
with only '0' or '1' chars representing the bits of the mask *)
let get_string_mask_from_op sp =
	let transform_str s =
		let n = String.length s in
		let rec aux str pos accu =
			if pos >= n then
				accu
			else
				begin
					(* a 'X' or 'x' in an image means a useless bit => not in the mask *)
					if s.[pos]='x' || s.[pos]='X' then
						accu.[pos] <- '0'
					else
						();
					aux s (pos+1) accu
				end
		in
		aux s 0 (String.make n '1')
	in
	let rec get_mask_from_regexp_list l =
		match l with
		[] -> ""
		| h::t ->
			(match h with
			Str.Text(txt) ->
				(* here we assume that an image contains only %.. , 01, X or x *)
				(transform_str txt) ^ (get_mask_from_regexp_list t)
			| Str.Delim(d) ->
				(String.make (get_length_from_format d) '0') ^ (get_mask_from_regexp_list t)
			)
	in
	let get_expr_from_iter_value v  =
		match v with
		Iter.EXPR(e) ->
			e
		| _ ->
			failwith "shouldn't happen (fetch.ml::get_string_mask_from_op::get_expr_from_iter_value)"
	in
	(* work only with strings with length multiple of 8, instructions have always length of 8*n bits so far *)
	let revert_bytes s =
		let rec aux length str =
			if length < 8 then
				""
			else
				(aux (length - 8) (String.sub str 8 (length - 8))) ^ (String.sub str 0 8)
		in
		if get_bit_image_order () then
			aux (String.length s) s
		else
			s
	in
	(* !!DEBUG!! *)
	(*print_string ((remove_space (get_str (get_expr_from_iter_value (Iter.get_attr sp "image"))))^"::");
	let res =*)
	revert_bytes (get_mask_from_regexp_list (Str.full_split (Str.regexp "%[0-9]*[bdfxs]") (remove_space (get_str (get_expr_from_iter_value (Iter.get_attr sp "image"))))))
	(*in
	let no_rev = get_mask_from_regexp_list (Str.full_split (Str.regexp "%[0-9]*[bdfxs]") (remove_space (get_str (get_expr_from_iter_value (Iter.get_attr sp "image")))))
	in print_string (no_rev ^ "::rev=" ^res^"\n");
	res*)


(** returns the length of a given instruction, based on the image description *)
let get_instruction_length sp =
	let rec get_length_from_regexp_list l =
		match l with
		[] -> 0
		| h::t ->
			(match h with
			Str.Text(txt) ->
				(* here we assume that an image contains only %.. , 01, X or x *)
				(String.length txt) + (get_length_from_regexp_list t)
			| Str.Delim(d) ->
				(get_length_from_format d) + (get_length_from_regexp_list t)
			)
	in
	let get_expr_from_iter_value v  =
		match v with
		Iter.EXPR(e) ->
			e
		| _ ->
			failwith "shouldn't happen (fetch.ml::get_instruction_length::get_expr_from_iter_value)"
	in
	get_length_from_regexp_list (Str.full_split (Str.regexp "%[0-9]*[bdfxs]") (remove_space (get_str (get_expr_from_iter_value (Iter.get_attr sp "image")))))


(* returns the value of an instruction code considering only the bit set in the mask,
the result is a '0' or '1' string with the bits not set in the mask being marked with an 'X' *)
let get_string_value_on_mask_from_op sp =
	let rec get_mask_from_regexp_list l =
		match l with
		| [] -> ""
		| h::t ->
			(match h with
			| Str.Text(txt) ->
				(* here we assume that an image contains only %.., 01, x or X *)
				txt ^ (get_mask_from_regexp_list t)
			| Str.Delim(d) ->
				(* put an X for unused bits *)
				(String.make (get_length_from_format d) 'X') ^ (get_mask_from_regexp_list t)
			)
	in
	let get_expr_from_iter_value v =
		match v with
		| Iter.EXPR(e) ->
			e
		| _ ->
			failwith "shouldn't happen (fetch.ml::get_string_value_on_mask_from_op::get_expr_from_iter_value)"
	in
	(* work only with strings with length multiple of 8, instructions have always length of 8*n bits so far *)
	let revert_bytes s =
		let rec aux length str =
			if length < 8 then
				""
			else
				(aux (length - 8) (String.sub str 8 (length - 8))) ^ (String.sub str 0 8)
		in
		if get_bit_image_order () then
			aux (String.length s) s
		else
			s
	in
	revert_bytes (get_mask_from_regexp_list (Str.full_split (Str.regexp "%[0-9]*[bdfxs]") (remove_space (get_str (get_expr_from_iter_value (Iter.get_attr sp "image"))))))


(* returns the value of an instruction code considering only the bit set in the mask,
the result is a '0' or '1' string with the bits not set in the mask being discarded,
so the result will have as many bits as the number of set bits in the mask *)
let get_value_on_mask sp =
	Str.global_replace (Str.regexp "X+") "" (get_string_value_on_mask_from_op sp)


(** returns the string result of a logical AND between 2 '0' or '1' string represented numbers,
    length will be the min between v1 and v2 lengths
*)
let compute_logical_and v1 v2 =
	let l = min (String.length v1) (String.length v2) in
	let rec aux accu step =
		if step >= l then
			accu
		else
			(if v2.[step] == '1' then
				(if v1.[step] == 'X' || v2.[step] == 'X' then
					failwith "shouldn't happen (fetch.ml::compute_logical_and)"
				else
					aux (accu ^ (String.make 1 v1.[step])) (step + 1))
			else
				aux (accu ^ "0") (step + 1))
	in
	aux "" 0


(** returns a '0' or '1' string from v, the bits returned are those indicated by the bits set in mask,
    bits are concatenated and length will be the min between v and mask lengths
*)
let compute_value_with_mask v mask =
	let l = min (String.length v) (String.length mask) in
	let rec aux accu step =
		if step >= l then
			accu
		else
			(if mask.[step] == '1' then
				(if v.[step] == 'X' then
					failwith "shouldn't happen (fetch.ml::compute_value_with_mask)"
				else
					aux (accu ^ (String.make 1 v.[step])) (step + 1))
			else
				aux accu (step + 1))
	in
	aux "" 0


(** returns v (representing a mask) with the mask "removed", if a bit is set in the mask it will be 0 in the result,
    otherwise it will the corresponding bit of v, the result will have same length as v
*)
let compute_value_without_mask v mask =
	let lv = String.length v in
	let l = min lv (String.length mask) in
	let rec aux accu step =
		if step >= l then
			(if lv > l then
				accu ^ (String.sub v step (lv - step))
			else
				accu)
		else
			if mask.[step] == '0' then
				aux (accu ^ (String.make 1 v.[step])) (step + 1)
			else
				aux (accu ^ "0") (step + 1)
	in
	aux "" 0


(** simple equality between 2 strings
*)
let string_is_equals s1 s2 =
	((String.compare s1 s2) == 0)


(** convert the 1st 32 chars of a string to an int32
the string is supposed to represent a binary number (only 0 and 1) of less than 32 bits *)
let str01_to_int32 s =
	let size = String.length s
	in
	let char01_to_int32 c =
		match c with
		'0' ->
			Int32.zero
		| '1' ->
			Int32.one
		| _ ->
			failwith ("we shouldn't have this char (" ^ (String.make 1 c) ^ ") here (fetch.ml::str01_to_int32)")
	in
	let rec aux s n accu =
		if n = size then
			accu
		else
			aux s (n+1) (Int32.add (Int32.shift_left accu 1) (char01_to_int32 s.[n]))
	in
	if size > 32 then
		failwith "string too long, 32 chars max allowed (fetch.ml::str01_to_int32)"
	else
		aux s 0 Int32.zero


(** convert the 1st n chars of a string to an int_n
the string is supposed to represent a binary number (only 0 and 1) *)
let str_to_int_n s n =
	let s_size = String.length s in
	let size = if s_size < n then s_size else n in
	let char01_to_int32 c =
		match c with
		| '0' ->
			Int32.zero
		| '1' ->
			Int32.one
		| _ ->
			failwith ("we shouldn't have this char (" ^ (String.make 1 c) ^ ") here (fetch.ml::str01_to_int_n)") in
	let rec aux s n accu =
		if n = size then
			accu
		else
			aux s (n+1) (Generic_int.set_lowest_bit (Generic_int.shift_left accu 1) (char01_to_int32 s.[n])) in
	aux s 0 Generic_int.zero


(** convert a string to an gen_int
the string is supposed to represent a binary number (only 0 and 1) *)
let str_to_gen_int s =
	let size = String.length s in
	let char01_to_int32 c =
		match c with
		| '0' -> Int32.zero
		| '1' -> Int32.one
		| _ -> failwith ("we shouldn't have this char (" ^ (String.make 1 c) ^ ") here (fetch.ml::str01_to_int_n)") in
	let rec aux s n accu =
		if n = size then
			accu
		else
			aux s (n+1) (Generic_int.set_lowest_bit (Generic_int.shift_left accu 1) (char01_to_int32 s.[n])) in
	aux s 0 Generic_int.zero


let str_to_int32_list s =
	let l = String.length s in
	let rec aux accu step =
		match l - step with
		| n when n == 0 -> accu (* end of string *)
		| n when n >= 32 -> aux (accu @ [(Int32.of_string ("0b" ^ (String.sub s step 32)))]) (step + 32)
		| n when n > 0 && n < 32 -> accu @ [Int32.shift_left (Int32.of_string ("0b" ^ (String.sub s step n))) (32 - n)] (* less than 32 bits from end of string *)
		| _ -> failwith "shouldn't happen (fetch.ml::str_to_int32_list)"
	in
	aux [] 0


(* produces the suffix needed in C for the given string number translated in C, currently only suffix for 64 bit const is returned *)
let get_C_const_suffix s =
	let l = String.length s in
	if l > 32 && l <= 64 then "LL" else ""



(* from here we assume we will deal only with 32bit instrs (32 bit optimized decode) or variable length ISAs *)

(*****************************************************************
 * 32 bit, NO MORE USED, everything is generated by generic part
 ****************************************************************)

(* for the next 2 functions, produce a warning if the instruction is more than 32 bit long *)

(* returns the 32 bit mask of a 32 bit instruction *)
let get_int32_mask sp =
	let mask = get_string_mask_from_op sp
	in
	let rec build_mask str pos accu =
		if pos >= String.length str then
			accu
		else
			build_mask str (pos+1) (Int32.add (Int32.shift_left accu 1) (if str.[pos]='1' then Int32.one else Int32.zero))
	in
	if String.length mask > 32 then
		failwith "instruction too long, 32 bits max allowed (fetch.ml::get_int32_mask)"
	else
		build_mask mask 0 Int32.zero


(** returns the 32 bit value of a 32 bit instruction code indicated by the set bits in the spec's mask, the bits not set in the mask are cleared to 0 *)
let get_int32_value sp =
	let v = get_string_value_on_mask_from_op sp
	in
	let rec build_mask str pos accu =
		if pos >= String.length str then
			accu
		else
			build_mask str (pos+1) (Int32.add (Int32.shift_left accu 1) (if str.[pos]='1' then Int32.one else Int32.zero))
	in
	if String.length v > 32 then
		failwith "instruction too long, 32 bits max allowed (fetch.ml::get_int32_value)"
	else
		build_mask v 0 Int32.zero


(** return the bits in a value num, only those indicated by the set bits in a given mask, bit are concatenated *)
let keep_value_on_mask num mask =
	let rec aux n m accu step numbit =
		if step>=32 then
			accu
		else
			if (Int32.rem m (Int32.of_int 2)) = Int32.zero then
				aux (Int32.shift_right_logical n 1) (Int32.shift_right_logical m 1) accu (step+1) numbit
			else
				if (Int32.rem n (Int32.of_int 2)) = Int32.one then
					aux (Int32.shift_right_logical n 1) (Int32.shift_right_logical m 1) (Int32.logor accu (Int32.shift_left Int32.one numbit)) (step+1) (numbit+1)
				else
					aux (Int32.shift_right_logical n 1) (Int32.shift_right_logical m 1) accu (step+1) (numbit+1)
	in
	aux num mask Int32.zero 0 0


(** perform an AND between all the mask of the instrs in spec_list, ignoring the bits in top_mask *)
let rec calcul_mask spec_list top_mask =
	match spec_list with
	[] ->
		Int32.lognot top_mask
	| h::t ->
		Int32.logand (get_int32_mask h) (calcul_mask t top_mask)


let calcul_value_on_mask sp mask =
	keep_value_on_mask (Int32.logand (get_int32_value sp) mask) mask


type dec_tree = DecTree of int32 list * Irg.spec list * int32 * int32 * dec_tree list


let print_dec_tree tr =
	let name_of t =
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (if (String.length s)=0 then (string_of_int (Int32.to_int a)) else (s^"_"^(string_of_int (Int32.to_int a))))
		in
		match t with
		DecTree(i, s, m, g, d) ->
			aux i ""
	in
	match tr with
	DecTree(int_l, sl, msk, gm, dt_l) ->
		begin
		Printf.printf "================================================================\nPrinting tree, dectree's name : %s\n" (name_of tr);
		Printf.printf "mask : %lX\n" msk;
		Printf.printf "global : %lX\n" gm;
		Printf.printf "spec : ";
		List.iter (fun x -> Printf.printf "\t%s, mask=%s, val=%s\n" (Iter.get_name x) (get_string_mask_from_op x) (get_string_value_on_mask_from_op x)) sl;
		(*Printf.printf "\n"*)
		end


let print_dec_tree_list tl =
	List.iter (fun x -> begin print_char '\n'; print_dec_tree x end) tl


(* returns the amount of set bits in a 32 bit number, help to get mask's "length" *)
let get_amount_of_set_bits num =
	let rec aux n accu step =
		if step >= 32 then
			accu
		else
			aux (Int32.shift_right_logical n 1) (accu + (Int32.to_int (Int32.rem n (Int32.of_int 2)))) (step+1)
	in
	aux num 0 0


let get_local_mask_length dt =
	match dt with
	DecTree(_, _, ml, _, _) ->
		get_amount_of_set_bits ml


let get_global_mask_length dt =
	match dt with
	DecTree(_, _, _, mg, _) ->
		get_amount_of_set_bits mg

let get_instr_list dt =
	match dt with
	DecTree(_, sl, _, _, _) ->
		sl


(* return a list like [(v_i, i_i),...]
the v_is will describe all mask values, i_i will be an associated instruction,
several tuples with the same v_i can appear for different instructions *)
let create_son_list_of_dec_node dt =
	let rec aux msk sl =
		(match sl with
		[] ->
			(* one instr => terminal node *)
			[]
		| a::b ->
			(* !!DEBUG!! *)
			(*print_string "\ncreating dec_node son, val_on_mask=";
			Printf.printf "%lX, spec=%s\n" (calcul_value_on_mask a msk) (Iter.get_name a);*)
			((calcul_value_on_mask a msk), a)::(aux msk b)
		)
	in
	match dt with
	DecTree(i_l, s_l, msk, gm, dt_l) ->
		aux msk s_l


(* take the result given by the previous function and returns a list of tuples of the type
(v_i, [i0,i1,...,in]) [ik] being the list of all instr with v_i as value on mask *)
let sort_son_list vl =
	(* add one instr with a given value on mask to a tuple list (with inst list like the top result expected) *)
	let rec add_instr_in_tuple_list l (v, sp) =
		match l with
		[] ->
			[(v, [sp])]
		| a::b ->
			(match a with
			(vv, sl) ->
				if (Int32.compare v vv) = 0 then
					(vv, sp::sl)::b
				else
					a::(add_instr_in_tuple_list b (v,sp))
			)
	in
	List.fold_left add_instr_in_tuple_list [] vl


let rec build_dectrees vl msk gm il =
	match vl with
	[] ->
		[]
	| a::b ->
		match a with
		(v, sl) ->
			let dt = DecTree(il@[v], sl, Int32.logand (calcul_mask sl Int32.zero) (Int32.lognot gm), calcul_mask sl Int32.zero, [])(* ::(build_dectrees b msk gm il) *)
			in
			(* !!DEBUG!! *)
			(*print_dec_tree dt;*)
			dt::(build_dectrees b msk gm il)


let build_sons_of_tree tr =
	match tr with
	DecTree(int_l, sl, msk, gm, dt_l) ->
		let res = build_dectrees (sort_son_list (create_son_list_of_dec_node tr)) msk gm int_l
		in
		(* !!DEBUG!! *)
		(*Printf.printf "build_sons, %d in father, %d sons\n" (List.length sl) (List.length res);flush stdout;*)
		if (List.length res) == 1 && (List.length (get_instr_list (List.hd res))) > 1 then
			begin
			output_string stderr "ERROR: some instructions seem to have same image.\n";
			output_string stderr "here is the list: ";
			List.iter (fun x -> Printf.fprintf stderr "%s, " (Iter.get_name x)) (get_instr_list (List.hd res));
			output_string stderr "\n";
			failwith "cannot continue with 2 instructions with same image"
			end
		else
			res


let build_dec_nodes m =
	let list_of_all_op_specs n =
		match n with
		0 ->
			Iter.iter (fun a x -> x::a) []
		| _ ->
			[]
	in
	let node_cond (DecTree(_, sl, lmask, gmask, _)) =
		((List.length sl)<=1)
	in
	let rec stop_cond l =
		match l with
		[] ->
			(*print_string "stop_cond=true\n";flush stdout;*)
			true
		| a::b ->
			if node_cond a then
			(*begin print_string "stop_cond=[rec]\n";flush stdout;*)
				stop_cond b (*end*)
			else
			(*begin print_string "stop_cond=false\n";flush stdout;*)
				false (*end*)
	in
	let get_sons x =
		match x with
		DecTree(int_l, sl, msk, gm, dt_l) ->
			if (List.length sl)>1 then
				DecTree(int_l, [], msk, gm, dt_l)::(build_sons_of_tree x)
			else
				[x]
	in
	let rec aux dl =
		if stop_cond dl then
			dl
		else
			aux (List.flatten (List.map get_sons dl))
	in
	match m with
	0 ->
		let specs = list_of_all_op_specs 0 in
		let mask = calcul_mask specs Int32.zero in
		aux [DecTree([], specs, mask, mask, [])]
	| _ ->
		[]


(* returns a list of the direct sons of a given DecTree among a given list *)
let find_sons_of_node node d_l =
	let get_name d =
		match d with
		DecTree(name, _, _, _, _) ->
			name
	in
	let length_of_name d =
		List.length (get_name d)
	in
	(* return true if l1 is a sub list at the beginning of l2,
	ie if l2 can be the name of a son (direct or not) of a DecTree whose name is l1,
	l1 and l2 are supposed to be two int list representing a name of a DecTree *)
	let rec is_sub_name l1 l2 =
		match l1 with
		[] ->
			true
		| a1::b1 ->
			(match l2 with
			[] ->
				false
			| a2::b2 ->
				if a1=a2 then
					(is_sub_name b1 b2)
				else
					false
			)
	in
	(* return true if d1 is a direct son of d2, false otherwise *)
	let is_son d1 d2 =
		if (length_of_name d1) = ((length_of_name d2) + 1) then
			if (is_sub_name (get_name d2) (get_name d1)) then
				true
			else
				false
		else
			false
	in
	List.flatten (List.map (fun d -> if (is_son d node) then [d] else [] ) d_l)


(* outputs the declaration of all structures related to the given DecTree dt in C language,
all needed Decode_Ent and Table_Decodage structures will be output and already initialised,
everything will be output in the given channel,
dl is the global list of all nodes, used to find sons for instance *)
let output_table_C_decl out dt dl =
	let name_of t =
		let correct_name s =
			if s = "" then
				s
			else
				"_"^s
		in
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (if (String.length s)=0 then (Printf.sprintf "%lu" a) else (s^"_"^(Printf.sprintf "%lu" a)))
		in
		match t with
		DecTree(i, s, m, g, d) ->
			correct_name (aux i "")
	in
	let sz_l_mask = get_local_mask_length dt
	in
	let num_dec_ent = Int32.to_int (Int32.shift_left Int32.one sz_l_mask)
	in
	let name = name_of dt
	in
	let l_mask =
		match dt with
		DecTree(_, _, lm, _, _) ->
			lm
	in
	let info = Toc.info ()
	in
	let sons = find_sons_of_node dt dl
	in
	(* is i the last element of i_l? i is an int32, i_l is an int32 list *)
	let rec is_suffix i i_l =
		match i_l with
		| a::b ->
			if b=[] then
				i=a
			else
				is_suffix i b
		| [] ->
			false
	in
	let exists_in i d_l =
		let predicate x =
			match x with
			DecTree(i_l, _, _, _, _) ->
				is_suffix (Int32.of_int i) i_l
		in
		List.exists predicate d_l
	in
	let get_i_th_son i d_l =
		let predicate x =
			match x with
			DecTree(i_l, _, _, _, _) ->
				is_suffix (Int32.of_int i) i_l
		in
		List.find predicate d_l
	in
	(* the way the nodes are built implies that a terminal node is a node containing spec of 1 instruction, the other nodes being "empty" *)
	let is_terminal_node d =
		match d with
		DecTree(_, s, _, _, _) ->
			s != []
	in
	(* returns the spec of a supposed terminal node *)
	let get_spec_of_term d =
		match d with
		DecTree(_, s, _, _, _) ->
			List.hd s
	in
	let produce_i_th_son i =
		if exists_in i sons then
			if is_terminal_node (get_i_th_son i sons) then
			(* TODO: decode or not decode ? *)
				let x = get_spec_of_term (get_i_th_son i sons)
				in
				begin
				Printf.fprintf out "/* 0X%X,%d */\t{INSTRUCTION, (void *)%s}" i i ((String.uppercase info.Toc.proc) ^ "_" ^ (String.uppercase (Iter.get_name x)));
				Printf.fprintf out "\t/* %s, %d bits, mask=%s, val=%s */" (String.uppercase (Iter.get_name x)) (get_instruction_length x) (get_string_mask_from_op x) (get_string_value_on_mask_from_op x)
				end
			else
				Printf.fprintf out "/* 0X%X,%d */\t{TABLEFETCH, &_table%s}" i i (name_of (get_i_th_son i sons))
		else
			Printf.fprintf out "{INSTRUCTION, %s_UNKNOWN}" (String.uppercase info.Toc.proc)
	in
	let rec produce_decode_ent i =
		if i >= num_dec_ent then
			()
		else
			begin
			Printf.fprintf out "\t";
			produce_i_th_son i;
			if i = (num_dec_ent-1) then
				Printf.fprintf out "\n"
			else
				Printf.fprintf out ",\n";
			produce_decode_ent (i+1)
			end
	in
	(* !!DEBUG!! *)
	(*match dt with
	DecTree(i_l, s_l, lm, gm, ss) ->
	print_string ("node treated[" ^ (name_of dt) ^ "], spec=[");
	List.iter (fun x -> (Printf.printf "(%s), " (Iter.get_name x))) s_l;
	print_string  "], sons=[";
	List.iter (fun x -> (Printf.printf "(%s), " (name_of x))) ss;
	print_string "]\n";*)
	if is_terminal_node dt then
		(* !!DEBUG!! *)
		(*print_string ((name_of dt) ^ ": [[terminal node]]\n")*)
		()
	else
		begin
		(* !!DEBUG!! *)
		(*print_string ((name_of dt) ^ ": [[normal node]]\n");*)
(*!!TODO!! param with gen/not gen*)
		Printf.fprintf out "static Decode_Ent table_table%s[%d] = {\n" name num_dec_ent;
		produce_decode_ent 0;
		Printf.fprintf out "};\n";
		Printf.fprintf out "static Table_Decodage _table%s = {0X%lX, table_table%s};\n" name l_mask name;
		Printf.fprintf out "static Table_Decodage *table%s = &_table%s;\n" name name;
		Printf.fprintf out "\n"
		end


let sort_dectree_list d_l =
	let name_of t =
		match t with
		DecTree(i, _, _, _, _) ->
			i
	in
	(* same specification as a standard compare function, x>y means x is "bigger" than y (eg: 12_3 < 13_3_5, "nothing" < x for any x) *)
	let rec comp_int32_list x y =
		match x with
		[] ->
			-1
		| x1::x2 ->
			(match y with
			[] ->
				1
			| y1::y2 ->
				let diff = Int32.compare x1 y1
				in
				if diff=0 then
					comp_int32_list x2 y2
				else
					diff
			)
	in
	let comp_fun x y =
		comp_int32_list (name_of y) (name_of x)
	in
	List.sort comp_fun d_l


(*********************************************************************
 * variable length
 ********************************************************************)


(** return the mask of an arbitrary length instruction as a generic int *)
let get_int_mask sp =
	(* !!DEBUG!! *)
	(*let res =*)
	str_to_gen_int (get_string_mask_from_op sp)
	(*in
	print_string ("get_int_mask, mask="^(get_string_mask_from_op sp)^", res =(");
	Printf.printf "%d" (Generic_int.length res);
	print_string (")"^(Generic_int.to_string res)^"\n");
	res*)


(** returns the value of an instruction code indicated by the set bits in the spec's mask, the bits not set in the mask are cleared to 0, result is a generic int *)
let get_int_value sp =
	str_to_gen_int (get_string_value_on_mask_from_op sp)


(** perform an AND between the mask of all the instrs in spec_list *)
let rec calcul_mask_gen sp_list =
(* !!DEBUG!! *)
	let rec aux sl =
	match sl with
	| [] -> ""
	| [a] -> get_string_mask_from_op a
	| h::t -> (* !!DEBUG!! *)(*Printf.printf "%20s-%s\n" (Irg.name_of h) (get_string_mask_from_op h); *)compute_logical_and (get_string_mask_from_op h) (aux t)
	in
	let res = aux sp_list in
	(* !!DEBUG!! *)(*Printf.printf "calcul_mask_gen, #%d, res=%s\n" (List.length sp_list) res;*)
	res


(* return the bits in a value num, only those indicated by the set bits in a given mask, bits are concatenated *)
(*let keep_value_on_mask_gen num mask =
	let max_step = Generic_int.length mask in
	let rec aux n m accu step numbit =
		if step >= max_step then
			accu
		else
			if Generic_int.get_lowest_bit m = Int32.zero then
				aux (Generic_int.shift_right_logical n 1) (Generic_int.shift_right_logical m 1) accu (step+1) numbit
			else
				if Generic_int.get_lowest_bit n = Int32.one then
					aux (Generic_int.shift_right_logical n 1) (Generic_int.shift_right_logical m 1) (Generic_int.logor accu (Generic_int.shift_left Generic_int.one numbit)) (step+1) (numbit+1)
				else
					aux (Generic_int.shift_right_logical n 1) (Generic_int.shift_right_logical m 1) accu (step+1) (numbit+1)
	in
	aux num mask Generic_int.zero 0 0*)


(** get the value of the given spec image on the given mask, bit not in the mask are discarded and so the result is the concatenation of the masked bits *)
let calcul_value_on_mask_gen sp mask =
	compute_value_with_mask (get_string_value_on_mask_from_op sp) mask
(*	keep_value_on_mask_gen (Generic_int.logand (get_int_value sp) mask) mask*)



(* "name" of the tree (list of int : all vals on mask beginning from the top), list of the instr, local mask, global mask (from ancestors), list of sons *)

(*type dec_tree_gen = DecTree_ of Generic_int.gen_int list * Irg.spec list * Generic_int.gen_int * Generic_int.gen_int * dec_tree_gen list*)
type dec_tree_gen = DecTree_ of string list * Irg.spec list * string * string * dec_tree_gen list


let print_dec_tree_gen tr =
	let name_of t =
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (if (String.length s)=0 then a else (s ^ "_" ^ a))
		in
		match t with
		DecTree_(i, s, m, g, d) ->
			aux i ""
	in
	match tr with
	DecTree_(int_l, sl, msk, gm, dt_l) ->
		begin
		Printf.printf "================================================================\nPrinting tree, dectree's name : %s\n" (name_of tr);
		Printf.printf "mask   : %s\n" msk;
		Printf.printf "global : %s\n" gm;
		Printf.printf "spec : ";
		if sl == [] then print_string "<none>\n" else 
		List.iter (fun x -> Printf.printf "\t%12s%20s, mask=%s, val=%s, val_mask=%s\n"
			(Irg.name_of x) (Iter.get_name x) (get_string_mask_from_op x) (get_string_value_on_mask_from_op x) (get_value_on_mask x)) sl;
		(*Printf.printf "\n"*)
		end


let print_dec_tree_list_gen tl =
	List.iter (fun x -> begin print_char '\n'; print_dec_tree_gen x end) tl

	
(* returns the amount of set bits in a string int, help to get mask's "length" *)
let get_amount_of_set_bits_gen num =
	let max_step = String.length num in
	let rec aux accu step =
		if step >= max_step then
			accu
		else
			aux (accu + (if num.[step] == '1' then 1 else 0)) (step + 1)
	in
	aux 0 0



let get_local_mask_length_gen dt =
	match dt with
	DecTree_(_, _, ml, _, _) ->
		get_amount_of_set_bits_gen ml


let get_global_mask_length_gen dt =
	match dt with
	DecTree_(_, _, _, mg, _) ->
		get_amount_of_set_bits_gen mg

let get_instr_list_gen dt =
	match dt with
	DecTree_(_, sl, _, _, _) ->
		sl


let create_son_list_of_dec_node_gen dt =
	let rec aux msk sl =
		(match sl with
		[] ->
			(* one instr => terminal node *)
			[]
		| a::b ->
			(* !!DEBUG!! *)
			(*print_string ("\ncreating dec_node son, mask="^msk^", val_on_mask=");
			Printf.printf "%s, inst_mask=%s, inst_val=%s, spec=%s\n" (calcul_value_on_mask_gen a msk) (get_string_mask_from_op a) (get_string_value_on_mask_from_op a) (Irg.name_of a);*)
			((calcul_value_on_mask_gen a msk), a)::(aux msk b)
		)
	in
	match dt with
	DecTree_(i_l, s_l, msk, gm, dt_l) ->
		aux msk s_l


let sort_son_list_gen vl =
	(* add one instr with a given value on mask to a tuple list (with inst list like the top result expected) *)
	let rec add_instr_in_tuple_list l (v, sp) =
		match l with
		[] ->
			[(v, [sp])]
		| a::b ->
			(match a with
			(vv, sl) ->
				if string_is_equals v vv then
					(vv, sp::sl)::b
				else
					a::(add_instr_in_tuple_list b (v,sp))
			)
	in
	List.fold_left add_instr_in_tuple_list [] vl


(* with the result of the previous function we can build the list of dec_tree associated,
we just have to calculate the local masks to do this we need the father's masks,
we also need the father's vals on mask to add the new one,
by default all trees will be created with no link between them (no tree structure) *)
let rec build_dectrees_gen vl msk gm il =
	match vl with
	[] ->
		[]
	| a::b ->
		match a with
		(v, sl) ->
			(*print_string "build_dectrees_gen\n";*)
			let common_mask = calcul_mask_gen sl in
			let dt = DecTree_(il@[v], sl, compute_value_without_mask common_mask gm, common_mask, [])(* ::(build_dectrees b msk gm il) *)
			in
			(* !!DEBUG!! *)
			(*print_dec_tree_gen dt;
			print_string "build_tree_gen, sl=[";
			List.iter (fun x -> Printf.printf "%s," (Irg.name_of x)) sl;
			print_string "]\n";*)
			dt::(build_dectrees_gen b msk gm il)




let build_sons_of_tree_gen tr =
	match tr with
	DecTree_(int_l, sl, msk, gm, dt_l) ->
		let res = build_dectrees_gen (sort_son_list_gen (create_son_list_of_dec_node_gen tr)) msk gm int_l
		in
		(* !!DEBUG!! *)
		(*Printf.printf "build_sons_gen, %d in father, %d sons\n" (List.length sl) (List.length res);*)
		if (List.length res) == 1 && (List.length (get_instr_list_gen (List.hd res))) > 1 then
			begin
			output_string stderr "ERROR: some instructions seem to have same image.\n";
			output_string stderr "here is the list: ";
			List.iter (fun x -> Printf.fprintf stderr "%s, " (Irg.name_of x)) (get_instr_list_gen (List.hd res));
			List.iter (fun x -> (Irg.print_spec x)) (get_instr_list_gen (List.hd res));
			output_string stderr "\n";
			raise (Sys_error "cannot continue with 2 instructions with same image")
			end
		else
			res


let build_dec_nodes_gen _ =
	let list_of_all_op_specs _ =
			Iter.iter (fun a x -> x::a) []
	in
	let node_cond (DecTree_(_, sl, lmask, gmask, _)) =
		((List.length sl)<=1)
	in
	let rec stop_cond l =
		match l with
		| [] ->
			(*print_string "stop_cond=true\n";flush stdout;*)
			true
		| a::b ->
			if node_cond a then
			(*begin print_string "stop_cond=[rec]\n";flush stdout;*)
				stop_cond b (*end*)
			else
			(*begin print_string "stop_cond=false\n";flush stdout;*)
				false (*end*)
	in
	let get_sons x =
		match x with
		| DecTree_(int_l, sl, msk, gm, dt_l) ->
			if (List.length sl)>1 then
				DecTree_(int_l, [], msk, gm, dt_l)::(build_sons_of_tree_gen x)
			else
				[x]
	in
	let rec aux dl =
		if stop_cond dl then
			dl
		else
			aux (List.flatten (List.map get_sons dl))
	in
	let specs = list_of_all_op_specs () in
	let mask = calcul_mask_gen specs in
	aux [DecTree_([], specs, mask, mask, [])]


(* returns a list of the direct sons of a given DecTree among a given list *)
let find_sons_of_node_gen node d_l =
	let get_name d =
		match d with
		DecTree_(name, _, _, _, _) ->
			name
	in
	let length_of_name d =
		List.length (get_name d)
	in
	(* return true if l1 is a sub list at the beginning of l2,
	ie if l2 can be the name of a son (direct or not) of a DecTree whose name is l1,
	l1 and l2 are supposed to be two int list representing a name of a DecTree *)
	let rec is_sub_name l1 l2 =
		match l1 with
		[] ->
			true
		| a1::b1 ->
			(match l2 with
			[] ->
				false
			| a2::b2 ->
				if string_is_equals a1 a2 then
					(is_sub_name b1 b2)
				else
					false
			)
	in
	(* return true if d1 is a direct son of d2, false otherwise *)
	let is_son d1 d2 =
		if (length_of_name d1) = ((length_of_name d2) + 1) then
			if (is_sub_name (get_name d2) (get_name d1)) then
				true
			else
				false
		else
			false
	in
	List.flatten (List.map (fun d -> if (is_son d node) then [d] else [] ) d_l)


exception CheckIsizeException
(* special value for fetch size, represent generic fetch *)
let fetch_generic = 0


(* outputs the declaration of all structures related to the given DecTree dt in C language,
all needed Decode_Ent and Table_Decodage structures will be output and already initialised,
everything will be output in the given channel,
dl is the global list of all nodes, used to find sons for instance *)
let output_table_C_decl_gen fetch_size out fetch_stat dt dl =
	let name_of t =
		let correct_name s =
			if s = "" then
				s
			else
				"_" ^ s
		in
		let rec aux l s =
			match l with
			| [] -> s
			| a::b -> aux b (if (String.length s) == 0 then a else (s ^ "_" ^ a))
		in
		match t with
		| DecTree_(i, s, m, g, d) -> correct_name (aux i "")
	in
	let sz_l_mask = get_local_mask_length_gen dt in
	let num_dec_ent =
	(* we hope we never have a too big mask, we don't want to produce
	 * fetch tables with millions of entries (most of them void).
	 * if that unfortunate case happen => change the algorithm *)
	(* don't forget also that caml int are 31 bit long,
	 * let's limit ourself below this limit *)
	 (* deactivated temporarily for testing with nml with only 1 instr (==> full length mask) *)
		if sz_l_mask > 32 then
			failwith "shouldn't happen? mask too big (fetch.ml::output_table_C_decl_gen::num_dec_ent)"
		else
			1 lsl sz_l_mask
	in
	let name = name_of dt in
	let l_mask =
		match dt with
		| DecTree_(_, _, lm, _, _) -> lm
	in
	let info = Toc.info () in
	let sons = find_sons_of_node_gen dt dl in
	(* is i the last element of i_l? i is an int, i_l is an string int list *)
	let rec is_suffix i i_l =
		match i_l with
		| a::b ->
			if b=[] then
				i == (int_of_string ("0b" ^ a))
			else
				is_suffix i b
		| [] -> false
	in
	let exists_in i d_l =
		let predicate x =
			match x with
			| DecTree_(i_l, _, _, _, _) -> is_suffix i i_l
		in
		List.exists predicate d_l
	in
	let get_i_th_son i d_l =
		let predicate x =
			match x with
			| DecTree_(i_l, _, _, _, _) -> is_suffix i i_l
		in
		List.find predicate d_l
	in (* the way the nodes are built implies that a terminal node is a node containing spec of 1 instruction, the other nodes being "empty" *)
	let is_terminal_node d =
		match d with
		| DecTree_(_, s, _, _, _) -> s != []
	in
	(* returns the spec of a supposed terminal node *)
	let get_spec_of_term d =
		match d with
		| DecTree_(_, s, _, _, _) -> List.hd s
	in
	(* returns the number of instruction nodes produced *)
	let produce_i_th_son i =
		if exists_in i sons then
			if is_terminal_node (get_i_th_son i sons) then
			(* TODO: decode or not decode ? *)
				(let x = get_spec_of_term (get_i_th_son i sons)
				in
				Printf.fprintf out "/* 0X%X,%d */\t{INSTRUCTION, (void *)%s}" i i ((String.uppercase info.Toc.proc) ^ "_" ^ (String.uppercase (Iter.get_name x)));
				Printf.fprintf out "\t/* %s, %d bits, mask=%s, val=%s */" (String.uppercase (Iter.get_name x)) (get_instruction_length x) (get_string_mask_from_op x) (get_string_value_on_mask_from_op x);
				1)
			else
				(Printf.fprintf out "/* 0X%X,%d */\t{TABLEFETCH, &_table%s}" i i (name_of (get_i_th_son i sons));
				0)
		else
			(Printf.fprintf out "{INSTRUCTION, %s_UNKNOWN}" (String.uppercase info.Toc.proc);
			0)
	in
	let rec produce_decode_ent i nb_nodes =
		if i >= num_dec_ent then
			nb_nodes
		else
			(Printf.fprintf out "\t";
			let nb = produce_i_th_son i in
			if i = (num_dec_ent-1) then
				Printf.fprintf out "\n"
			else
				Printf.fprintf out ",\n";
			produce_decode_ent (i+1) (nb_nodes + nb))
	in
	let to_C_list mask =
		let list = str_to_int32_list mask in
		let rec aux comma l =
			match l with
			| [] -> ""
			| a::b ->
				((if comma then ", " else "") ^ (Printf.sprintf "0X%lX" a)) ^ (aux true b)
		in
			aux false list
	in
	(* !!DEBUG!! *)(*
	match dt with
	DecTree(i_l, s_l, lm, gm, ss) ->
	print_string ("node treated[" ^ (name_of dt) ^ "], spec=[");
	List.iter (fun x -> (Printf.printf "(%s), " (Iter.get_name x))) s_l;
	print_string  "], sons=[";
	List.iter (fun x -> (Printf.printf "(%s), " (name_of x))) ss;
	print_string "]\n";*)
	if is_terminal_node dt then
		(* !!DEBUG!! *)
		(*print_string ((name_of dt) ^ ": [[terminal node]]\n")*)
		()
	else
		(* !!DEBUG!! *)
		(*print_string ((name_of dt) ^ ": [[normal node]]\n");*)
( (*print_string "tree_to_C :"; print_dec_tree_gen dt;*)
		Printf.fprintf out "static Decode_Ent table_table%s[%d] = {\n" name num_dec_ent;
		let nb_nodes = produce_decode_ent 0 0 in
		Printf.fprintf out "};\n";
		if fetch_size != fetch_generic then
			Printf.fprintf out "static Table_Decodage _table%s = {0X%lX%s, table_table%s};\n" name (str01_to_int32 l_mask) (get_C_const_suffix l_mask) name
		else
			(Printf.fprintf out "static uint32_t tab_mask%s[%d] = {%s};\n" name (List.length (str_to_int32_list l_mask)) (to_C_list l_mask);
			Printf.fprintf out "static mask_t mask%s = {\n\ttab_mask%s," name name;
			Printf.fprintf out "\t%d};\n" (String.length l_mask);
			Printf.fprintf out "static Table_Decodage _table%s = {&mask%s, table_table%s};\n" name name name
			);
		Printf.fprintf out "static Table_Decodage *table%s = &_table%s;\n" name name;
		Printf.fprintf out "\n";

		if !output_fetch_stat then
			Printf.fprintf fetch_stat "%8d/%8d, name=%s\n" nb_nodes num_dec_ent name)


(* sort the DecTree in a given list according to a reverse pseudo-lexicographic order among the name of the DecTrees *)
let sort_dectree_list_gen d_l =
	let name_of t =
		match t with
		DecTree_(i, _, _, _, _) ->
			i
	in
	(* same specification as a standard compare function, x>y means x is "bigger" than y (eg: 12_3 < 13_3_5, "nothing" < x for any x) *)
	let rec comp_gen_int_list x y =
		match x with
		| [] -> -1
		| x1::x2 ->
			(match y with
			| [] -> 1
			| y1::y2 ->
				let diff = compare (int_of_string ("0b" ^ x1)) (int_of_string ("0b" ^ y1)) in
				if diff == 0 then
					comp_gen_int_list x2 y2
				else
					diff
			)
	in
	let comp_fun x y =
		comp_gen_int_list (name_of y) (name_of x)
	in
	(*!!DEBUG!!*)
	(*Printf.printf "sort %d nodes\n" (List.length d_l);*)
	(*List.iter print_dec_tree_gen d_l;*)
	flush stdout;
	List.sort comp_fun d_l


let output_all_table_C_decl out =
	let isize = Irg.get_isize ()
	in
	let get_min_max_instr_size_from_instr _ =
		let iter_fun accu inst =
			let size = get_instruction_length inst
			in
			let (min_size, max_size) = accu
			in
			if size < min_size then
				(size, max_size)
			else
				if size > max_size then
					(min_size, size)
				else
					accu
		in
		let init_max = 0 in
		let init_min = 10000 in
		Iter.iter iter_fun (init_min, init_max)
	in
	let get_min_max_instr_size_from_isize _ =
		let min_fun a b_i =
			if b_i < a then
				b_i
			else
				a
		in
		let max_fun a b_i =
			if b_i > a then
				b_i
			else
				a
		in
		(List.fold_left min_fun (List.hd isize) (List.tl isize),
		 List.fold_left max_fun (List.hd isize) (List.tl isize))
	in
	let is_isize _ =
		isize != []
	in
	(* check if all instr have their size in isize *)
	let check_isize _ =
		let iter_fun accu inst =
			let size = get_instruction_length inst
			in
			if List.exists (fun x -> x==size) isize then
				true
			else
				raise CheckIsizeException
		in
		try
			Iter.iter iter_fun false
		with
		CheckIsizeException ->
			false
	in
	(* list of the specialized fixed fetch sizes (for RISC ISA),
	 * they correspond to the size of C's standard integer types (uintN_t)
	 * other or variable sizes imply use of generic fetch and decode *)
	let fetch_sizes = [8; 16; 32; 64]
	in
	let get_fetch_size_from_min_max min_max =
		let (min_size, max_size) = min_max
		in
		if min_size == max_size then
			(* constant size instrs (RISC) *)
			(try
				List.find (fun x -> min_size == x) fetch_sizes
			with
			Not_found ->
				(* constant size but not implemented => generic *)
				fetch_generic)
		else
			(* variable size or diff *)
			fetch_generic
	in
	let choose_fetch_size _ =
		if is_isize () then
			if check_isize () then
				get_fetch_size_from_min_max (get_min_max_instr_size_from_isize ())
			else
				raise (Sys_error "isize definition incorrect, some instructions have a size not contained in isize.")
		else
			get_fetch_size_from_min_max (get_min_max_instr_size_from_instr ())
	in
	let fetch_size = choose_fetch_size ()
	in (* output the declaration of struct Table_Decodage *)
	let output_table_type_decl _ =
		if fetch_size == fetch_generic then
			(* generic, mask is not an uintN_t here *)
			((*output_string out "typedef struct {\n\tuint32_t\t*mask;\n\tint\tbit_length;\n} mask_t;\n\n";*)
			output_string out "typedef struct {\n\tmask_t\t*mask;\n\tDecode_Ent\t*table;\n} Table_Decodage;\n";
			output_string out "\n\n/* and now the tables */\n\n\n")
		else
			(Printf.fprintf out "typedef struct {\n\tuint%d_t\tmask;\n" fetch_size;
			output_string out "\tDecode_Ent\t*table;\n} Table_Decodage;\n";
			output_string out "\n\n/* and now the tables */\n\n\n")
	in
	let fetch_stat = if !output_fetch_stat then open_out ((Irg.get_proc_name ()) ^ "_fetch_tables.stat") else stdout in
	let aux dl dt =
		output_table_C_decl_gen fetch_size out fetch_stat dt dl
	in
	let dl  = sort_dectree_list_gen (build_dec_nodes_gen 0)
	in
		output_table_type_decl ();
		List.iter (aux dl) dl;
		if !output_fetch_stat then
			close_out fetch_stat


(* some testing *)


let test_build_dec_nodes n =
	match n with
	0 ->
		begin
		Printf.printf "\n\ntest build decode nodes\n";
		print_dec_tree_list (build_dec_nodes 0)
		end
	| _ ->
		()

let test_sort _ =
	let name_of t =
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (if (String.length s) == 0 then (string_of_int (Int32.to_int a)) else (s^"_"^(string_of_int (Int32.to_int a))))
		in
		match t with
		DecTree(i, s, m, g, d) ->
			aux i "name:"
	in
	let dl = (*sort_dectree_list ( *)build_dec_nodes 0
	in
	let aux x =
		Printf.printf "%s\n" (name_of x)
	in
	print_string "let's test!\n";
	List.iter aux dl


(* dot related *)


(* print a node's informations to be used in dot format *)
let print_dot_dec_tree tr =
	let name_of t =
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (s^"_"^(string_of_int (Int32.to_int a)))
		in
		match t with
		DecTree(i, s, m, g, d) ->
			aux i ""
	in
	let spec_list_of t =
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (s^"\\l"^(Iter.get_name a))
		in
		match t with
		DecTree(i, s, m, g, d) ->
			aux s ""
	in
	match tr with
	DecTree(int_l, sl, msk, gm, dt_l) ->
		begin
		print_string "\"";
		print_string (name_of tr);
		print_string "\" ";
		print_string "[label=\"{";
		print_string (name_of tr);
		print_string " | ";
		print_string (spec_list_of tr);
		print_string "}\"]"
		end

(* returns a list of all edges that would be present if the given list of DecTree
was to be represented as a tree, used to build the graph in dot format,
the result is a list of sub-lists symbolizing each edge containing 2 elements, the head and the tail of an edge *)
let get_edges_between_dec_tables dl =
	let make_edge src sons_list =
		List.map (fun x -> [src; x]) sons_list
	in
	List.flatten (List.map (fun x -> make_edge x (find_sons_of_node x dl)) dl)


let print_dot_edges edge =
	let name_of t =
		let rec aux l s =
			match l with
			[] ->
				s
			| a::b ->
				aux b (s^"_"^(string_of_int (Int32.to_int a)))
		in
		match t with
		DecTree(i, s, m, g, d) ->
			aux i ""
	in
	match edge with
	[a; b] ->
		begin
		Printf.printf "\"%s\"" (name_of a);
		Printf.printf " -> ";
		Printf.printf "\"%s\"" (name_of b);
		end
	| _ ->
		()

		
let print_dot_dec_tree_list tl =
	begin
	print_string "digraph DEC {\n";
	print_string "node [shape=Mrecord, labeljust=1, fontsize=10];\n";
	List.iter (fun x -> begin print_dot_dec_tree x; print_string "\n" end) tl;
	List.iter (fun x -> begin print_dot_edges x; print_string "\n" end) (get_edges_between_dec_tables tl);
	print_string "}\n"
	end
