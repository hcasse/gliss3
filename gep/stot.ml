(*
 * GLISS V2 -- switch to tab transformation
 * Copyright (c) 2008-10, IRIT - UPS <casse@irit.fr>
 *
 * GLISS V2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GLISS V2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GLISS V2; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

open Irg

type tab_t = Irg.type_expr * Irg.const array
let num = ref 0
let max = ref 256
let enabled = ref false
let opts = [
		("-switch", Arg.Set enabled, "activate the constant switch optimization using tables");
		("-switch-max", Arg.Set_int max, "maximum size of constant switch tables")
	]

(* switch hashtable *)
module ExprHash = struct
	type t = Irg.expr
	let equal v1 v2 = v1 == v2
	let hash v = Hashtbl.hash v
end
module ExprHashtbl = Hashtbl.Make(ExprHash)
let map : int ExprHashtbl.t = ExprHashtbl.create 211

(* table hashtable *)
module TabHash = struct
	type t = tab_t
	let equal v1 v2 = v1 = v2
	let hash v = Hashtbl.hash v
end
module TabHashtbl = Hashtbl.Make(TabHash)
let tab : int TabHashtbl.t = TabHashtbl.create 211


(** Get the switch getter name.
	@param n	Name of the table.
	@return		Switch getter name. *)
let gen_get n = Printf.sprintf "__gswitch_get%d" n


(** Called to initialize the switch-to-table pass.
	*)
let init _ =
	ExprHashtbl.clear map;
	TabHashtbl.clear tab


(** Check a switch.
	@param sw			Added switch.
	@raise Not_found	If the expression is not a switch. *)
let check sw =

	(* already processed ? *)
	try
		ExprHashtbl.find map sw
	with Not_found ->

	(* else explore it *)
	match sw with
	| Irg.SWITCH_EXPR (t, c, cs, def) ->
		(try
			(*print [PTEXT "DEBUG: SWITCH\n"];*)

			(* try to get a constant table *)
			let b = Sem.get_type_length (Sem.get_type_expr c) in
			if b >= (Sys.word_size - 1) then -1 else
			let s = 1 lsl b in
			if s > !max then -1 else
			let items = List.map (fun (c, v) ->
				 (Sem.eval_const c, Sem.eval_const v)) cs in
			let def =
				if def = NONE then NULL else  Sem.eval_const def in
			(*List.iter (fun (c, v) -> print [PTEXT "DEBUG: "; PCST c; PTEXT " -> "; PCST v; PTEXT "\n"]) items;*)

			(* build the table *)
			let n = !num in
			let a = Array.make s def in
			List.iter (fun (i, v) -> Array.set a (Sem.to_int i) v) items;
			let k = (t, a) in

			(* declare it *)
			(try TabHashtbl.find tab k
			with Not_found ->
				ExprHashtbl.add map sw n;
				(if not (TabHashtbl.mem tab k) then TabHashtbl.add tab k n);
				incr num;
				n)

		with Irg.Error _ | Irg.PreError _ -> -1)
	| _ -> raise Not_found


(** Declare the array with the given ToC information.
	@param info		ToC information. *)
let declare info =
	TabHashtbl.iter
		(fun (t, a) n ->
			let tn = Toc.type_to_string (Toc.convert_type t) in
			Printf.fprintf info.Toc.out "static %s __gswitch_t%d[%d] = {\n\t" tn n (Array.length a);
			let _ = Array.fold_left (fun f i ->
						if not f then output_string info.Toc.out ",\n\t";
						Toc.gen_const info t i; false) true a in
			Printf.fprintf info.Toc.out "\n};\n\n#define %s(i) __gswitch_t%d[i]\n" (gen_get n) n
		)
		tab


(** Transform the current instruction set to optimize switches
	that may be translated as tables.
	@param attr		Name of the attribute to transform. *)
let transform_aux attr =

	let rec set_attr al n a =
		match al with
		| [] -> failwith "stot: lost attribute?"
		| Irg.ATTR_STAT (m, _)::t when n = m -> a::t
		| Irg.ATTR_EXPR (m, _)::t when n = m -> a::t
		| h::t -> h::(set_attr t n a) in

	let rec process_attrs name (pl, al, st) =
		if List.mem name st then (pl, al, st) else
		process_attr name al (pl, al, st)		

	and process_attr name l info =
		match l with
		| [] ->
			info
		| Irg.ATTR_STAT (n, s)::_ when n = name ->
			let (pl, al, st) = info in
			let sp, (pl, al, _) = process_stat s (pl, al, name::st) in
			(pl, set_attr al name (Irg.ATTR_STAT(name, sp)), st)
		| Irg.ATTR_EXPR (n, e)::_ when n = name ->
			let (pl, al, st) = info in
			(pl, set_attr al name (Irg.ATTR_EXPR(name, process_expr e)), st)
		| _::l ->
			process_attr name l info

	and process_stat s info =
		match s with
		| SEQ (s1, s2) ->
			let (s1, info) = process_stat s1 info in
			let (s2, info) = process_stat s2 info in
			(SEQ (s1, s2), info)
		| EVAL ("", name) ->
			(s, process_attrs name info)
		| SET (l, e) ->
			(SET (process_loc l, process_expr e), info)
		| CANON_STAT (n, args) ->
			(CANON_STAT(n, List.map process_expr args), info)
		| IF_STAT (c, t, e) ->
			let t, info = process_stat t info in
			let e, info = process_stat e info in
			(IF_STAT(process_expr c, t, e), info)
		| SWITCH_STAT (c, cs, d) ->
			let d, info = process_stat d info in
			let (cs, info) = List.fold_left
				(fun (l, info) (c, a) -> let a, info = process_stat a info in ((c, a)::l, info))
				([], info) cs in
			(SWITCH_STAT (process_expr c, cs, d), info)
		| LINE (f, l, s) ->
			let s, info = process_stat s info in
			(LINE (f, l, s), info)
		| _ -> (s, info)

	and process_loc l =
		match l with
		| LOC_NONE -> l
		| LOC_REF (t, n, i, u, l) ->
			LOC_REF (t, n, process_expr i, process_expr u, process_expr l)
		| LOC_CONCAT (t, l1, l2) ->
			LOC_CONCAT (t, process_loc l1, process_loc l2)

	and process_expr e =
		match e with
		| COERCE (t, e) -> COERCE(t, process_expr e)
		| FORMAT (s, args) -> FORMAT (s, List.map process_expr args)
		| CANON_EXPR (t, n, args) -> CANON_EXPR (t, n, List.map process_expr args)
		| ITEMOF (t, n, i) -> ITEMOF (t, n, process_expr i)
		| BITFIELD (t, e, u, l) -> BITFIELD (t, process_expr e, process_expr u, process_expr l)
		| UNOP (t, o, e) -> UNOP (t, o, process_expr e)
		| BINOP (t, o, e1, e2) -> BINOP (t, o, process_expr e1, process_expr e2)
		| IF_EXPR (tp, c, t, e) -> IF_EXPR (tp, process_expr c, process_expr t, process_expr e)
		| ELINE (f, l, e) -> ELINE (f, l, process_expr e)
		| CAST (t, e) -> CAST(t, process_expr e)
		| SWITCH_EXPR (t, c, cs, d) ->
			let n = check e in
			if n >= 0 then CANON_EXPR(t, gen_get n, [process_expr c]) else
			SWITCH_EXPR (t, process_expr c, List.map (fun (c, e) -> (c, process_expr e)) cs, process_expr d)
		| _ -> e in

	if !enabled then begin
		init ();
		Iter.transform (fun _ pl al ->
			param_stack pl;
			let pl, al, _ = process_attrs attr (pl, al, []) in
			param_unstack pl;
			((), pl, al)) ()
	end


(** Transform the current instruction set to optimize switches
	that may be translated as tables. *)
let transform _ =
	transform_aux "action"
