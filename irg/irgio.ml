(*
 * GLISS2 -- IRG input/output module
 * Copyright (c) 2015, IRIT - UPS <casse@irit.fr>
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

(** This module provides several functions to perform input/output on IRG data structures.
	- 
*)

open Irg

type xattribute_t = string * string
type xelement_t =
	| XTEXT of string
	| XELEMENT of string * xattribute_t list * xelement_t list

let xattribute id value = (id, value)
let xelement tag attrs content = XELEMENT (tag, attrs, content)

(** Output an XML file.
	@param out	Output stream.
	@param elt	Top-level element. *)
let output_xml out elt =
	
	let escape_char c =
		match c with
		| '"'	-> "&quote;"
		| '\''	-> "&apos;"
		| '<'	-> "&lt;"
		| '>'	-> "&gt;"
		| _ 	-> String.make 1 c in
	
	let rec escape s i r =
		if i >= (String.length s) then r else
		escape s (i + 1) (r ^ (escape_char (String.get s i))) in

	let print_attr (id, value) =
		Printf.fprintf out " %s=\"%s\"" id (escape value 0 "") in
		
	let rec print elt =
		match elt with
		| XTEXT t -> output_string out (escape t 0 "")
		| XELEMENT (tag, attrs, content) ->
			Printf.fprintf out "<%s" tag;
			List.iter print_attr attrs;
			if content = [] then output_string out "/>" else
			begin
				output_string out ">\n";
				List.iter print content;
				Printf.fprintf out "</%s>" tag
			end in
	
	output_string out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	print elt


let output_xml out list =
	let sprintf = Printf.sprintf in
	
	let type_of t =
		match t with
		| NO_TYPE 		-> "no"
		| BOOL			-> "bool"
		| INT n 		-> sprintf "int(%d)" n
		| CARD n		-> sprintf "card(%d)" n
		| FIX (i, f)	-> sprintf "fix(%d, %d)" i f
		| FLOAT (m, e)	-> sprintf "float(%d, %d)" m e
		| RANGE (l, u)	-> sprintf "range(%s, %s)" (Int32.to_string l) (Int32.to_string u)
		| STRING		-> "string'"
		| ENUM _		-> "enum"
		| ANY_TYPE		-> "any" in
	
	let const_of c =
		match c with
	 	| NULL				-> "null"
		| CARD_CONST v 		-> Int32.to_string v 
		| CARD_CONST_64 v	-> Int64.to_string v
		| STRING_CONST s	-> sprintf "'%s'" s
		| FIXED_CONST f		-> sprintf "%f" f 
		| CANON n			-> n in
	
	let rec expr_of e =
		match e with
		| NONE 						-> xelement "none" [] []
		| COERCE (t, e)				-> xelement "coerce" [("type", type_of t)] [expr_of e]
		| FORMAT (f, es)			-> xelement "format" [("f", f)] (List.map expr_of es)
		| CANON_EXPR (t, id, es)	-> xelement "canon" [("id", id); ("type", type_of t)] (List.map expr_of es)
		| REF (t, id)				-> xelement "ref" [("id", id); ("type", type_of t)] []
		| FIELDOF (t, pid, cid)		-> xelement "fieldof" [("pid", pid); ("cid", cid); ("type", type_of t)] []
		| ITEMOF (t, id, idx)		-> xelement "itemof" [("id", id); ("type", type_of t)] [expr_of idx]
		| BITFIELD (t, b, l, u)		-> xelement "bitfield" [("type", type_of t)] [expr_of b; expr_of l; expr_of u]
		| UNOP (t, o, e)			-> xelement "unop" [("op", Irg.string_of_unop o); ("type", type_of t)] [expr_of e]
		| BINOP (t, o, e1, e2)		-> xelement "binop" [("op", Irg.string_of_binop o); ("type", type_of t)] [expr_of e1; expr_of e2]
		| IF_EXPR (tt, c, t, e)		-> xelement "if" [("type", type_of tt)] [expr_of c; expr_of t; expr_of e]
		| SWITCH_EXPR (t, v, cs, d)	-> xelement "switch" [("type", type_of t)] ([expr_of v] @ (List.map case_of cs) @ [expr_of d])
		| CONST (t, k)				-> xelement "const" [("type", type_of t); ("value", const_of k)] []
		| ELINE (_, _, e)			-> expr_of e
		| CAST (t, e) 				-> xelement "casr" [("type", type_of t)] [expr_of e]
	and case_of (c, e) =
		xelement "case" [] [expr_of c; expr_of e] in
	
	let rec loc_of loc =
		match loc with
		| LOC_NONE						-> xelement "none" [] []
		| LOC_REF (t, id, idx, l, u)	-> xelement "ref" [("id", id); ("type", type_of t)] [expr_of idx; expr_of l; expr_of u]
		| LOC_CONCAT (t, l1, l2)		-> xelement "concant" [("type", type_of t)] [loc_of l1; loc_of l2] in

	let rec stat_of stat =
		match stat with
		| NOP						-> xelement "nop" [] []
		| SEQ (s1, s2)				-> xelement "seq" [] [stat_of s1; stat_of s2]
		| EVAL (pid, cid)			-> xelement "eval" [("pid", pid); ("cid", cid)] []
		| SET (l, e)				-> xelement "set" [] [loc_of l; expr_of e]
		| CANON_STAT (id, es)		-> xelement "canon" [("id", id)] (List.map expr_of es)
		| ERROR id					-> xelement "error" [("id", id)] []
		| IF_STAT (c, s1, s2)		-> xelement "if" [] [expr_of c; stat_of s1; stat_of s2]
		| SWITCH_STAT (v, cs, s)	-> xelement "switch" [] ([expr_of v] @ (List.map scase_of cs) @ [stat_of s])
		| LINE (_, _, s)			-> stat_of s
		| LOCAL (v, o, t)			-> xelement "let" [("id", v); ("oid", o); ("type", type_of t)] []
		| FOR(v, uv, t, l, u, b)	-> xelement "for" [("id", v); ("uid", uv); ("type", type_of t); ("lo", const_of l); ("hi", const_of u)] [stat_of b]
	and scase_of (c, s) =
		xelement "case" [] [expr_of c; stat_of s] in
			
	let print_item item =
		match item with
		| PTEXT t	-> XTEXT t
		| PSTAT s 	-> stat_of s
		| PEXPR e 	-> expr_of e
		| PLOC l 	-> loc_of l
		| PTYPE t 	-> XTEXT (type_of t)
		| PFUN f 	-> XTEXT ""
		| PINT i 	-> XTEXT (string_of_int i)
		| PINT32 i	-> XTEXT (Int32.to_string i)
		| PINT64 i 	-> XTEXT (Int64.to_string i)
		| PLN		-> XTEXT "\n"
		| PSPEC	s	-> XTEXT ""
		| PCST c	-> XTEXT (const_of c) in
	
	output_xml out (xelement "irg" [] (List.map print_item list))


(** IRG output to an XML file.
	@param name		File name.
	@param list		List of item to display. *)
let output_xml_file name list =
	let file = open_out name in
	output_xml file list;
	close_out file


(** Dump given list of IRG item in parenthesized way.
	@param out	Out stream.
	@param list	List of items. *) 
let dump_stream out list =
	let write t = output_string out t in
	let print = Printf.fprintf in
	let sprintf = Printf.sprintf in
	let op t = print out "%s(" t in
	let com _ = write ", " in
	let close _ = write ")" in
	let str t = print out "\"%s\"" t in

	let type_of t =
		write (match t with
				| NO_TYPE 		-> "no"
				| BOOL			-> "bool"
				| INT n 		-> sprintf "int(%d)" n
				| CARD n		-> sprintf "card(%d)" n
				| FIX (i, f)	-> sprintf "fix(%d, %d)" i f
				| FLOAT (m, e)	-> sprintf "float(%d, %d)" m e
				| RANGE (l, u)	-> sprintf "range(%s, %s)" (Int32.to_string l) (Int32.to_string u)
				| STRING		-> "string'"
				| ENUM _		-> "enum"
				| ANY_TYPE		-> "any") in
	
	let const_of c =
		write (match c with
			 	| NULL				-> "null"
				| CARD_CONST v 		-> Int32.to_string v 
				| CARD_CONST_64 v	-> Int64.to_string v
				| STRING_CONST s	-> sprintf "'%s'" s
				| FIXED_CONST f		-> sprintf "%f" f 
				| CANON n			-> n) in
	
	let rec expr_of e =
		match e with
		| NONE 						-> write "none"
		| COERCE (t, e)				-> op "coerce"; type_of t; com (); expr_of e; close ()
		| FORMAT (f, es)			-> op "format"; str f; List.iter (fun e -> com (); expr_of e) es; close ()
		| CANON_EXPR (t, id, es)	-> op "canon"; type_of t; str id; List.iter (fun e -> com (); expr_of e) es; close () 
		| REF (t, id)				-> op "ref"; type_of t; com (); str id; close ()
		| FIELDOF (t, pid, cid)		-> op "fieldof"; str pid; com (); str cid; close ()
		| ITEMOF (t, id, idx)		-> op "itemof"; type_of t; com (); str id; com (); expr_of idx; close ()
		| BITFIELD (t, b, l, u)		-> op "bitfield"; type_of t; com (); expr_of b; com (); expr_of l; com (); expr_of u; close ()
		| UNOP (t, o, e)			-> op "unop"; type_of t; com (); write (Irg.string_of_unop o); com (); expr_of e; close ()
		| BINOP (t, o, e1, e2)		-> op "binop"; type_of t; com (); write (Irg.string_of_binop o); com (); expr_of e1; com (); expr_of e2; close ()
		| IF_EXPR (tt, c, t, e)		-> op "if"; type_of tt; com (); expr_of c; com (); expr_of t; com (); expr_of e; close ()
		| SWITCH_EXPR (t, v, cs, d)	-> op "switch"; type_of t; com(); expr_of v; com (); List.iter case_of cs; expr_of d; close ()
		| CONST (t, k)				-> op "const"; type_of t; com(); const_of k; close ()
		| ELINE (_, _, e)			-> expr_of e
		| CAST (t, e) 				-> op "cast"; type_of t; com(); expr_of e; close ()
	and case_of (c, e) =
		op "case"; expr_of c; com(); expr_of e; close(); com() in
	
	let rec loc_of loc =
		match loc with
		| LOC_NONE						-> write "none"
		| LOC_REF (t, id, idx, l, u)	-> op "ref"; type_of t; com (); str id; com (); expr_of idx; com (); expr_of l; com (); expr_of u; close ()
		| LOC_CONCAT (t, l1, l2)		-> op "concat"; type_of t; com(); loc_of l1; com (); loc_of l2; close () in

	let rec stat_of stat =
		match stat with
		| NOP						-> write "nop"
		| SEQ (s1, s2)				-> op "seq"; stat_of s1; com (); stat_of s2; close ()
		| EVAL (pid, cid)			-> print out "eval(\"%s\", \"%s\")" pid cid;
		| SET (l, e)				-> op "set"; loc_of l; com (); expr_of e; close ()
		| CANON_STAT (id, es)		-> print out "canon(\"%s\", " id;  List.iter (fun e -> expr_of e; com ()) es; close ()
		| ERROR id					-> op "error"; str id; close ()
		| IF_STAT (c, s1, s2)		-> op "if"; expr_of c; com (); stat_of s1; com (); stat_of s2; close ()
		| SWITCH_STAT (v, cs, s)	-> op "switch"; expr_of v; com (); List.iter scase_of cs; stat_of s; close ()
		| LINE (_, _, s)			-> stat_of s
		| LOCAL (v, o, t)			-> op "let"; str o; com(); type_of t; close()
		| FOR (v, _, t, l, u, b)	-> op "for"; str v; str ": "; type_of t; str " in "; const_of l; str ".."; const_of u; str "do"; com(); stat_of b; com(); close ()
	and scase_of (c, s) =
		op "case"; expr_of c; com (); stat_of s; close(); com () in
			
	let item_of item =
		match item with
		| PTEXT t	-> write t
		| PSTAT s 	-> stat_of s
		| PEXPR e 	-> expr_of e
		| PLOC l 	-> loc_of l
		| PTYPE t 	-> type_of t
		| PFUN f 	-> f out
		| PINT i 	-> write (string_of_int i)
		| PINT32 i	-> write (Int32.to_string i)
		| PINT64 i 	-> write (Int64.to_string i)
		| PLN		-> write "\n"
		| PSPEC s	-> ()
		| PCST c	-> const_of c in

	List.iter item_of list


(** Dump to standard output the given list of IRG items.
	@param list	List to output. *)
let dump list =
	dump_stream stdout list
