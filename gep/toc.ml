(*
 * $Id: toc.ml,v 1.5 2009/01/27 21:12:53 casse Exp $
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

exception UnsupportedType of Irg.type_expr
exception UnsupportedExpression of Irg.expr
exception Error of string

(** C type in the generated code. *)
type c_type =
	  INT8
	| UINT8
	| INT16
	| UINT16
	| INT32
	| UINT32
	| INT64
	| UINT64
	| FLOAT
	| DOUBLE
	| LONG_DOUBLE
	| CHAR_PTR

type bit_order = UPPERMOST | LOWERMOST


(** Gather information useful for the generation. *)
type info_t = {
	mutable out: out_channel;	(** Out channel *)
	mutable proc: string;		(** Processor name *)
	mutable state: string;		(** state variable name *)
	mutable inst: string;		(** inst variable name *)
	mutable ipath: string;		(** include path *)
	mutable spath: string;		(** source path *)
	mutable bito: bit_order;	(** define bit order for bit fields *)
}



(** Empty information record. *)
let info _ =
	let p = 
		match Irg.get_symbol "proc" with
		  Irg.LET(_, Irg.STRING_CONST name) -> name
		| _ -> raise (Error "'proc' must be defined as a string let") in
	let b =
		match Irg.get_symbol "bit_order" with
		  Irg.UNDEF -> UPPERMOST
		| Irg.LET(_, Irg.STRING_CONST id) ->
			if (String.uppercase id) = "UPPERMOST" then UPPERMOST
			else if (String.uppercase id) = "LOWERMOST" then LOWERMOST
			else raise (Error "'bit_order' must contain either 'uppermost' or 'lowermost'")
		| _ -> raise (Error "'bit_order' must be defined as a string let") in 
	{
		out = stdout;
		proc = p;
		state = "state";
		inst = "inst";
		ipath = "include/" ^ p;
		spath = "src";
		bito = b;
	}


(** Convert an NML type to C type.
	@param t	Type to convert.
	@return		Matching C type.
	@raise UnsupportedType	If the type is not supported. *)
let rec convert_type t =
	match t with
	  Irg.NO_TYPE -> assert false
	| Irg.BOOL -> UINT8
	| Irg.INT n when n <= 8 -> INT8
	| Irg.INT n when n <= 16 -> INT16
	| Irg.INT n when n <= 32 -> INT32
	| Irg.INT n when n <= 64 -> INT64
	| Irg.CARD n when n <= 8 -> UINT8
	| Irg.CARD n when n <= 16 -> UINT16
	| Irg.CARD n when n <= 32 -> UINT32
	| Irg.CARD n when n <= 64 -> UINT64
	| Irg.FLOAT (23, 9) -> FLOAT
	| Irg.FLOAT (52, 12) -> DOUBLE
	| Irg.FLOAT (64, 16) -> LONG_DOUBLE
	| Irg.STRING -> CHAR_PTR
	| Irg.ENUM _ -> UINT32
	| Irg.RANGE (_, m) ->
		convert_type (Irg.INT (int_of_float (ceil ((log (Int32.to_float m)) /. (log 2.)))))
	| _ -> raise (UnsupportedType t)


(** Convert a C type to a string.
	@param t	C type to convert.
	@return		Matching string. *)
let rec type_to_string t =
	match t with
	  INT8 -> "int8_t"
	| UINT8 -> "uint8_t"
	| INT16 -> "int16_t"
	| UINT16 -> "uint16_t"
	| INT32 -> "int32_t"
	| UINT32 -> "uint32_t"
	| INT64 -> "int64_t"
	| UINT64 -> "uint64_t"
	| FLOAT -> "float"
	| DOUBLE -> "double"
	| LONG_DOUBLE -> "long double"
	| CHAR_PTR -> "char *"


(** Convert a C type to a parameter name.
	@param t	C type to convert.
	@return		Matching parameter name. *)
let rec type_to_field t =
	match t with
	  INT8 -> "int8"
	| UINT8 -> "uint8"
	| INT16 -> "int16"
	| UINT16 -> "uint16"
	| INT32 -> "int32"
	| UINT32 -> "uint32"
	| INT64 -> "int64"
	| UINT64 -> "uint64"
	| FLOAT -> "_float"
	| DOUBLE -> "_double"
	| LONG_DOUBLE -> "_long_double"
	| CHAR_PTR -> "string"


(** Convert a C type to a memory access name.
	@param t	C type to convert.
	@return		Matching memory access name. *)
let rec type_to_mem t =
	match t with
	  INT8 -> "8"
	| UINT8 -> "8"
	| INT16 -> "16"
	| UINT16 -> "16"
	| INT32 -> "32"
	| UINT32 -> "32"
	| INT64 -> "64"
	| UINT64 -> "64"
	| FLOAT -> "f"
	| DOUBLE -> "d"
	| LONG_DOUBLE -> "ld"
	| CHAR_PTR -> assert false

(** Convert an NML type to his size.
	@param t	Type to convert.
	@return		Matching size.
	@raise UnsupportedType	If the type is not supported. *)
let rec type_to_int t =
	match t with
	  Irg.NO_TYPE -> assert false
	| Irg.BOOL -> 8
	| Irg.INT n -> n
	| Irg.CARD n -> n
	| _ -> raise (UnsupportedType t)

(** Get the name of a state macro.
	@param proc	Processor name.
	@param name	Register or memory name. *)
let state_macro info name =
	Printf.sprintf "%s_%s" (String.uppercase info.proc) (String.uppercase name)


(** Get the name of a parameter macro.
	@param proc	Processor name (in uppercase).
	@param i	Current instruction.
	@param name	Parameter name. *)
let param_macro info i name =
	Printf.sprintf "%s_%s_%s" (String.uppercase info.proc) (Iter.get_name i) name


(** Generate code for reading an unindexed data parameter,
	register or memory.
	@param info		Information about generation.
	@param i		Current instruction.
	@param name		Name of the accessed data. *)
let get_unindexed info i name =
	if List.mem_assoc name (Iter.get_params i) then
		Printf.fprintf info.out "%s(%s)" (param_macro info i name) info.inst
	else
		Printf.fprintf info.out "%s(%s)" (state_macro info name) info.state


(** Generate code for writing an unindexed register or variable.
	@param info		Information about generation.
	@param i		Current instruction.
	@param name		Name of the accessed data.
	@param expr		Function to generate the assigned value. *)
let set_unindexed info i name value =
	Printf.fprintf info.out "%s(%s) = " (state_macro info name) info.state;
	value info i;
	Printf.fprintf info.out ";"


(** Generate start of code for accessing an indexed register or memory.
	@param info		Generation information
	@param i		Current instruction.
	@param name		Name of the data.
	@param index	Function to generate the index value. *)
let get_indexed info i name index =
	match Irg.get_symbol name with
	
	  Irg.REG _ | Irg.VAR _ ->
		Printf.fprintf info.out "%s(%s)[" (state_macro info name) info.state;
		index info i;
		Printf.fprintf info.out "]"
		
	| Irg.MEM (_, _, t, _) ->
		Printf.fprintf info.out "gliss_mem_read%s(%s, " (type_to_mem (convert_type t)) (state_macro info name);
		index info i;
		Printf.fprintf info.out ")"

	| _ -> assert false


(** Generate start of code for setting an indexed register or memory.
	@param info		Generation information
	@param i		Current instruction.
	@param name		Name of the data.
	@param index	Function to generate the index value.
	@param expr		Function to generate the set value. *)
let get_indexed info i name index expr =
	match Irg.get_symbol name with
	
	  Irg.REG _ | Irg.VAR _ ->
		Printf.fprintf info.out "%s(%s)[" (state_macro info name) info.state;
		index info i;
		Printf.fprintf info.out "] = ";
		expr info i;
		Printf.fprintf info.out ";"
		
	| Irg.MEM (_, _, t, _) ->
		Printf.fprintf info.out "gliss_mem_write%s(%s, " (type_to_mem (convert_type t)) (state_macro info name);
		index info i;
		Printf.fprintf info.out ", ";
		expr info i;
		Printf.fprintf info.out ")";

	| _ -> assert false
(** Convert an expression to C string.
	@param expr	Expression to convert.
	@param out 	Output Channel.
	@return		C type as string.
	@raise UnsupportedExpression	If the expression is not supported. *)

let convert_unop out op =
	match op with
	  Irg.NOT	-> Printf.fprintf out "!"
	| Irg.BIN_NOT	-> Printf.fprintf out "~"
	| Irg.NEG	-> Printf.fprintf out "-"

let convert_binop out op =
	match op with
	  Irg.ADD	-> Printf.fprintf out "+"
	| Irg.SUB	-> Printf.fprintf out "-"
	| Irg.MUL	-> Printf.fprintf out "*"
	| Irg.DIV	-> Printf.fprintf out "/"
	| Irg.MOD	-> Printf.fprintf out "%%"
	| Irg.LSHIFT	-> Printf.fprintf out "<<"
	| Irg.RSHIFT	-> Printf.fprintf out ">>"
	| Irg.LT	-> Printf.fprintf out "<"
	| Irg.GT	-> Printf.fprintf out ">"
	| Irg.LE	-> Printf.fprintf out "<="
	| Irg.GE	-> Printf.fprintf out ">="
	| Irg.EQ	-> Printf.fprintf out "=="
	| Irg.NE	-> Printf.fprintf out "!="
	| Irg.AND	-> Printf.fprintf out "&&"
	| Irg.OR	-> Printf.fprintf out "||"
	| Irg.BIN_AND	-> Printf.fprintf out "&"
	| Irg.BIN_OR	-> Printf.fprintf out "|"
	| Irg.BIN_XOR	-> Printf.fprintf out "^"
	| Irg.CONCAT	-> Printf.fprintf out ""
	| Irg.RROTATE	-> Printf.fprintf out ""
	| Irg.LROTATE	-> Printf.fprintf out ""
	| Irg.EXP	-> Printf.fprintf out ""


let rec convert_expression out expr =
	match expr with
	| Irg.NONE ->
		Printf.fprintf out ""
	
	| Irg.UNOP (_, op, e) -> 
		convert_unop out op;
		convert_expression out e

	| Irg.BINOP (t, Irg.LROTATE, e1, e2) ->
		Printf.fprintf out "gliss_rotate_left%s (" (type_to_mem(convert_type t));
		convert_expression out e1;
		Printf.fprintf out ",";
		convert_expression out e2;
		Printf.fprintf out ",";
		Printf.fprintf out "%d" (type_to_int t);
		Printf.fprintf out ")"
		

	| Irg.BINOP (t, Irg.RROTATE, e1, e2) ->
		Printf.fprintf out "gliss_rotate_right%s (" (type_to_mem(convert_type t));
		convert_expression out e1;
		Printf.fprintf out ",";
		convert_expression out e2;
		Printf.fprintf out ",";
		Printf.fprintf out "%d" (type_to_int t);
		Printf.fprintf out ")"

	| Irg.BINOP (t, Irg.EXP, e1, e2) ->
		Printf.fprintf out "gliss_exp%s (" (type_to_mem(convert_type t));
		convert_expression out e1;
		Printf.fprintf out ", ";
		convert_expression out e2;
		Printf.fprintf out ")"

	(*| Irg.BINOP (t, Irg.CONCAT, e1, e2) ->
		Printf.fprintf out "gliss_concat%s (" (type_to_mem(convert_type t));
		convert_expression out e1;
		Printf.fprintf out ", ";
		convert_expression out e2;
		Printf.fprintf out ",";*)

	| Irg.BINOP (_, op, e1, e2)-> 
		Printf.fprintf out "(";
		convert_expression out e1;
		Printf.fprintf out ")";
		convert_binop out op;
		Printf.fprintf out "(";
		convert_expression out e2;
		Printf.fprintf out ")"
 	
	| Irg.IF_EXPR (_, c, t, e) ->
		Printf.fprintf out "if (";
		convert_expression out c;
		Printf.fprintf out ") ";
		convert_expression out t;
		Printf.fprintf out " else ";
		convert_expression out e

	(*| Irg.SWITCH_EXPR (_,c, cases, def) ->*)
		

	| _ -> raise (UnsupportedExpression expr) 
