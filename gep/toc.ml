(*
 * $Id: toc.ml,v 1.8 2009/02/06 18:23:04 casse Exp $
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
exception PreError of (out_channel -> unit)
exception LocError of string * int * (out_channel -> unit)


(** Execute the function f, capturing PreError exception and
	adding error location to re-raise them as LocError.
	@param file	Source file.
	@param line	Source line.
	@param f	Function execution.
	@param arg	Argument to apply on f*)
let locate_error file line f arg =
	try
		f arg
	with PreError f ->
		raise (LocError (file, line, f))


(** Raise an error with the given message.
	@param msg	Message to display. *)
let error msg =
	raise (PreError (fun out -> output_string out msg))


(** Generate an error exception with the given message
	from the given expression.
	@param msg	Message to display.
	@param expr	Expression causing the error. *)
let error_on_expr msg expr =
	raise (PreError (fun out ->
		output_string out msg;
		output_string out ": ";
		Irg.output_expr out expr))


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
	mutable ipath: string;		(** include path *)
	mutable spath: string;		(** source path *)
	mutable bito: bit_order;	(** define bit order for bit fields *)
	mutable inst: string;		(** current  integrated instruction name *)
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
		inst = "";
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
	@param info	Generation information.
	@param name	Register or memory name. *)
let state_macro info name =
	Printf.sprintf "%s_%s" (String.uppercase info.proc) (String.uppercase name)


(** Get the name of a parameter macro.
	@param info	Generation information.
	@param name	Parameter name. *)
let param_macro info name =
	Printf.sprintf "%s_%s_%s" (String.uppercase info.proc) info.inst name


(** Generate the name of a temporary of index i.
	@param i	Index of the temporary.
	@return		Temporary name. *)
let temp_name i =
	Printf.sprintf "_gtmp%d" i


(** Generate code for reading an unindexed data parameter,
	register or memory.
	@param info		Information about generation.
	@param name		Name of the accessed data. *)
let get_unindexed info name =
	match Irg.get_symbol name with
	  Irg.MEM _
	| Irg.REG _
	| Irg.VAR _ -> output_string info.out (state_macro info name)
	| Irg.PARAM _ -> output_string info.out (param_macro info name)
	| _ -> failwith "get_unindexed"


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


(** Convert unary operator to C operator.
	@param out	Channel to output to.
	@param op	Unary operator to convert. *)
let convert_unop out op =
	match op with
	  Irg.NOT	-> Printf.fprintf out "!"
	| Irg.BIN_NOT	-> Printf.fprintf out "~"
	| Irg.NEG	-> Printf.fprintf out "-"


(** Convert binary operator to C operator.
	@param out	Channel to output to.
	@param op	Binary operator to convert. *)
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


(** Generate the variables required to translate the given expression.
	@param ctx	(top index of generated variables, variables created until now).
	@param expr	Expression to process.
	@return		(top index, created variables) *)
let rec declare_expression expr ctx =
	let get (idx, vars) t = (idx + 1, (idx, t)::vars) in

	match expr with
	  Irg.NONE ->
	  	ctx
	| Irg.COERCE (_, expr) ->
		declare_expression expr ctx
	| Irg.FORMAT (_, args) ->
		declare_list args ctx
	| Irg.CANON_EXPR (_, _, args) ->
		declare_list args ctx
	| Irg.REF _ ->
		ctx
	| Irg.ITEMOF (t, Irg.REF name, index) ->
		declare_expression index ctx
	| Irg.ITEMOF _
	| Irg.FIELDOF _ ->
		failwith "???"
	| Irg.BITFIELD (_, expr, low, up) ->
		declare_list [expr; low; up] ctx
	| Irg.UNOP (_, _, expr) ->
		declare_expression expr ctx
	| Irg.BINOP (_, _, expr1, expr2) ->
		declare_list [expr1; expr2] ctx
	| Irg.IF_EXPR (t, cond, tpart, epart) ->
		declare_list [cond; tpart; epart] (get ctx t)
	| Irg.SWITCH_EXPR (t, cond, cases, def) ->
		declare_list (cond::def::(snd (List.split cases))) (get ctx t)
	| Irg.CONST _ -> ctx
	| Irg.ELINE (_, _, expr) -> declare_expression expr ctx

and declare_list args ctx =
	List.fold_left (fun ctx arg -> declare_expression arg ctx) ctx args
	


(** Generate the declaration of the given list of
	temporary variables.
	@param out	Channel to output to.
	@param vars	List of temporaries to declare. *)
let rec declare_temp out vars =
	List.iter
		(fun (i, t) ->
			Printf.fprintf out "\t%s %s; "
				(type_to_string (convert_type t))
				(temp_name i))
		vars


(** Count the number of used temporary variables
	without generating the codE.
	@param		Expression to compute.
	@param 		Initial index of temporaries.
	@return		Final index of temporaries. *)
let rec count_expression expr idx =
	match expr with
	| Irg.REF _
	| Irg.NONE
	| Irg.FIELDOF _
	| Irg.CONST _ ->
		idx
	| Irg.COERCE (_, e) ->
		count_expression e idx
	| Irg.UNOP (_, _, e) ->
		count_expression e idx
	| Irg.BINOP (t, _, e1, e2) ->
		count_expression_list [e1; e2] idx
	| Irg.FORMAT (_, args) ->
		count_expression_list args idx
	| Irg.CANON_EXPR (_, _, args) ->
		count_expression_list args idx
	| Irg.ITEMOF (_, _, index) ->
		count_expression index idx
	| Irg.BITFIELD (_, e, l, u) ->
		count_expression_list [e; l; u] idx
	| Irg.IF_EXPR (_, c, t, e) ->
		count_expression_list [c; t; e] (idx + 1)
	| Irg.SWITCH_EXPR (_, cond, cases, def) ->
		count_expression_list (cond::def::(snd (List.split cases))) (idx + 1)
	| Irg.ELINE (_, _, expr) ->
		count_expression expr idx

and count_expression_list exprs idx =
	List.fold_left (fun idx expr -> count_expression expr idx) idx exprs


(** Generate the code for the given expression.
	@param info		Generation information.
	@param expr		Expression to generate.
	@param idx		Initial index of temporaries.
	@return			Final index of temporaries. *)
let rec gen_expression (info: info_t) (expr: Irg.expr) idx =

	let get_function op t =
		match op with
		| Irg.LROTATE -> Printf.sprintf "gliss_rotate_left%s" (type_to_mem(convert_type t))
		| Irg.RROTATE -> Printf.sprintf "gliss_rotate_right%s" (type_to_mem(convert_type t))
		| Irg.EXP -> Printf.sprintf "gliss_exp%s" (type_to_mem(convert_type t))
		| Irg.CONCAT -> Printf.sprintf "gliss_concat%s" (type_to_mem(convert_type t))
		| _ -> "" in

	match expr with
	| Irg.NONE ->
		idx
	
	| Irg.REF name ->
		get_unindexed info name; idx

	| Irg.UNOP (_, op, e) -> 
		convert_unop info.out op;
		gen_expression info e idx

	| Irg.BINOP (t, op, e1, e2) ->
		let fname = get_function op t in
		if fname = "" then
			begin
				output_char info.out '(';
				let idx = gen_expression info e1 idx in
				output_char info.out ' ';
				convert_binop info.out op;
				output_char info.out ' ';
				let idx = gen_expression info e2 idx in
				output_char info.out ')';
				idx
			end
		else
			begin
				Printf.fprintf info.out "%s(" fname;
				let idx = gen_expression info e1 idx in
				output_string info.out ", ";
				let idx = gen_expression info e2 idx in
				output_char info.out ')';
				idx
			end
		
	| Irg.IF_EXPR (_, c, t, e) ->
		output_string info.out (temp_name idx);
		count_expression_list [c; t; e] (idx + 1)

	| Irg.SWITCH_EXPR (_, cond, cases, def) ->
		output_string info.out (temp_name idx);
		count_expression_list (cond::def::(snd (List.split cases))) (idx + 1)		

	| _ -> raise (UnsupportedExpression expr) 


(** Generate expression that can not be embedded in
	C expressions.
	@param info		Generation information.
	@param expr		Expression to generation.
	@param idx		Top temporary index.
	@return			Overall temporary index. *)
let rec pregen_expression (info : info_t) expr idx =

	let make_if (cond: Irg.expr) tpart epart =
		let tmp = idx in
		
		(* condition *)
		let cond_idx = pregen_expression info cond (idx + 1) in
		output_string info.out "\tif(";
		ignore (gen_expression info cond (idx + 1));
		
		(* the part *)
		output_string info.out "\t) {\n";
		let tpart_idx = pregen_expression info  tpart cond_idx in
		Printf.fprintf info.out "\t\t%s = " (temp_name tmp);
		ignore(gen_expression info tpart cond_idx);
		
		(* else part *)
		output_string info.out "\t} else {\n";
		let epart_idx = pregen_expression info epart tpart_idx in
		Printf.fprintf info.out "\t\t%s = " (temp_name tmp);
		ignore (gen_expression info tpart tpart_idx);		

		(* end of if *)
		output_string info.out "\t}\n";
		epart_idx in
	
	let make_switch cond cases def =
		let tmp = idx in
		
		(* the condition *)
		let cond_idx = pregen_expression info cond idx in
		output_string info.out "\tswitch(";
		ignore(gen_expression info cond idx);
		output_string info.out ") {\n";
		
		(* the cases *)
		let case_idx = List.fold_left
			(fun idx (case, expr) ->
				output_string info.out "\t\tcase ";
				ignore(gen_expression info case 0);
				output_string info.out ": {\n";
				let new_idx = pregen_expression info expr idx in
				Printf.fprintf info.out "\t\t%s = " (temp_name tmp);
				ignore(gen_expression info expr idx);
				output_string info.out "; }\n\t\tbreak;\n";
				new_idx
			)
			(count_expression def cond_idx) cases in
		
		(* default case *)
		if def <> Irg.NONE then
			begin
				output_string info.out "\tdefault: {\n";
				ignore(pregen_expression info def cond_idx);
				Printf.fprintf info.out "\t\t%s = " (temp_name tmp);
				ignore(gen_expression info def cond_idx);
				output_string info.out ";\n\t}\n\tbreak;\n"
			end;
		
		(* the end *)
		output_string info.out "\t}\n";
		case_idx in
	
	match expr with
	  Irg.NONE
	| Irg.REF _ ->
	  	idx
	| Irg.COERCE (_, expr) ->
		pregen_expression info expr idx
	| Irg.FORMAT (_, args) ->
		pregen_list info args idx
	| Irg.CANON_EXPR (_, _, args) ->
		pregen_list info args idx
	| Irg.ITEMOF (t, Irg.REF name, index) ->
		pregen_expression info index idx
	| Irg.ITEMOF _
	| Irg.FIELDOF _ ->
		failwith "???"
	| Irg.BITFIELD (_, expr, low, up) ->
		pregen_list info [expr; low; up] idx
	| Irg.UNOP (_, _, expr) ->
		pregen_expression info expr idx
	| Irg.BINOP (_, _, expr1, expr2) ->
		pregen_list info [expr1; expr2] idx
	| Irg.IF_EXPR (t, cond, tpart, epart) ->
		make_if cond tpart epart
	| Irg.SWITCH_EXPR (t, cond, cases, def) ->
		make_switch cond cases def 
	| Irg.CONST _ -> idx
	| Irg.ELINE (_, _, expr) -> pregen_expression info expr idx

and pregen_list info args idx =
	List.fold_left
		(fun idx arg -> pregen_expression info arg idx)
		idx args
	

(** Convert an OCAML string to generated C string code.
	Replace '"' and '\', respectively, by '\"' and '\\'.
	@param str	String to transform.
	@return		Transformed string. *)
let cstring str =
	let rec aux str i res =
		if i >= String.length str then res else
		match String.get str i with
		  '\"' -> aux str (i + 1) (res ^ "\\\"")
		| '\\' -> aux str (i + 1) (res ^ "\\\\")
		| c -> aux str (i + 1) (res ^ (String.make 1 c)) in
		
	aux str 0 ""

