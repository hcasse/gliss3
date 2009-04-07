(*
 * $Id: toc.ml,v 1.27 2009/04/07 16:34:07 barre Exp $
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

let trace id = () (*Printf.printf "TRACE: %s\n" id; flush stdout*)	(* !!DEBUG!! *)


(* KNOWN BUGS
	reg r[1, t] alias rp[i]	is not rightly generated
 *)

(* StringHash module *)
module HashedString = struct
	type t = string
	let equal s1 s2 = s1 = s2
	let hash s = Hashtbl.hash s
end
module StringHashtbl = Hashtbl.Make(HashedString)


(** Get a statement attribute.
	@param name		Name of the attribute. *)
let get_stat_attr name =
	match Irg.get_symbol name with
	| Irg.ATTR (Irg.ATTR_STAT (_, stat)) -> stat
	| _ -> failwith "Not a statement attribute !"


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


(** Raise an unsupported type error.
	@param t	Unsupported type. *)
let unsupported_type t =
	raise (PreError (fun out ->
		output_string out "unsupported type: ";
		Irg.output_type_expr out t))


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
	mutable out: out_channel;		(** out channel *)
	mutable proc: string;			(** processor name *)
	mutable state: string;			(** state variable name *)
	mutable ipath: string;			(** include path *)
	mutable spath: string;			(** source path *)
	mutable bito: bit_order;		(** define bit order for bit fields *)
	mutable iname: string;			(** current  integrated instruction name *)
	mutable inst: Iter.inst;		(** current instruction *)
	mutable temp: int;				(** index for a new temporary *)
	mutable temps: (string * Irg.type_expr) list;		(** list of temporaries *)
	mutable vars: (string * (int * Irg.type_expr)) list;(** list of used variables *)
	mutable calls: (string * string) list;				(** list of performed attributes call *)
	mutable recs: string list;		(** list of recursive actions *)
	mutable lab: int;				(** index of a new label *)
	mutable attrs: Irg.stat StringHashtbl.t;			(** list of prepared attributes *)
	mutable pc_name: string;			(** name of the register used as PC (marked as __attr(pc)) *)
	mutable ppc_name: string;		(** name of the register used as previous instruction PC (marked as __attr(ppc)) *)
	mutable npc_name: string;		(** name of the register used as next instruction PC (marked as __attr(npc)) *)
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
	let get_attr_regs name_attr =
		let rec search_in_mem_attr_list ma_l =
			match ma_l with
			[] -> false
			| a::b ->
				(match a with
				Irg.NMP_ATTR(n_a, args) ->
					if n_a = name_attr && args = [] then
					(* we suppose this function will be called only to retrieve pc like attrs, so no args needed *)
						true
					else
						search_in_mem_attr_list b
				| _ ->
					search_in_mem_attr_list b
				)
		in
		let aux key sp accu =
			if accu = "" then
				(match sp with
				Irg.REG(name, _, _, m_a_l) ->
					if search_in_mem_attr_list m_a_l then
						name
					else
						accu
				| _ ->
					accu)
			else
				accu
		in
		Irg.StringHashtbl.fold aux Irg.syms ""
	in
	{
		out = stdout;
		proc = p;
		state = "state";
		iname = "";
		inst = Iter.null;
		ipath = "include/" ^ p;
		spath = "src";
		bito = b;
		temp = 0;
		temps = [];
		vars = [];
		calls = [];
		recs = [];
		lab = 0;
		attrs = StringHashtbl.create 211;
		pc_name = get_attr_regs "pc";
		npc_name = get_attr_regs "npc";
		ppc_name = get_attr_regs "ppc"
	}


(** Set the current instruction.
	@param info		Current generation information.
	@param inst		Current instruction. *)
let set_inst info inst =
	info.inst <- inst;
	info.iname <- Iter.get_name inst


(** Generate a new label name.
	@param info	Current generation information.
	@return		New label name. *)
let new_label info =
	let res = Printf.sprintf "__gliss_lab_%d" info.lab in
	info.lab <- info.lab + 1;
	res


(** Compute the power of two whose exponent is the lowest
	value greater or equal to n.
	@param n	N to compute with.
	@return		Lowest power of 2. *)
let ceil_log2 n =
	int_of_float (ceil ((log (Int32.to_float n)) /. (log 2.)))


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
	| Irg.UNKNOW_TYPE ->
		(* we have some of this type in bitfield expr, we can't determine the real type and mem size
		so let's patch it up for the moment, uint32 should be the least worst choice *)
		UINT32
	| _ -> unsupported_type t


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
	| _ -> unsupported_type t



(** Get the name of a state macro.
	@param info	Generation information.
	@param name	Register or memory name. *)
let state_macro info name =
	Printf.sprintf "%s_%s" (String.uppercase info.proc) (String.uppercase name)


(** Get the name of a parameter macro.
	@param info	Generation information.
	@param name	Parameter name. *)
let param_macro info name =
	Printf.sprintf "%s_%s_%s" (String.uppercase info.proc) info.iname name


(** Generate the name of a temporary of index i.
	@param i	Index of the temporary.
	@return		Temporary name. *)
let temp_name i =
	Printf.sprintf "_gtmp%d" i


(** Test if the given statement is a nop.
	@param stat		Statement to test.
	@return			True if it is a nop, false else. *)
let rec is_nop stat =
	match stat with
	| Irg.NOP -> true
	| Irg.LINE (_, _, s) -> is_nop s
	| _ -> false

(** Build a new temporary.
	@param info	Current generation information.
	@param t	Type of the temporary.
	@return		Temporary name. *)
let new_temp info typ =
	let var = Printf.sprintf "__gtmp_%d" info.temp in
	info.temp <- info.temp + 1;
	info.temps <- (var, typ)::info.temps;
	var


(** Add a used variable (and only if it has not been declared).
	@param info	Generation information.
	@param name	Name of the variable.
	@param cnt	Count of array.
	@param typ	Type of the variable. *)
let add_var info name cnt typ =
	if not (List.mem_assoc name info.vars)
	then info.vars <- (name, (cnt, typ)) :: info.vars


(** Declare temporaries variables.
	@param	Generation information. *)
let declare_temps info =
	List.iter
		(fun (name, typ) ->
			Irg.add_symbol name (Irg.VAR (name, 1, typ));
			Printf.fprintf info.out "\t%s %s;\n"
				(type_to_string (convert_type typ))
				name
		)
		info.temps;
	List.iter
		(fun (name, (cnt, typ)) ->
			Printf.fprintf info.out "\t%s %s%s;\n"
				(type_to_string (convert_type typ))
				name
				(if cnt = 1 then "" else Printf.sprintf "[%d]" cnt)
		)
		info.vars


(** cleanup temporaries.
	@param info	Generation information. *)
let cleanup_temps info =
	List.iter
		(fun (name, _) -> Irg.rm_symbol name)
		info.temps;
	info.temp <- 0;
	info.temps <- [];
	info.vars <- []


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


(** Get the alias from the list of attributes.
	@param attrs	Attributes to get alias from.
	@return			Alias relocation expression found
					(LOC_NONE if there is no alias) *)
let rec get_alias attrs =
	match attrs with
	| [] -> Irg.LOC_NONE
	| (Irg.ALIAS loc)::_ -> loc
	| _::tl -> get_alias tl



(** Perform alias resolution, that is, translate a state read/write into
	a tuple of unaliased states.
	@param name		Name of the accessed state resource.
	@param idx		Index of the accessed state resource.
	@param ub		Upper bit.
	@param lb		Lower bit.
	@return			(unaliased state name, first state index, 
					state resource count, upper bit, lower bit,
					resource type) *)
let resolve_alias name idx ub lb =
(*print_string ("resolve_alias name=" ^ name);	(* !!DEBUG!! *)
print_string ", idx="; Irg.print_expr idx;	(* !!DEBUG!! *)
print_string ", ub="; Irg.print_expr ub;	(* !!DEBUG!! *)
print_string ", lb="; Irg.print_expr lb;	(* !!DEBUG!! *)
print_char '\n';				(* !!DEBUG!! *)*)
	trace "resolve_alias 1";

	let t = Irg.CARD(32) in
	let const c =
		Irg.CONST (t, Irg.CARD_CONST (Int32.of_int c)) in 
	let add e1 e2 =
		if e1 = Irg.NONE then e2 else
		Irg.BINOP (t, Irg.ADD, e1, e2) in
	let mul e1 e2 =
		if e1 = Irg.NONE then Irg.NONE else
		Irg.BINOP (t, Irg.MUL, e1, e2) in
	let div e1 e2 =
		if e1 = Irg.NONE then Irg.NONE else
		Irg.BINOP (t, Irg.DIV, e1, e2) in
	let rem e1 e2 =
		if e1 = Irg.NONE then Irg.NONE else
		Irg.BINOP (t, Irg.MOD, e1, e2) in

	let convert tr v =
		let (r, i, il, ub, lb, ta) = v in
		if ta = Irg.NO_TYPE then (r, i, il, ub, lb, tr) else
		let sa = Sem.get_type_length ta in
		let sr = Sem.get_type_length tr in
		if sa = sr then v else
		if sa < sr then
			let f = const (sr / sa) in
			(r, div i f, 1, add ub (rem i f), add lb (rem i f), t) 
		else
			let f = sa / sr in
			(r, mul i (const f), il * f, ub, lb, t) in

	let shift s v =
		let (r, i, il, ub, lb, t) = v in
		(r, add i s, il, ub, lb, t) in
	
	let field u l v =
		let (r, i, il, ub, lb, t) = v in
		(r, i, il, add lb u, add lb l, t) in

	let set_name name v =
		let (_, i, il, ub, lb, t) = v in
		(name, i, il, ub, lb, t) in

	let rec process_alias tr attrs v =
		let v = convert tr v in
		match get_alias attrs with
		| Irg.LOC_NONE -> v
		| Irg.LOC_CONCAT _ -> failwith "bad relocation alias"
		| Irg.LOC_REF (tr, r, idxp, ubp, lbp) ->
			let v = set_name r v in
			let v = if idx = Irg.NONE then v else shift idx v in
			let v = if ub = Irg.NONE then v else field ub lb v in
			process v

	and process v =
		let (r, i, il, ub, lb, t) = v in
		match Irg.get_symbol r with
		| Irg.REG (_, _, tr, attrs) ->
			process_alias tr attrs v
		| Irg.VAR (_, _, tr) ->
			process_alias tr [] v
		| Irg.LET _ ->
			(name, Irg.NONE, 1, Irg.NONE, Irg.NONE, Irg.NO_TYPE)
		| Irg.MEM (_, _, tr, attrs) ->
			process_alias tr attrs v
		| _ ->
(*
			Printf.printf "BAD_ALIAS: %s\n" r;	(* !!DEBUG!! *)
			Irg.print_spec (Irg.get_symbol r);	(* !!DEBUG!! *)
			print_char '\n';			(* !!DEBUG!! *)
*)
			failwith "bad alias" in
	
	process (name, idx, 1, ub, lb, Irg.NO_TYPE)


(** Unalias an expression.
	@param name		Name of the accessed state resource.
	@param idx		Index (may be NONE)
	@param ub		Upper bit number (may be NONE)
	@patam lb		Lower bit number (may be NONE)
	@return			Unaliased expression. *)
let unalias_expr name idx ub lb =
(*print_string ("unalias_expr name=" ^ name);	(* !!DEBUG!! *)
print_string ", idx="; Irg.print_expr idx;	(* !!DEBUG!! *)
print_string ", ub="; Irg.print_expr ub;	(* !!DEBUG!! *)
print_string ", lb="; Irg.print_expr lb;	(* !!DEBUG!! *)
print_char '\n';				(* !!DEBUG!! *)*)
	let (r, i, il, ubp, lbp, t) = resolve_alias name idx ub lb in
	let t = Irg.CARD(32) in
	let const c =
		Irg.CONST (t, Irg.CARD_CONST (Int32.of_int c)) in 
	let add e1 e2 =
		if e1 = Irg.NONE then e2 else
		Irg.BINOP (t, Irg.ADD, e1, e2) in
	let rec concat l tt =
		if l = 0 then
			if i = Irg.NONE then Irg.REF r
			else Irg.ITEMOF (t, r, i)
		else
			Irg.BINOP(tt, Irg.CONCAT,
				Irg.ITEMOF (t, r, add i (const l)),
				concat (l - 1) tt) in
	let field e ub lb tt =
		if ub = Irg.NONE then e
		else Irg.BITFIELD(tt, e, ub, lb) in

	match Irg.get_symbol name with
	| Irg.REG (_, _, tt, _) ->
		field (concat (il - 1) tt) ubp lbp tt
	| Irg.MEM (_, _, tt, _) ->
		field (concat 0 tt) ub lb tt
	| s ->
		failwith "unalias_expr"


(** Prepare expression for generation.
	@param info		Generation information.
	@param stats	Prefix statements.	
	@param expr		Expression to prepare.
	@return			(new prefix statements, prepared expression) *)
let rec prepare_expr info stats expr =

	let set typ var expr =
		Irg.SET (Irg.LOC_REF (typ, var, Irg.NONE, Irg.NONE, Irg.NONE), expr) in
	let unalias name idx =
	(*print_string ("prepare_expr::unalias name=" ^ name);	(* !!DEBUG!! *)
	print_string ", idx=";			(* !!DEBUG!! *)
	Irg.print_expr idx;			(* !!DEBUG!! *)
	print_char '\n';			(* !!DEBUG!! *)*)
		match Irg.get_symbol name with
		| Irg.REG _ | Irg.MEM _ ->
			unalias_expr name idx Irg.NONE Irg.NONE
		| Irg.VAR (_, cnt, Irg.NO_TYPE) ->
			expr
		| Irg.VAR (_, cnt, t) ->
			add_var info name cnt t; expr
		| _ ->
			expr in

	match expr with
	| Irg.REF name ->
		(stats, unalias name Irg.NONE)
	| Irg.NONE
	| Irg.CONST _ -> (stats, expr)
	| Irg.COERCE (typ, expr) ->
		let (stats, expr) = prepare_expr info stats expr in
		(stats, Irg.COERCE (typ, expr))
	| Irg.FORMAT (fmt, args) ->
		let (stats, args) = prepare_exprs info stats args in
		(stats, Irg.FORMAT (fmt, List.rev args))
	| Irg.CANON_EXPR (typ, name, args) -> 
		let (stats, args) = prepare_exprs info stats args in
		(stats, Irg.CANON_EXPR (typ, name, List.rev args))
	| Irg.FIELDOF (typ, base, id) ->
		(stats, Irg.FIELDOF (typ, base, id))
	| Irg.ITEMOF (typ, tab, idx) ->
		let (stats, idx) = prepare_expr info stats idx in
		(stats, unalias tab idx)
	| Irg.BITFIELD (typ, expr, lo, up) ->
		let (stats, expr) = prepare_expr info stats expr in
		let (stats, lo) = prepare_expr info stats lo in
		let (stats, up) = prepare_expr info stats up in
		(stats, Irg.BITFIELD (typ, expr, lo, up))
	| Irg.UNOP (typ, op, arg) ->
		let (stats, arg) = prepare_expr info stats arg in
		(stats, Irg.UNOP (typ, op, arg))
	| Irg.BINOP (typ, op, arg1, arg2) ->
		let (stats, arg1) = prepare_expr info stats arg1 in
		let (stats, arg2) = prepare_expr info stats arg2 in
		(stats, Irg.BINOP (typ, op, arg1, arg2))

	| Irg.IF_EXPR (typ, cond, tpart, epart) ->
		let (stats, cond) = prepare_expr info stats cond in
		let (stats, tpart) = prepare_expr info stats tpart in
		let (stats, epart) = prepare_expr info stats epart in
		let tmp = new_temp info typ in
		(Irg.SEQ(stats, Irg.IF_STAT (cond, set typ tmp tpart, set typ tmp epart)),
		Irg.REF tmp)
		
	| Irg.SWITCH_EXPR (typ, cond, cases, def) ->
		let tmp = new_temp info typ in
		let (stats, cond) = prepare_expr info stats cond in
		let (stats, def) = prepare_expr info stats def in
		let (stats, cases) = List.fold_left
			(fun (stats, cases) (case, expr) ->
				let (stats, expr) = prepare_expr info stats expr in
				(stats, (case, set typ tmp expr) :: cases))
			(stats, [])
			cases in
		(Irg.SEQ(stats, Irg.SWITCH_STAT (cond, cases, set typ tmp def)),
		Irg.REF tmp)
	
	| Irg.ELINE (file, line, expr) ->
		let (stats, expr) = prepare_expr info stats expr in
		(stats, Irg.ELINE (file, line, expr))

and prepare_exprs info (stats: Irg.stat) (args: Irg.expr list) =
	List.fold_left
		(fun (stats, args) arg ->
			let (stats, arg) = prepare_expr info stats arg in
			(stats, arg::args))
		(stats, [])
		args


(** Build a sequence, optimizing the result if one is a nop.
	@param s1	First statement.
	@param s2	Second statement.
	@return		Sequenced statements. *)
let seq s1 s2 =
		if s1 = Irg.NOP then s2 else
		if s2 = Irg.NOP then s1 else
		Irg.SEQ (s1, s2)


(** Build a sequence from a list of statements.
	@param list		List of statements.
	@return			Sequence of statements. *)
let rec seq_list list =
	match list with
	| [] -> Irg.NOP
	| [s] -> s
	| s::t -> seq s (seq_list t)


(** Unalias an assignement.
	@param info		Generation information.
	@param stats	Side-effect statements.
	@param name		Name of the accessed state resource.
	@param idx		Index (may be NONE)
	@param ub		Upper bit number (may be NONE)
	@param lb		Lower bit number (may be NONE)
	@param			Expression to assign.
	@return			statements *)
let unalias_set info stats name idx ub lb expr =
(*print_string ("unalias_set name=" ^ name);	(* !!DEBUG!! *)
print_string ", idx="; Irg.print_expr idx;	(* !!DEBUG!! *)
print_string ", ub="; Irg.print_expr ub;	(* !!DEBUG!! *)
print_string ", lb="; Irg.print_expr lb;	(* !!DEBUG!! *)
print_string ", expr="; Irg.print_expr expr;	(* !!DEBUG!! *)
print_char '\n';				(* !!DEBUG!! *)*)
	trace "unalias_set 1";
	let (r, i, il, ubp, lbp, t) = resolve_alias name idx ub lb in
	trace "unalias_set 2";

	(*let addn e1 e2 =
		if e1 = Irg.NONE then e2 else addi e1 e2  in*)
	let index_t = Irg.CARD(32) in
	let index c = Irg.CONST (index_t, Irg.CARD_CONST (Int32.of_int c)) in
	let addi e1 e2 = Irg.BINOP (index_t, Irg.ADD, e1, e2) in
	let subi e1 e2 = Irg.BINOP (index_t, Irg.SUB, e1, e2) in
	let shr e1 e2 = Irg.BINOP (index_t, Irg.RSHIFT, e1, e2) in
	let field e ub lb  = Irg.BITFIELD (t, e, ub, lb) in

	let set_full i ub lb e =
		Irg.SET (Irg.LOC_REF (t, r, i, ub, lb), e) in
	(*let set _ = set_full Irg.NONE Irg.NONE Irg.NONE in*)
	let set_field ub lb e = set_full Irg.NONE ub lb e in
	let set_item i e = set_full i Irg.NONE Irg.NONE e in
	let sett t n e =
		Irg.SET (Irg.LOC_REF (t, n, Irg.NONE, Irg.NONE, Irg.NONE), e) in

	let rec set_concat l s =
		if l = 0 then set_item i expr else
		seq (set_item
				(addi i (index l))
				(field expr
					(index (l * s + s - 1))
					(index (l * s))))
			(set_concat (l - 1) s) in

	let rec set_concat_field l s =
		if l = 0 then set_field ub lb expr else
		seq
			(set_full
				(addi i (index l))
				(subi ub (index (l * s)))
				(subi lb (index (l * s)))
				(shr expr (index (l * s))))
			(set_concat_field (l - 1) s) in

	let process tt =
		if ub = Irg.NONE then
			if il = 1 then seq stats (set_item i expr) else
			let name = new_temp info tt in
			seq (seq stats (sett tt name expr)) (set_concat (il - 1) (Sem.get_type_length t))
		else
			if il = 1 then seq stats (set_full i ubp lbp expr) else
			let name = new_temp info tt in
			seq (seq stats (sett tt name expr)) (set_concat_field (il - 1) (Sem.get_type_length t)) in

	match Irg.get_symbol name with
	| Irg.REG (_, _, tt, _) ->
		process tt
	| Irg.MEM (_, _, tt, _) ->
		seq stats (set_full i ub lb expr)
	| Irg.VAR (_, cnt, tt) ->
		add_var info name cnt tt;
		seq stats (set_full i ub lb expr)
	| _ -> failwith "unalias_expr"


(* !!TODO!! move to Sem *)
let get_loc_size l =
	Sem.get_type_length (Sem.get_loc_type l)	

(** Prepare a statement before generation. It includes:
	- preparation of expressions,
	- split of concatenation location in assignment.
	@param info		Generation information.
	@param stat		Statement to prepare.
	@return			Prepared statement. *)
let rec prepare_stat info stat =
(*print_string "prepare_stat stat="; Irg.print_statement stat;	(* !!DEBUG!! *)*)
	trace "prepare_stat 1";
	let set t n e =
		Irg.SET (Irg.LOC_REF (t, n, Irg.NONE, Irg.NONE, Irg.NONE), e) in
	let refto n = Irg.REF n in
	let rshift t v s = Irg.BINOP (t, Irg.RSHIFT, v, s) in
	let index c = Irg.CONST (Irg.CARD(32), Irg.CARD_CONST (Int32.of_int c)) in
	
	let rec prepare_set stats loc expr =
		(*print_string "prepare_stat::prepare_set loc="; Irg.print_location loc;	(* !!DEBUG!! *)
		print_string ", expr="; Irg.print_expr expr;				(* !!DEBUG!! *)
		print_string "stat="; Irg.print_statement stats;			(* !!DEBUG!! *)*)
		trace "prepare_set 1";
		match loc with
		| Irg.LOC_NONE ->
			failwith "no location to set (3)"
		| Irg.LOC_REF (_, r, i, u, l) ->
			unalias_set info stats r i u l expr
		| Irg.LOC_CONCAT (t, l1, l2) ->
			let tmp = new_temp info t in
			let stats = seq stats (set t tmp expr) in
			let stats = prepare_set stats l1
				(rshift t (refto tmp) (index (get_loc_size l2))) in
			prepare_set stats l2 (refto tmp) in
	
	match stat with
	| Irg.NOP
	| Irg.ERROR _ ->
		stat
	| Irg.SEQ (s1, s2) ->
		Irg.SEQ (prepare_stat info s1, prepare_stat info s2)
	| Irg.SET (loc, expr) ->
		let (stats, expr) = prepare_expr info Irg.NOP expr in
		prepare_set stats loc expr
	| Irg.CANON_STAT (name, args) ->
		let (stats, args) = prepare_exprs info Irg.NOP args in
		Irg.CANON_STAT (name, List.rev args)

	| Irg.IF_STAT (cond, tpart, epart) ->
		let (stats, cond) = prepare_expr info Irg.NOP cond in
		Irg.SEQ (stats, Irg.IF_STAT (cond,
			prepare_stat info tpart,
			prepare_stat info epart))

	| Irg.SWITCH_STAT (cond, cases, def) ->
		let (stats, cond) = prepare_expr info Irg.NOP cond in
		let cases = List.map
			(fun (case, stat) -> (case, prepare_stat info stat))
			cases in
		Irg.SEQ (stats, Irg.SWITCH_STAT (cond, cases, prepare_stat info def))

	| Irg.LINE (file, line, stat) ->
		Irg.LINE (file, line, prepare_stat info stat)

	| Irg.SETSPE (l, e) ->
		(* !!TODO!! fix type here *)
		prepare_stat info (Irg.SET (l, e))

	| Irg.EVAL name ->
		prepare_call info name;
		stat
		
	| Irg.EVALIND _ ->
		failwith "prepare_stat: must have been removed !"

and prepare_call info name =
	if not (StringHashtbl.mem info.attrs name) then
		begin
			StringHashtbl.add info.attrs name Irg.NOP;
			StringHashtbl.replace info.attrs name (prepare_stat info (get_stat_attr name))
		end


(** Generate a prepared expression.
	@param info		Generation information.
	@param expr		Expression to generate. *)	
let rec gen_expr info (expr: Irg.expr) =
	let out = output_string info.out in

	let get_function op t =
		match op with
		| Irg.LROTATE -> (true, Printf.sprintf "%s_rotate_left%s" info.proc (type_to_mem(convert_type t)))
		| Irg.RROTATE -> (true, Printf.sprintf "%s_rotate_right%s" info.proc (type_to_mem(convert_type t)))
		| Irg.EXP -> (false, Printf.sprintf "%s_exp%s" info.proc (type_to_mem(convert_type t)))
		| Irg.CONCAT -> (false, Printf.sprintf "%s_concat%s" info.proc (type_to_mem(convert_type t)))
		| _ -> (false, "") in

	match expr with
	| Irg.NONE -> ()

	| Irg.CONST (_, Irg.NULL) -> failwith "null constant"
	| Irg.CONST (_, Irg.CARD_CONST v) -> out (Int32.to_string v)
	| Irg.CONST (_, Irg.CARD_CONST_64 v) -> out (Int64.to_string v); out "LL"
	| Irg.CONST (_, Irg.STRING_CONST s) -> out "\""; out (cstring s); out "\""
	| Irg.CONST (_, Irg.FIXED_CONST v) -> Printf.fprintf info.out "%f" v  
	
	| Irg.REF name ->
		(match Irg.get_symbol name with
		| Irg.LET (_, cst) -> gen_expr info (Irg.CONST (Irg.NO_TYPE, cst)) 
		| Irg.VAR _ -> out name
		| Irg.REG _ -> out (state_macro info name)
		| Irg.MEM _ -> out (state_macro info name)
		| Irg.PARAM _ -> out (param_macro info name)
		| Irg.ENUM_POSS (_, _, v, _) -> out (Int32.to_string v)
		| s ->
			out name (*failwith "expression form must have been removed")*) )

	| Irg.ITEMOF (_, name, idx) ->
		(match Irg.get_symbol name with
		| Irg.VAR _ ->
			out name;
			out "[";
			gen_expr info idx;
			out "]"
		| Irg.REG _ -> out (state_macro info name); out "["; gen_expr info idx; out "]"
		| Irg.MEM (_, _, typ, _)  ->
			out info.proc;
			out "_mem_read";
			out (type_to_mem (convert_type typ));
			out "(";
			out (state_macro info name);
			out ", ";
			gen_expr info idx;
			out ")"
		| _ -> failwith "invalid itemof")

	| Irg.BITFIELD (typ, expr, lo, up) ->
		out info.proc;
		out "_field";
		out (type_to_mem (convert_type typ));
		out "(";
		gen_expr info expr;
		out ", ";
		gen_expr info lo;
		out ", ";
		gen_expr info up;
		out ")"

	| Irg.UNOP (_, op, e) -> 
		convert_unop info.out op;
		gen_expr info e

	| Irg.BINOP (t, op, e1, e2) ->
		let size, fname = get_function op t in
		if fname = "" then
			begin
				out "(";
				gen_expr info e1;
				out " ";
				convert_binop info.out op;
				out " ";
				gen_expr info e2;
				out ")"
			end
		else
			begin
				out fname;
				out "(";
				gen_expr info e1;
				out ", ";
				gen_expr info e2;
				if size then begin
					out ", ";
					Printf.fprintf info.out "%d" (Sem.get_type_length t);
				end;
				out ")"
			end

	| Irg.COERCE (typ, expr) ->
		let mask m =
			gen_expr info expr;
			out "& 0x";
			Printf.fprintf info.out "%LX" (Int64.sub (Int64.shift_left Int64.one m) Int64.one) in
		let apply pref suff = out pref; gen_expr info expr; out suff in
		let trans _ = gen_expr info expr in
		let otyp = Sem.get_type_expr expr in
		let to_range lo up =
			apply (Printf.sprintf "%s_check_range(" info.proc) (Printf.sprintf  ", %ld, %ld)" lo up) in
		let to_enum vals =
			apply (Printf.sprintf "%s_check_enum(" info.proc) (Printf.sprintf ", %d)" (List.length vals)) in
		let coerce fn =
			apply (Printf.sprintf "%s_coerce_%s(" info.proc fn) ")" in
		if typ = otyp then gen_expr info expr else
		(match (typ, otyp) with
		| Irg.BOOL, Irg.INT _
		| Irg.BOOL, Irg.CARD _
		| Irg.BOOL, Irg.FLOAT _
		| Irg.BOOL, Irg.RANGE _
		| Irg.BOOL, Irg.ENUM _ -> apply "((" ") ? : 1 : 0"
		| Irg.INT _, Irg.BOOL		
		| Irg.INT _, Irg.INT _ -> trans ()
		| Irg.INT n, Irg.CARD m when n > m -> mask m
		| Irg.INT _, Irg.CARD _ -> trans ()
		| Irg.INT 32, Irg.FLOAT (23, 9) -> coerce "ftoi"
		| Irg.INT 64, Irg.FLOAT (52, 12) -> coerce "dtoi"
		| Irg.INT _, Irg.RANGE _
 		| Irg.INT _, Irg.ENUM _ -> trans ()
		| Irg.CARD _, Irg.BOOL -> trans ()
		| Irg.CARD n, Irg.INT m when n > m -> mask m
		| Irg.CARD _, Irg.INT _ 
		| Irg.CARD _, Irg.CARD _ -> trans ()
		| Irg.CARD 32, Irg.FLOAT (23, 9) -> coerce "ftou"
		| Irg.CARD 64, Irg.FLOAT (52, 12) -> coerce "dtou"
		| Irg.CARD _, Irg.RANGE _
		| Irg.CARD _, Irg.ENUM _ -> trans ()
		| Irg.FLOAT (23, 9), Irg.INT 32 -> coerce "itof"
		| Irg.FLOAT (23, 9), Irg.CARD 32 -> coerce "utof"
		| Irg.FLOAT (52, 12), Irg.INT 64 -> coerce "itod"
		| Irg.FLOAT (52, 12), Irg.CARD 64 -> coerce "itod"
		| Irg.RANGE (lo, up), _ -> to_range lo up
		| Irg.ENUM vals, _ -> to_enum vals
		| _ -> error_on_expr "unsupported coercition" expr)
	
	| Irg.CANON_EXPR (_, name, args) ->
		out name;
		out "(";
		ignore (List.fold_left
			(fun com arg -> if com then out ", "; gen_expr info arg; true)
			false args);
		out ")"
	
	| Irg.ELINE (file, line, expr) -> gen_expr info expr
	| Irg.FORMAT _ -> failwith "format out of image/syntax attribute"
	| Irg.IF_EXPR _
	| Irg.SWITCH_EXPR _
	| Irg.FIELDOF _ -> failwith "should have been reduced"


(** Generate a prepared statement.
	@param info		Generation information.
	@param stat		Statement to generate. *)
let rec gen_stat info stat =
	trace "gen_stat 1";
	let out = output_string info.out in

	let iter_args args =
		ignore(List.fold_left
			(fun first arg ->
				if not first then out ", ";
				ignore (gen_expr info arg); false)
			true
			args) in

	let set_field typ id idx lo up expr =
		if lo = Irg.NONE then expr
		else
			Irg.CANON_EXPR (
				typ,
				Printf.sprintf "%s_set_field%s"
					info.proc (type_to_mem (convert_type typ)),
				[
					if idx = Irg.NONE then Irg.REF id
					else Irg.ITEMOF (typ, id, idx);
					expr;
					lo;
					up
				]
			) in

	match stat with
	| Irg.NOP -> ()

	| Irg.SEQ (s1, s2) ->
		gen_stat info s1; gen_stat info s2

	| Irg.SET (Irg.LOC_REF(typ, id, idx, lo, up), expr) ->
		out "\t";
		(match Irg.get_symbol id with
		| Irg.VAR _ ->
			out id;
			if idx <> Irg.NONE then
				(out "["; gen_expr info idx; out "]");
			out " = ";
			gen_expr info (set_field typ id idx lo up expr);
			out ";\n"
		| Irg.REG _ ->
			out (state_macro info id);
			if idx <> Irg.NONE then
				(out "["; gen_expr info idx; out "]");
			out " = ";
			gen_expr info (set_field typ id idx lo up expr);
			out ";\n"
		| Irg.MEM _ ->
			out (Printf.sprintf "%s_mem_write%s(" info.proc
				(type_to_mem (convert_type typ)));
			out (state_macro info id);
			out ", ";
			gen_expr info idx;
			out ", ";
			gen_expr info (set_field typ id Irg.NONE lo up expr);
			out ");\n"
		| s ->
			Printf.printf "==> %s\n" id;
			Irg.print_spec s;
			failwith "gen stat 1")
	| Irg.SET _ ->
		failwith "should have been removed"

	| Irg.CANON_STAT (name, args) ->
		out "\t";
		out name;
		out "(";
		iter_args args;
		out ");\n"

	| Irg.ERROR msg ->
		Printf.fprintf info.out "\t%s_error(\"%s\");\n"
			info.proc
			(cstring msg)

	| Irg.IF_STAT (cond, tpart, epart) -> 
		out "\tif(";
		gen_expr info cond;
		out ") {\n";
		gen_stat info tpart;
		out "\t}\n";
		if not (is_nop epart) then
			begin
				out "\telse {\n";
				gen_stat info epart;
				out "\t}\n"
			end

	| Irg.SWITCH_STAT (cond, cases, def) ->
		out "\tswitch(";
		gen_expr info cond;
		out ") {\n";
		List.iter
			(fun (case, stat) ->
				out "\t case ";
				gen_expr info case;
				out ":\n";
				gen_stat info stat)
			cases;
		if def <> Irg.NOP then
			begin
				out "\tdefault:\n";
				gen_stat info def; (*stat;*)
			end;
		out "\t}\n"

	| Irg.LINE (_, _, stat) -> 
		gen_stat info stat

	| Irg.EVAL name ->
		gen_call info name

	| Irg.EVALIND _
	| Irg.SETSPE _ ->
		failwith "must have been removed"
		
and gen_call info name =
	
	(* recursive call ? *)
	if 	List.mem_assoc name info.calls then
		Printf.fprintf info.out "\tgoto %s;\n" (List.assoc name info.calls)
	
	(* normal call *)
	else
		begin
			let stat = StringHashtbl.find info.attrs name in
			let before = info.calls in
			if List.mem name info.recs then
				begin
					let lab = new_label info in 
					Printf.fprintf info.out "\t%s:\n" lab;
					info.calls <- (name, lab)::info.calls
				end;
			gen_stat info stat;
			info.calls <- before
		end



(** Get the list if recursives attribute starting from the given one.
	@param info		Generation information.
	@param name		Name of the first attribute.
	@return			List of recursive attributes. *)
let find_recursives info name =
	
	let rec look_attr name stack recs =
		if List.mem name stack then
			if List.mem name recs then recs
			else name::recs
		else
			let stack = name::stack in

			let rec look_stat stat recs =
				match stat with
				| Irg.NOP -> recs
				| Irg.SEQ (s1, s2) -> look_stat s1 (look_stat s2 recs)
				| Irg.EVAL name -> look_attr name stack recs
				| Irg.EVALIND _ -> failwith "find_recursives: EVALIND"
				| Irg.SET _ -> recs
				| Irg.CANON_STAT _ -> recs
				| Irg.ERROR _ -> recs
				| Irg.IF_STAT (_, s1, s2) -> look_stat s1 (look_stat s2 recs)
				| Irg.SWITCH_STAT (_, cases, def) ->
					look_stat def (List.fold_left
						(fun recs (_, s) -> look_stat s recs)
						recs
						cases)
				| Irg.SETSPE (loc, expr) ->
					(* !!TODO!! must no occurs next *)
					(*failwith "find_recursives: SETSPE"*)
					recs
				| Irg.LINE (_, _, s) -> look_stat s recs in

			match Iter.get_attr info.inst name with
			| Iter.STAT stat -> look_stat stat recs
			| _ -> failwith "find_recursives" in
	
	info.recs <- look_attr name [] []

(** generate the instruction responsibe for the incrementation of PCs,
we return an Irg.STAT which has to be transformed, useful to resolve alias
	@param	info		Generation information (PCs name)
	@return			an Irg.STAT object representing the sequence of the desired instructions *)
let gen_pc_increment info =
	let ppc_stat =
		if info.ppc_name = "" then
			Irg.NOP
		else
		(* cannot retrieve the type easily, not needed for generation *)
			(* PPC = PC *)
			Irg.SET(Irg.LOC_REF(Irg.NO_TYPE, info.ppc_name, Irg.NONE, Irg.NONE, Irg.NONE), Irg.REF(info.pc_name))
	in
	let npc_stat =
		if info.npc_name = "" then
			Irg.NOP
		else
		(* cannot retrieve the type easily, not needed for generation *)
			(* NPC = NPC + (size of current instruction) *)
			Irg.SET(Irg.LOC_REF(Irg.NO_TYPE, info.npc_name, Irg.NONE, Irg.NONE, Irg.NONE),
				Irg.BINOP(Irg.NO_TYPE, Irg.ADD,
					Irg.REF(info.npc_name),
					(* it is not easy to convert C code into nML *)
					Irg.REF(info.proc ^ "_inst_size_table[inst->ident]")))
					(*Irg.ITEMOF(Irg.NO_TYPE,
						info.proc ^ "_inst_size_table",
						Irg.REF("instr->ident")
						)))*)
					(*Irg.CONST(Irg.STRING,
					(* it is not easy to convert C code into nML *)
						Irg.STRING_CONST(info.proc ^ "_inst_size_table[inst->ident]"))
						))*)
	in
	let pc_stat =
		if info.npc_name = "" then
			(* PC = PC + (size of current instruction) *)
			Irg.SET(Irg.LOC_REF(Irg.NO_TYPE, info.pc_name, Irg.NONE, Irg.NONE, Irg.NONE),
				Irg.BINOP(Irg.NO_TYPE, Irg.ADD,
					Irg.REF(info.pc_name),
					Irg.CONST(Irg.STRING,
					(* it is not easy to convert C code into nML *)
						Irg.STRING_CONST(info.proc ^ "_inst_size_table[inst->ident]"))
						))
		else
			(* PC = NPC *)
			Irg.SET(Irg.LOC_REF(Irg.NO_TYPE, info.pc_name, Irg.NONE, Irg.NONE, Irg.NONE), Irg.REF(info.npc_name))
	in
	if info.pc_name = "" then
		failwith "PC not defined, one register must have the \"pc\" attribute ( __attr(pc) )"
	else
		Irg.SEQ(ppc_stat, Irg.SEQ(pc_stat, npc_stat))



(** Generate an action.
	@param info		Generation information.
	@param name		Name of the attribute. *)
let gen_action info name =

	(* prepare statements *)
	find_recursives info name;
	prepare_call info name;
	
	(* generate the code *)
	declare_temps info;
	
	(* PCs incrementation *)
	gen_stat info (gen_pc_increment info);
	
	(* generate the code *)
	gen_call info name;	
	cleanup_temps info;
	StringHashtbl.clear info.attrs


(** Return the code corresponding to what is found in the op init.
	@param info		Generation information.*)
let get_init_code _ =
	let init_sp = Irg.get_symbol "init"
	in
	match init_sp with
	Irg.AND_OP(n, p, al) ->
		(match Iter.get_attr init_sp "action" with
		Iter.STAT(s) ->
			s
		| _ ->
			failwith "shouldn't happen ! (toc.ml::get_init_code::init_action)"
		)
	| _ ->
		failwith "bad init, nML symbol init must be an AND OP only"
