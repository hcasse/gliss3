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

(** IRG module is dedicated to the Intermediate Representation in GLISS (IRG) of SimNML
	and to its associate facilities.

	{2 Intermediate representation}
	The intermediate representation uses a recursive tree of several datatypes.
	Interesting types includes:
	- {!spec} - top level specification item (type, constant, operation, mode, etc).
	- {!attr} - representation of attributes on object supporting them (AND operation, AND mode, registers, etc);
	  Attributes supports values that may be locations, expression or statements.
	- {!location} - allows to design resources supporting assignments (like registers, memories and variables).
	- {!expr} - represents a computation expression (i.e. constant, binary or unary operations, resource access, etc).
	- {!stat} - represents statements in SimNML i.e. assignment, sequence, selection, multiple-selection.

	Once parsed, a SimNML file (extension [.nmp] or [.nml]) is a collection of specifications
	that are stored in the main dictionnary provided by this module.

	Functions this collection of specifications includes:
	- {!add_attr} to add an attribute,
	- {!add_canon} to add and declare a canonical function,
	- {!add_param} to add a parameter,
	- {!add_pos} to add source information for a symbol,
	- {!add_symbol} to add a new symbol,
	- {!fold} to perform a computation on the set of symbols,
	- {!get_canon} to get a canonical function,
	- {!get_isize} to get the instruction size special constant,
	- {!get_proc_name} to get ISA name,
	- {!get_root} to get the root operation(s),
	- {!get_symbol} to get a symbol by its name,
	- {!iter} to iterate on the set of symbols,
	- {!is_defined} to test if a symbol is defined,
	- {!is_defined_canon} to test if a canonical function is defined,
	- {!pos_of} to get source information about a symbol,
	- {!rm_symbol} to remove a symbol.

	This module provides also function to serialize/deserialize the instruction to/from
	a file (files with extension [.irg]:
	- {!load}
	- {!save}

	{2 Working with AND-mode or -operations}

	IRG does not provide a hierarchical table of symbols. As is, the symbol tables contains top-level
	symbols, that is, the top-level specification. When some one has to work inside
	an AND-mode or an AND-operation, the symbol table needs to be populated with parameters and
	attributes. This is done with function:
	- {!param_stack}
	- {!attr_stack}

	After the work has been done, the symbol table need to be cleaned with functions:
	- {!param_unstack}
	- {!attr_unstack}

	{2 Useful Accessors}

	As IRG is not so easy to handle, some functions are provided to help the user to handle attributes:
	- {!attr_defined} to test if an attribute is in a list of attributes,
	- {!attr_expr} to get an attribute of type expression,
	- {!attr_loc} to get an attribute of type location,
	- {!attr_name} to get the name of an attribute,
	- {!attrs_of} to get the attributes of a specification,
	- {!set_attr} to set an attribute value in an attribute list.

	Other function are useful to handle specifications:
	- {!escape_eline} allows to remove source information around an expression,
	- {!is_reg} to test if a symbol is a register,
	- {!name_of} to get name of a specification,
	- {!string_of_binop} to convert binary operator to string,
	- {!string_of_unop} to convert unary operator to string.

	Some functions are designed to support format strings:
	- {!is_bin_format} to test if an escape sequence is binary,
	- {!is_float_format} to test if an escape sequence is float,
	- {!is_int_format} to test if an escape sequence is integer,
	- {!is_text_format} to test if ane scape sequence is a string/text,
	- {!split_format_string} to split format string from [format] function.

	Other functions and data allows working with source information :
	- {!line_from_expr} to lookup in expression for source information,
	- {!line_from_stat} to lookup in statement for source information,
	- {!no_line} represents empty source information.

	{2 Pretty Printing}

	IRG module provides lots of function to pretty-print the IRG elements on standard output:
	- {!print_attr},
	- {!print_const},
	- {!print_expr},
	- {!print_location},
	- {!print_mem_attr},
	- {!print_mem_attrs},
	- {!print_pos},
	- {!print_spec},
	- {!print_statement},
	- {!print_type},
	- {!print_type_expr}.
	Or any output stream:
	- {!output_attr},
	- {!output_const},
	- {!output_expr},
	- {!output_location},
	- {!output_mem_attr},
	- {!output_mem_attrs},
	- {!output_spec},
	- {!output_statement},
	- {!output_type},
	- {!output_type_expr}.

	As these functions may be a bit tedious to use, it is possible to combine alltogether
	in a list of {!printable} (supporting statement, expressiones, type or simple string or integers).
	The following functions supports this feature:
	- {!output},
	- {!outputln},
	- {!prerr},
	- {!prerrln}
	- {!print},
	- {!println}.

	{2 Error Management}

	This module provides also a collection of exception that may be used when IRG is processed:
	- {!SyntaxError} - only raised at parsing time to alert about a syntax error (this usually stops the parsing),
	- {!Error} - raised to alert about a error during IRG processing or building,
	- {!PreError} - errors that has not been localized.

	An important issue concerns the way the error message is provided to the final catcher of the error: basically,
	as function taking as unique parameter the stream to output to. This method provide bunch of flexibility in the way
	the message are displayed. In addition, this module provides several tool function to handle error message functions:
	- {!asis} text - generate an error function displaying the given text.
	- {!join} f1 f2 - generate an error function concatenating both messages of f1 and f2.
	- {!output} list - generate an error function displaying complex IRG message.

	Raising an error in a compiler may be a tricky action: to make useful by the human user, the error must be located
	relatively to the sources. IRG provides source information through special constructors put in {!expr} and {!stat} datatypes.
	When an error is raised during the processing of an expression or of a statement, one has to find back the closer
	source information to locate the error. IRG provide facilities to do that if the a {!PreError} is raised:
	an handler, associated with source information, will catch it and add missing source information to the error
	message, that is, will transform it in {!Error} exception.

	To support this scheme, the user must call the following functions to raise an error:
	- {!error} - to raise a PreError that will be fixed thereafter.
	- {!handle_error} file line f - will call f with () parameter but will catch any {!PreError} and will locate it.
	- {!complete_error} f file line - build an error function that will display file and line and append the f error function message.

	Alternatively, one may look in {!expr} or {!stat} to find closer source information (not very precise but may do the trick):
	- {!expr_error} e f -- generate an error with display closer source information from e and message from f.
	- {!stat_error} s f -- generate an error with display closer source information from s and message from f.

	For error concerning other IRG elements, like specification, IRG maintains an associative map between the element and the source information
	where they are declared. The following functions may be used:
	- {!error_symbol} name f - raise an error that displays source information of named symbol and message from f.
	- {!error_spec} spec f - raise an error that displays source information of the given specification and message from f.

	{2 Useful constants}

	This constants provides useful intermediate representations construction.
	- {!ieee754_32} IEEE-754 binary32 type
	- {!ieee754_64} IEEE-754 binary64 type
	- {!ieee754_128} IEEE-754 binary128 type
*)


exception SyntaxError of string
exception Error of (out_channel -> unit)
exception PreError of (out_channel -> unit)


(** Raise the error exception.
	@param f	Function to display error. *)
let error f = raise (PreError f)


(** Emit a PreError exception. PreError are error without source line information
	that need to be fulfilled with this information.
	@param msg		Message of the preerror.
	@raise PreError Ever.*)
let pre_error msg = raise (PreError (fun out -> output_string out msg))


(** Take a pre-error exception and build a complete error message.
	@param m		Message function (out_channel -> unit).
	@param f		Source file.
	@param l		Source line.
	@raise Error	Ever. *)
let complete_error m f l =
	raise (Error (fun out -> Printf.fprintf out "%s:%d: " f l; m out))


(** Manage errors from the IRG ELINE or SLINE.
	Call teh given function with the given argument and handle any PreError.
	@param file		Current source file.
	@param line		Current source line.
	@param fn 		Function to call with ().
	@return			Result of fn call.
	@raise Error	If there is an error. *)
let handle_error file line fn =
	try
		fn ()
	with PreError msg ->
		complete_error msg file line


(** Output the given text when the out stream will be passed.
	@param text		Text to display.
	@param out		Output stream. *)
let asis text out =
	output_string out text


(** Join two output functions.
	@param f1	First output function.
	@param f2	Second output function.
	@return		Output function joining both functions. *)
let join f1 f2 out =
	f1 out; f2 out


(** Join several error functions.
	@param lst	List of output functions.
	@param out	Output stream (usually left open). *)
let join_all lst out =
	List.iter (fun f -> f out) lst


(** Find index of an item in a list.
	@param item		Looked item.
	@param list		List to look in.
	@return			Position of item in list or -1. *)
let index_of item list =
	let rec scan list i =
		match list with
		| [] -> -1
		| h::_ when item = h -> i
		| _::t -> scan t (i + 1) in
	scan list 0


(** May be set to true to dump line information during expression/statement
	output. *)
let dump_lines = ref false
let dump_type = ref true

(** Type expression *)
type type_expr =
	  NO_TYPE					(** only used for undefined type *)
	| BOOL						(** boolean value, supports value 0 or 1 *)
	| INT of int				(** (bits number) signed integer type *)
	| CARD of int				(** (bits number) unsigned interger type *)
	| FIX of int * int			(** (integer part bits number, fractional part bits number) fixed point type *)
	| FLOAT of int * int		(** (exponent bits number, mantissa bits number) floating-point type *)
	| RANGE of int32 * int32	(** (low, up) integer range *)
	| STRING					(** string uniquely used for disassembly *)
	| ENUM of int32 list		(** (list of values) enumerated type *)
	| ANY_TYPE					(** used to represent a variable type (usually induced by OR-mode or operations *)


(** Float type for IEEE-754 binary32 *)
let ieee754_32 = FLOAT (8, 23)

(** Float type for IEEE-754 binary64 *)
let ieee754_64 = FLOAT (11, 52)

(** Float type for IEEE-754 binary128 *)
let ieee754_64 = FLOAT (15, 112)


(** Use of a type *)
type typ =
	  TYPE_ID of string
	| TYPE_EXPR of type_expr


(* Expressions *)
type unop =
	  NOT
	| BIN_NOT
	| NEG

type binop =
	  ADD
	| SUB
	| MUL
	| DIV
	| MOD
	| EXP
	| LSHIFT
	| RSHIFT
	| LROTATE
	| RROTATE
	| LT
	| GT
	| LE
	| GE
	| EQ
	| NE
	| AND
	| OR
	| BIN_AND
	| BIN_OR
	| BIN_XOR
	| CONCAT

(** Type to describe constants. *)
type const =
	  NULL						(** Null value. *)
	| CARD_CONST of Int32.t		(** Natural 32-bits. *)
	| CARD_CONST_64 of Int64.t	(** Natural 64-bits. *)
	| STRING_CONST of string	(** String. *)
	| FIXED_CONST of float		(** Fixed-precision real value. *)
	| CANON of string			(** Canonical constant. *)

type expr =
	  NONE																(** null expression *)
	| COERCE of type_expr * expr										(** explicit coercition *)
	| FORMAT of string * expr list										(** format expression *)
	| CANON_EXPR of type_expr * string * expr list						(** canonical expression *)
	| REF of type_expr * string											(** attribute / state item access *)
	| FIELDOF of type_expr * string * string							(** attribute access *)
	| ITEMOF of type_expr * string * expr								(** state item array access *)
	| BITFIELD of type_expr * expr * expr * expr						(** bit field access *)
	| UNOP of type_expr * unop * expr									(** unary operation (negation, not, etc) *)
	| BINOP of type_expr * binop * expr * expr							(** binary operation (arithmetic, logic, shift, etc) *)
	| IF_EXPR of type_expr * expr * expr * expr							(** if expression *)
	| SWITCH_EXPR of type_expr * expr * (expr * expr) list * expr		(** switch expression *)
	| CONST of type_expr * const										(** constant value *)
	| ELINE of string * int * expr										(** source/line information (file, line, expression) *)
	| CAST of type_expr * expr											(** binary cast (target type, expression *)

(** Statements *)
type location =
	| LOC_NONE												(** null location *)
	| LOC_REF of type_expr * string * expr * expr * expr	(** (type, memory name, index, lower bit, upper bit) *)
	| LOC_CONCAT of type_expr * location * location		(** concatenation of locations *)


(** argument of attributes *)
type attr_arg =
	| ATTR_ID of string * attr_arg list
	| ATTR_VAL of const


(** A statement in an action. *)
type stat =
	  NOP
	| SEQ of stat * stat										(** (s1, s2) Sequential execution of s1 then s2. *)
	| EVAL of string * string									(** (parameter, attribute) Access to an attribye. If parameter = "", this is a self-attribute. *)
	| SET of location * expr									(** (location, expression) Assignment of expression to the given location *)
	| CANON_STAT of string * expr list							(** Call to a canonical procedure. *)
	| ERROR of string	(* a changer : stderr ? *)
	| IF_STAT of expr * stat * stat								(** (condition, s1, s2) Selection statement. *)
	| SWITCH_STAT of expr * (expr * stat) list * stat			(** (value, cases, default case) Multiple selection. *)
	| LINE of string * int * stat								(** Used to store source information.  *)
	| LOCAL of string * string * type_expr						(** (variable name, original name, variable type) Local variable declaration *)
	| FOR of string * string * type_expr * const * const * stat	(** (variable name, unique name, variable type, initial bound, final bound, body) Loop definition *)


(** attribute specifications *)
type attr =
	| ATTR_EXPR of string * expr
	| ATTR_STAT of string * stat
	| ATTR_USES
	| ATTR_LOC of string * location

(* 2 kinds of canonicals, functions and constants *)
type canon_type =
	| CANON_FUNC
	| CANON_CNST


(** Specification of an item *)
type spec =
	  UNDEF
	| LET of string * type_expr * const
	| TYPE of string * type_expr
	| MEM of string * int * type_expr * attr list
	| REG of string * int * type_expr * attr list
	| VAR of string * int * type_expr * attr list
	| AND_MODE of string * (string * typ) list * expr * attr list
	| OR_MODE of string * string list
	| AND_OP of string * (string * typ) list * attr list
	| OR_OP of string * string list
	| RES of string
	| EXN of string
	| PARAM of string * typ
	| ATTR of attr
	| CANON_DEF of string * canon_type * type_expr * type_expr list	(** declaration of a canonical: name of canonical, type (fun or const name), return type, args type *)

(** Get the name from a specification.
	@param spec		Specification to get name of.
	@return			Name of the specification. *)
let name_of spec =
	match spec with
	  UNDEF -> "<undef>"
	| LET (name, _, _) -> name
	| TYPE (name, _) -> name
	| MEM (name, _, _, _) -> name
	| REG (name, _, _, _) -> name
	| VAR (name, _, _, _) -> name
	| AND_MODE (name, _, _, _) -> name
	| OR_MODE (name, _) -> name
	| AND_OP (name, _, _) -> name
	| OR_OP (name, _) -> name
	| RES (name) -> name
	| EXN (name) -> name
	| PARAM (name, _) -> name
	| ATTR(a) ->
		(match a with
		| ATTR_EXPR(name, _) -> name
		| ATTR_STAT(name, _) -> name
		| ATTR_USES -> "<ATTR_USES>"
		| ATTR_LOC(name, _) -> name)
	| CANON_DEF(name, _, _, _) -> name


(* Symbol table *)
module HashString =
struct
	type t = string
	let equal (s1 : t) (s2 : t) = s1 = s2
	let hash (s : t) = Hashtbl.hash s
end
module StringHashtbl = Hashtbl.Make(HashString)


(* Position *)

type pos_type = {ident:string; file : string; line : int}

(* This table is used to record the positions of the declaration of all symbols *)
let pos_table : pos_type StringHashtbl.t = StringHashtbl.create 211

(** Add a symbol to the localisation table.
	@param v_name	Name of the symbol to add.
	@param v_file	Name of the file where the symbol is declared
	@param v_line	Approximate line number of the declaration.
*)
let add_pos v_name v_file v_line =
	StringHashtbl.add pos_table v_name {ident=v_name;file=v_file;line=v_line}


(** Return string identifying file and line definition of the given symbol.
	@param sym	Required symbol.
	@return		File and line information about the symbol. *)
let pos_of sym =
	try
		let p = StringHashtbl.find pos_table sym in
		Printf.sprintf "%s:%d" p.file p.line
	with Not_found ->
		"<no line>"


(** Handle an error from a symbol.
	@param name		Name of the symbol.
	@param f		Message function.
	@raise Error	Located to the given symbol with the given message. *)
let error_symbol name f =
	raise (Error (fun out -> Printf.fprintf out "%s: " (pos_of name); f out))


(** Handle an error from a specification.
	@param spec		Specification.
	@param msg		Message of the error.
	@raise Error	Located to the given symbol with the given message. *)
let error_spec spec f =
	error_symbol (name_of spec) f


(** table of symbols of the current loaded NMP or IRG file. *)
let syms : spec StringHashtbl.t = StringHashtbl.create 211
let _ =
	StringHashtbl.add syms "__IADDR" (PARAM ("__IADDR", TYPE_EXPR (CARD(32))));
	StringHashtbl.add syms "__ISIZE" (PARAM ("__ISIZE", TYPE_EXPR (CARD(32))))


(** Get the symbol matching the given name or UNDEF if not found.
	@param n	Symbol to look for.
	@return		Symbol found or Irg.UNDEF (if not found). *)
let get_symbol n =
	try
		StringHashtbl.find syms n
	with Not_found ->
		UNDEF

(** Get processor name of the simulator *)
let get_proc_name () = match get_symbol "proc" with
	| LET(_, _, STRING_CONST(name)) -> name
	| _                         ->
		failwith ("Unable to find 'proc_name'."^
				  "'proc' must be defined as a string let")


(** Add a symbol to the namespace.
	@param name	Name of the symbol to add.
	@param sym	Symbol to add.
	@raise RedefinedSymbol	If the symbol is already defined. *)
let add_symbol name sym =



	(*
	transform an old style subpart alias declaration (let ax [1, card(16)] alias eax[16])
	into a new style one with bitfield notation (let ax [1, card(16)] alias eax<16..0>)
	before inserting the definition in the table
	*)
	let translate_old_style_aliases s =
		let is_array nm =
			match get_symbol nm with
			MEM(_, i, _, _)
			| REG(_, i, _, _)
			| VAR(_, i, _, _) ->
				i > 1
			| _ ->
				false
		in
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
			| RANGE (_, m) ->
				int_of_float (ceil ((log (float (Int32.to_int m))) /. (log 2.)))
			| NO_TYPE
			| STRING
			| ANY_TYPE ->
				failwith "irg: length unknown"
		in
		let b_o =
			match get_symbol "bit_order" with
			UNDEF -> true
			| LET(_, _, STRING_CONST(id)) ->
				if (String.uppercase id) = "UPPERMOST" then true
				else if (String.uppercase id) = "LOWERMOST" then false
				else failwith "'bit_order' must contain either 'uppermost' or 'lowermost'"
			| _ -> failwith "'bit_order' must be defined as a string let"
		in
		let rec change_alias_attr mem_attr_l n =
			let t = CARD(32)
			in
			let const c =
				CONST (t, CARD_CONST (Int32.of_int c))
			in
			let sub e1 e2 =
				BINOP (t, SUB, e1, e2)
			in
			match mem_attr_l with
			[] ->
				[]
			| a::b ->
				(match a with
				ATTR_LOC("alias", l) ->
					(match l with
					LOC_REF(typ, name, i, l, u) ->
						if is_array name then
							a::(change_alias_attr b n)
						else
							if l=NONE && u=NONE then
								if b_o then
									(ATTR_LOC("alias", LOC_REF(typ, name, NONE, i, sub i (sub (const n) (const 1)) (*i-(n-1)*))))::(change_alias_attr b n)
								else
									(ATTR_LOC("alias", LOC_REF(typ, name, NONE, sub i (sub (const n) (const 1)) (*i-(n-1)*), i)))::(change_alias_attr b n)
							else
								a::(change_alias_attr b n)
					| _ ->
						a::(change_alias_attr b n)
					)
				| _ ->
					a::(change_alias_attr b n)
				)
		in
		match s with
		MEM(name, size, typ, m_a_l) ->
			MEM(name, size, typ, change_alias_attr m_a_l (get_type_length typ))
		| REG(name, size, typ, m_a_l) ->
			REG(name, size, typ, change_alias_attr m_a_l (get_type_length typ))
		| _ ->
			s
	in

	(*if StringHashtbl.mem syms name
	then raise (Error (fun out -> Printf.fprintf out "ERROR: %s: symbol %s already defined at %s" (Lexer.current_loc ()) name (pos_of name)))
	else*) StringHashtbl.add syms name (translate_old_style_aliases sym)


(**	Check if a given name is defined in the namespace
		@param name	The name to check *)
let is_defined name = StringHashtbl.mem syms name

(**	Add a parameter in the namespace.
		This function don't raise RedefinedSymbol if the name already exits.
		It is used to temporary overwrite existing symbols with the same name than a parameter

		@param name	Name of the parameter to add.
		@param t	Type of the parameter to add.	*)
let add_param (name,t) =
	StringHashtbl.add syms name (PARAM (name,t))

(**	Remove a symbol from the namespace.
		@param name	The name of the symbol to remove. *)
let rm_symbol name=StringHashtbl.remove syms name

(**	Add a list of parameters to the namespace.
		@param l	The list of parameters to add.	*)
let param_stack l= List.iter add_param l

(**	Remove a list of parameters from the namespace.
		@param l	The list of parameters to remove.	*)
let param_unstack l= List.iter (StringHashtbl.remove syms) (List.map fst l)


(**	Add a attribute in the namespace.
		This function don't raise RedefinedSymbol if the name already exits.
		It is used to temporary overwrite existing symbols with the same name than an attribute
		@param name	name of the attribute
		@param attr	attribute to add.	*)
let add_attr attr =
	StringHashtbl.add syms (name_of (ATTR(attr))) (ATTR(attr))


(**	Add a list of attributes to the namespace.
		@param l	The list of attributes to add.	*)
let attr_stack l= List.iter add_attr l


(**	Remove a list of attributes from the namespace.
		@param l	The list of attributes to remove.	*)
let attr_unstack l= List.iter (StringHashtbl.remove syms) (List.map (fun x -> name_of (ATTR(x))) l)


(* --- canonical functions --- *)

type canon_name_type=
	 UNKNOW		(* this is used for functions not defined in the cannon_list *)
	|NAMED of string

(* canonical function table *)
module HashCanon =
struct
	type t = canon_name_type
	let equal (s1 : t) (s2 : t) = match (s1,s2) with
					(UNKNOW,UNKNOW)->true
					|(NAMED a,NAMED b) when a=b->true
					|_->false
	let hash (s : t) = Hashtbl.hash s
end
module CanonHashtbl = Hashtbl.Make(HashCanon)

type canon_fun={name : canon_name_type; type_fun : canon_type ; type_param : type_expr list ; type_res:type_expr}

(* the canonical functions space *)
let canon_table : canon_fun CanonHashtbl.t = CanonHashtbl.create 211

(* list of all defined canonical functions *)
let canon_list = [
			(* this is the "default" canonical function, used for unknown functions *)
			{ name=UNKNOW; type_fun=CANON_FUNC;type_param=[];type_res=ANY_TYPE };
			{ name=NAMED "print";type_fun=CANON_FUNC;type_param=[STRING];type_res=NO_TYPE };

			(* for debugging generation *)
			{name=NAMED "GLISS_IDX"; type_fun=CANON_CNST; type_param=[]; type_res=CARD(32); };
			{name=NAMED "GLISS_I"; type_fun=CANON_CNST; type_param=[]; type_res=CARD(32); };
			{name=NAMED "GLISS_L"; type_fun=CANON_CNST; type_param=[]; type_res=CARD(64); };
			{name=NAMED "GLISS_F"; type_fun=CANON_CNST; type_param=[]; type_res=FLOAT(23, 9); };
			{name=NAMED "GLISS_D"; type_fun=CANON_CNST; type_param=[]; type_res=FLOAT(52, 12); };
			{name=NAMED "GLISS_GET_I"; type_fun=CANON_FUNC; type_param=[CARD(32)]; type_res=NO_TYPE; };
			{name=NAMED "GLISS_GET_L"; type_fun=CANON_FUNC; type_param=[CARD(65)]; type_res=NO_TYPE; };
			{name=NAMED "GLISS_GET_F"; type_fun=CANON_FUNC; type_param=[FLOAT(23, 9)]; type_res=NO_TYPE; };
			{name=NAMED "GLISS_GET_D"; type_fun=CANON_FUNC; type_param=[FLOAT(52, 12)]; type_res=NO_TYPE; };
		 ]

(* we add all the defined canonical functions to the canonical functions space *)
let _ =	List.iter (fun e->CanonHashtbl.add canon_table e.name e) canon_list

(** Check if a canonical function is defined
	@param name	The name of the function to check *)
let is_defined_canon name = CanonHashtbl.mem canon_table (NAMED name)

(** Get a canonincal function
	@param name	The name of the canonical function to get
	@return A canon_fun element, it can be the function with the attribute name UNKNOW if the name given is not defined *)
let rec get_canon name=
	try
		CanonHashtbl.find canon_table (NAMED name)
	with Not_found -> CanonHashtbl.find canon_table UNKNOW

(** Add a canonical definition to the namespace.
	@param sym	Canonical specification (Irg.CANON_DEF(...)).
	@param fun_name	Canonical name. *)
let add_canon fun_name sym =
	let canon_def_sym =
		match sym with
		| CANON_DEF(n, typ, res, prms) -> {name = NAMED n; type_fun = typ; type_param = prms; type_res = res}
		| _ -> failwith "irg.ml::add_canon: shouldn't happen!\n"
	in
	let name = canon_def_sym.name
	in
	if CanonHashtbl.mem canon_table name
	(* symbol already exists *)
	then pre_error (Printf.sprintf "redefined symbol \"%s\", firstly defined at %s" fun_name (pos_of fun_name))
	(* add the symbol to the hashtable *)
	else CanonHashtbl.add canon_table name canon_def_sym

(* --- end canonical functions --- *)


(* --- display functions --- *)

(** Used to print a position
	Debug only
	@param e	Element of which we want to display the position
*)
let print_pos e=
	(Printf.fprintf stdout "%s->%s:%d\n" e.ident e.file e.line)


(** Output a constant.
	@param out	Channel to output to.
	@param cst	Constant to output. *)
let output_const out cst =
	match cst with
	  NULL ->
	    output_string out "<null>"
	| CARD_CONST v ->
		output_string out (Int32.to_string v);
		output_string out "L"
	| CARD_CONST_64 v->
		output_string out (Int64.to_string v);
		output_string out "LL"
	| CANON v
	| STRING_CONST v ->
		Printf.fprintf out "\"%s\"" v
	| FIXED_CONST v ->
		Printf.fprintf out "%f" v


(** Print a constant.
	@param cst	Constant to display. *)
let print_const cst = output_const stdout cst


(** Print a type expression.
	@param out	Channel to output to.
	@param t	Type expression to display. *)
let output_type_expr out t =
	match t with
	  NO_TYPE ->
		output_string out "<no type>"
	| BOOL ->
		output_string out "bool"
	| INT s ->
		Printf.fprintf out "int(%d)" s
	| CARD s ->
		Printf.fprintf out "card(%d)" s
	| FIX(s, f) ->
		Printf.fprintf out "fix(%d, %d)" s f
	| FLOAT(s, f) ->
		Printf.fprintf out "float(%d, %d)" s f
	| RANGE(l, u) ->
		Printf.fprintf out "[%s..%s]" (Int32.to_string l) (Int32.to_string u)
	| STRING ->
		output_string out "string"
	| ENUM l->
		output_string out "enum (";
		Printf.fprintf out "%ld" (List.hd (List.rev l));
		List.iter (fun i->(Printf.fprintf out ", %ld" i)) (List.tl (List.rev l));
		output_string out ")"
	| ANY_TYPE -> output_string out "any_type"


(** Print a type expression.
	@param t	Type expression to display. *)
let print_type_expr t = output_type_expr stdout t


(** Print the unary operator.
	@param op	Operator to print. *)
let string_of_unop op =
	match op with
	  NOT		-> "!"
	| BIN_NOT	-> "~"
	| NEG		-> "-"

(** Print a binary operator.
	@param op	Operator to print. *)
let string_of_binop op =
	match op with
	  ADD		-> "+"
	| SUB		-> "-"
	| MUL		-> "*"
	| DIV		-> "/"
	| MOD		-> "%"
	| EXP		-> "**"
	| LSHIFT	-> "<<"
	| RSHIFT	-> ">>"
	| LROTATE	-> "<<<"
	| RROTATE	-> ">>>"
	| LT		-> "<"
	| GT		-> ">"
	| LE		-> "<="
	| GE		-> ">="
	| EQ		-> "=="
	| NE		-> "!="
	| AND		-> "&&"
	| OR		-> "||"
	| BIN_AND	-> "&"
	| BIN_OR	-> "|"
	| BIN_XOR	-> "^"
	| CONCAT	-> "::"


(** Print an expression.
	@param out	Channel to output to.
	@param expr	Expression to print. *)
let rec output_expr out e =

	let print_arg fst arg =
		if not fst then output_string out ", ";
		output_expr out arg;
		false in
	match e with
	  NONE ->
	  	output_string out "<none>"
	| COERCE (t, e) ->
		output_string out "coerce(";
		output_type_expr out t;
		output_string out ", ";
		output_expr out e;
		output_string out ")"
	| FORMAT (fmt, args) ->
		output_string out "format(\"";
		output_string out fmt;
		output_string out "\", ";
		let _ = List.fold_left print_arg true args in
		output_string out ")"
	| CANON_EXPR (_, n, args) ->
		output_string out "\"";
		output_string out n;
		output_string out "\" (";
		let _ = List.fold_left print_arg true args in
		output_string out ")"
	| FIELDOF(t, e, n) ->
		output_string out e;
		output_string out ".";
		output_string out n
	| REF (t, id) ->
		Printf.fprintf out "%s: " id;
		output_type_expr out t
	| ITEMOF (t, name, idx) ->
		Printf.fprintf out "%s: " name;
		output_type_expr out t;
		output_string out " [";
		output_expr out idx;
		output_string out "]";
	| BITFIELD (t, e, l, u) ->
		output_expr out e;
		output_string out "<";
		output_expr out l;
		output_string out "..";
		output_expr out u;
		output_string out ">";
		output_string out "(: ";
		output_type_expr out t;
		output_string out ")"
	| UNOP (_,op, e) ->
		output_string out (string_of_unop op);
		output_expr out e
	| BINOP (_,op, e1, e2) ->
		output_string out "(";
		output_expr out e1;
		output_string out ")";
		output_string out (string_of_binop op);
		output_string out "(";
		output_expr out e2;
		output_string out ")"
	| IF_EXPR (tt,c, t, e) ->
		output_string out "(:";
		output_type_expr out tt;
		output_string out ")if ";
		output_expr out c;
		output_string out " then ";
		output_expr out t;
		output_string out " else ";
		output_expr out e;
		output_string out " endif"
	| SWITCH_EXPR (tt,c, cases, def) ->
		output_string out "(:";
		output_type_expr out tt;
		output_string out ")switch(";
		output_expr out c;
		output_string out ")";
		output_string out "{ ";
		List.iter (fun (c, e) ->
				output_string out "case ";
				output_expr out c;
				output_string out ": ";
				output_expr out e;
				output_string out " "
			) (List.rev cases);
		output_string out "default: ";
		output_expr out def;
		output_string out " }"
	| ELINE (file, line, e) ->
		if !dump_lines then Printf.fprintf out "(%s:%d: " file line;
		output_expr out e;
		if !dump_lines then output_string out ")"
	| CONST (t, c) ->
		if !dump_type then
			(output_string out "const(";
			output_type_expr out t;
			output_string out ", ");
		output_const out c;
		if !dump_type then output_string out ")"
	| CAST (typ, expr) ->
		output_string out "cast<";
		output_type_expr out typ;
		output_string out ">(";
		output_expr out expr;
		output_string out ")"


(** Print an expression.
	@param expr	Expression to print. *)
let rec print_expr e = output_expr stdout e


(** Print a location.
	@param out	Channel to output to.
	@param loc	Location to print. *)
let rec output_location out loc =
	match loc with
	| LOC_NONE ->
		output_string out "<none>"
	| LOC_REF (t, id, idx, lo, up) ->
	  	output_string out id;
		if idx <> NONE then
			begin
				output_string out "[";
				output_expr out idx;
				output_string out "]"
			end;
		if lo <> NONE then
			begin
				output_string out "<";
				output_expr out lo;
				output_string out "..";
				output_expr out up;
				output_string out ">"
			end;
		output_string out ": ";
		output_type_expr out t
	| LOC_CONCAT (_, l1, l2) ->
		output_location out l1;
		output_string out "::";
		output_location out l2


(** Print a location.
	@param loc	Location to print. *)
let rec print_location loc = output_location stdout loc


(** Print a statement
	@param out	Channel to output to.
	@param stat	Statement to print.*)
let rec output_statement out stat =
	match stat with
	  NOP ->
	  	output_string out "\t\t <NOP>;\n"
	| SEQ (stat1, stat2) ->
		output_statement out stat1;
		output_statement out stat2
	| EVAL (ch1, ch2) ->
		if ch1 <> "" then Printf.fprintf out "\t\t%s.%s;\n" ch1 ch2 else Printf.fprintf out "\t\t%s;\n" ch2
	| SET (loc, exp) ->
		output_string out "\t\t";
		output_location out loc;
		output_string out "=";
		output_expr out exp;
		output_string out ";\n"
	| CANON_STAT (ch, expr_liste) ->
		Printf.fprintf out "\t\t\"%s\" (" ch;
		ignore (List.fold_left
			(fun f e ->
				if not f then output_string out ", ";
				output_expr out e; false)
			true
			expr_liste);
		output_string out ");\n"
	| ERROR ch ->
		Printf.fprintf out "\t\t error %s;\n" ch
	| IF_STAT (exp,statT,statE) ->
		output_string out "\t\t if ";
		output_expr out exp;
		output_string out "\n";
		output_string out "\t\t then \n";
		output_statement out statT;
		output_string out "\t\t else \n";
		output_statement out statE;
		output_string out "\t\t endif;\n"
	| SWITCH_STAT (exp,stat_liste,stat) ->
		output_string out "\t\t switch (";
		output_expr out exp;
		output_string out ") {\n";
		List.iter (fun (v,s)->
			output_string out "\t\t\t case";
			output_expr out v;
			output_string out " : \n\t\t";
			output_statement out s) (List.rev stat_liste);
		output_string out "\t\t\t default : \n\t\t";
		output_statement out stat;
		output_string out "\t\t }; \n"
	| LINE (file, line, s) ->
		output_statement out s
	| LOCAL (v, o, t) ->
		Printf.fprintf out "\t\tlet %s = " o;
		output_type_expr out t;
		output_string out " \n"
	| FOR(v, _, t, l, u, b) ->
		Printf.fprintf out "\t\tfor %s: " v;
		output_type_expr out t;
		output_string out " in ";
		output_const out l;
		output_string out "..";
		output_const out u;
		output_string out " do\n";
		output_statement out b;
		output_string out "\t\tenddo;\n"


(** Print a statement
	@param stat	Statement to print.*)
let rec print_statement stat= output_statement stdout stat


(** Print a type.
	@param typ	Type to print. *)
let output_type out typ =
	match typ with
	  TYPE_ID id -> output_string out id
	| TYPE_EXPR te -> output_type_expr out te

(** Print a type.
	@param typ	Type to print. *)
let print_type typ =
	output_type stdout typ

(** Print an attribute.
	@param attr	Attribute to print. *)
let output_attr out attr =
	match attr with
	| ATTR_EXPR (id, expr) ->
	  	Printf.fprintf out "\t%s = " id;
		output_expr out expr;
		output_char out '\n'

	| ATTR_STAT (id, stat) -> Printf.printf "\t%s = {\n" id ;
				  output_statement out stat;
				  Printf.fprintf out "\t}\n";
		()
	| ATTR_USES ->
		()
	| ATTR_LOC (id, l) ->
		Printf.fprintf out "\t%s = " id;
		output_location out l;
		output_char out '\n'


(** Print an attribute.
	@param attr	Attribute to print. *)
let print_attr attr =
	output_attr stdout attr

(** Print a memory attibute.
	@param attr	Memory attribute to print. *)
let output_mem_attr out attr =
	(*let rec print_call id args =
		print_string id;
		if args <> [] then
			begin
				ignore(List.fold_left (fun sep arg ->
						output_string out sep;
						print_arg arg;
						", "
					) "(" args);
				print_string ")"
			end
	and print_arg arg =
		match arg with
		| ATTR_ID (id, args) -> print_call id args
		| ATTR_VAL cst -> output_const out cst in*)

	match attr with
	| ATTR_EXPR("volatile", CONST(_, CARD_CONST n)) ->
		Printf.fprintf out "volatile(%d)" (Int32.to_int n)
	| ATTR_LOC("alias", l) ->
		output_string out "alias "; output_location out l
	| ATTR_EXPR("init", CONST(_, v)) ->
		output_string out "init = ";
		output_const out v
	| _ ->
		output_attr out attr

(** Print a memory attibute.
	@param attr	Memory attribute to print. *)
let print_mem_attr attr =
	output_mem_attr stdout attr

(** Print a list of memory attributes.
	@param attrs	List of attributes. *)
let output_mem_attrs out attrs =
	List.iter (fun attr -> output_string out " "; output_mem_attr out attr) attrs

(** Print a list of memory attributes.
	@param attrs	List of attributes. *)
let print_mem_attrs attrs =
	output_mem_attrs stdout attrs

(** Print a specification item.
	@param out	Stream to output to.
	@param spec	Specification item to print. *)
let output_spec out spec =
	let print_newline _ = output_char out '\n' in
	let print_string = output_string out in
	match spec with
	| LET (name, _, cst) ->
	  	Printf.fprintf out "let %s = " name;
		output_const out cst;
		print_newline ()
	| TYPE (name, t) ->
		Printf.fprintf out "type %s = " name;
		output_type_expr out t;
		print_newline ()
	| MEM (name, size, typ, attrs) ->
		Printf.fprintf out "mem %s [%d, " name size;
		output_type_expr out typ;
		print_string "]";
		output_mem_attrs out attrs;
		print_newline ()
	| REG (name, size, typ, attrs) ->
		Printf.fprintf out "reg %s [%d, " name size;
		output_type_expr out typ;
		print_string "]";
		output_mem_attrs out attrs;
		print_newline ()
	| VAR (name, size, typ, attrs) ->
		Printf.fprintf out "var %s [%d, " name size;
		output_type_expr out typ;
		print_string "]";
		output_mem_attrs out attrs;
		print_newline ()
	| RES name ->
		Printf.fprintf out "resource %s\n" name
	| EXN name ->
		Printf.fprintf out "exception %s\n" name
	| AND_MODE (name, pars, res, attrs) ->
		print_string "mode ";
		print_string name;
		print_string " (";
		let _ = List.fold_left
			(fun fst (id, typ) ->
				if not fst then print_string ", ";
				print_string id;
				print_string ": ";
				output_type out typ;
				false
			)
			true pars in
		print_string ")";
		if res <> NONE then begin
			print_string " = ";
			output_expr out res
		end;
		print_string "\n";
		List.iter (output_attr out) (List.rev attrs) ;
		print_newline ();
	| OR_MODE (name, modes) -> Printf.printf "mode %s = " name ;
				   List.iter (fun a -> Printf.fprintf out " %s | " a) (List.rev (List.tl modes)) ;
				   Printf.fprintf out "%s" (List.hd (modes));
				   Printf.fprintf out "\n";
		()
	| AND_OP (name, pars, attrs) -> Printf.fprintf out "op %s (" name ;
					if (List.length pars)>0
					then begin
						List.iter (fun a -> begin 	Printf.fprintf out "%s : " (fst a) ;
										output_type out (snd a);
										Printf.fprintf out ", ";
								   end) (List.rev (List.tl pars));
						Printf.fprintf out "%s : " (fst (List.hd pars));
						output_type out (snd (List.hd pars));
					end;
					Printf.fprintf out ")\n";
					List.iter (output_attr out) (List.rev attrs) ;
		()
	| OR_OP (name, modes) -> Printf.fprintf out "op %s = " name ;
				 List.iter (fun a -> Printf.fprintf out " %s | " a) (List.rev (List.tl modes));
				 Printf.fprintf out "%s" (List.hd modes);
				 Printf.fprintf out "\n";
		()

	| PARAM (name,t)->
		Printf.fprintf out "param %s (" name;
		output_type out t;
		output_string out ")\n";

	| ATTR (a) -> print_attr a;
		()

	| CANON_DEF(name, kind, type_res, type_prms_list) ->
		Printf.fprintf out "canon ";
		if kind = CANON_FUNC then
			(output_type_expr out type_res;
			Printf.fprintf out " \"%s\"(" name;
			ignore (List.fold_left
				(fun f e ->
					if not f then output_string out ", ";
					output_type_expr out e; false)
				true
				type_prms_list);
			output_string out ")\n")
		else
			Printf.fprintf out "\"%s\"\t: " name;
			output_type_expr out type_res;
			output_string out "\n"


	| UNDEF ->
		output_string out "<UNDEF>"

(** Print a specification item.
	@param spec	Specification item to print. *)
let print_spec spec =
	output_spec stdout spec


(**
    return the gliss_isize defined in nmp sources
*)
let get_isize _ =
	match get_symbol "gliss_isize" with
	(* if gliss_isize not defined, we assume we have a cisc isa *)
	| UNDEF -> []
	| LET(st, _, cst) ->
		(match cst with
		STRING_CONST(nums) ->
			List.map
			(fun x ->
				try
				int_of_string x
				with
				Failure "int_of_string" ->
					failwith "gliss_isize must be a string containing integers seperated by commas."
			)
			(Str.split (Str.regexp ",") nums)
		| _ ->
			failwith "gliss_isize must be defined as a string constant (let gliss_size = \"...\")"
		)
	| _ ->
		failwith "gliss_isize must be defined as a string constant (let gliss_size = \"...\")"

(** Get the type of a parameter.
	@param p	Parameter.
	@return		Parameter type. *)
let get_type_param p =
	match p with
	(str, TYPE_ID(n)) ->
		n
	| _ ->
		"[err741]"

(** Get the parameter name.
	@param p	Parameter.
	@return		Parameter name. *)
let get_name_param p =
	match p with
	(str, _) ->
		str

(** Display a parameter.
	@param p	Parameter to display. *)
let print_param p =
	match p with
	(str, TYPE_ID(n)) ->
		Printf.printf " (%s:%s) " (get_name_param p) (get_type_param p)
	| (str, TYPE_EXPR(t)) ->
		begin
		Printf.printf " (%s:" (get_name_param p);
		print_type (TYPE_EXPR(t));
		print_string ") "
		end


(** Display a parameter list.
	@param l	List of parameters. *)
let rec print_param_list l =
begin
	match l with
	[] ->
		print_string "\n"
	| h::q ->
		begin
		print_param h;
		print_param_list q
		end
end


(** True if the current system is Windwos *)
let is_windows = Sys.os_type = "Win32"


(** If needed, convert a MingW path to native path.
	@param path		MingW path to convert.
	@return			Path converted to native path. *)
let native_path path =
	let starts_with s1 s2 =
		if (String.length s1) < (String.length s2) then false
		else (String.sub s1 0 (String.length s2)) = s2 in

	let head s n = String.sub s 0 n in
	let tail s n = String.sub s n ((String.length s) - n) in

	let rec replace s =
		if s = "" then "" else
		(if starts_with s "/" then "\\" else head s 1)
		^ (replace (tail s 1)) in

	if not is_windows then path else
	let path =
		if starts_with path "/c/" then "C:\\" ^ (tail path 3) else
		path in
	replace path


(**	Save the current IRG definition to a file.
	@param path			Path of the file to save to.
	@raise	Sys_error	If there is an error during the write. *)
let save path =
	let out = open_out_bin path in
	Marshal.to_channel out (syms, pos_table) []


(** Load an NML description either NMP, NML or IRG.
	@param 	path		Path of the file to read from.
	@raise	Sys_error	If there is an error during the read. *)
let load path =
	let input = open_in_bin path in
	let (new_syms, new_pt) =
		(Marshal.from_channel input :  spec StringHashtbl.t * pos_type StringHashtbl.t) in
	StringHashtbl.clear syms;
	StringHashtbl.clear pos_table;
	StringHashtbl.iter (fun key spec -> StringHashtbl.add syms key spec) new_syms;
	StringHashtbl.iter (fun key pos -> StringHashtbl.add pos_table key pos) new_pt


(** Test if an attribute is defined.
	@param id		Identifier of the attribute.
	@param attrs	List of attributes.
	@return			True if the attribute is found, false else. *)
let rec attr_defined id attrs =
	match attrs with
	| [] -> false
	| (ATTR_EXPR (id', _))::_ when id = id' -> true
	| (ATTR_STAT (id', _))::_ when id = id' -> true
	| (ATTR_LOC (id', _))::_ when id = id' -> true
	| _::tl -> attr_defined id tl


(** Get an attribute as an expression.
	@param id			Identifier of the looked attribute.
	@param attrs		List of attributes.
	@param def			Default value if the attribute is not found.
	@return				Found attribute value or the default.
	@raise PreError		If the attribute exists but does not have the right type. *)
let rec attr_expr id attrs def =
	let error _ =
		pre_error (Printf.sprintf "attribute \"%s\" should be an expression" id) in
	match attrs with
	| [] -> def
	| (ATTR_EXPR (id', e))::_ when id = id' -> e
	| (ATTR_STAT (id', _))::_ when id = id' -> error ()
	| (ATTR_LOC (id', _))::_ when id = id' -> error ()
	| _::tl -> attr_expr id tl def


(** Get an attribute as a statement.
	@param id			Identifier of the looked attribute.
	@param attrs		List of attributes.
	@param def			Default value if the attribute is not found.
	@return				Found attribute value or the default.
	@raise PreError		If the attribute exists but does not have the right type. *)
let rec attr_stat id attrs def =
	let error _ =
		pre_error (Printf.sprintf "attribute \"%s\" should be a statement" id) in
	match attrs with
	| [] -> def
	| (ATTR_EXPR (id', _))::_ when id = id' -> error ()
	| (ATTR_STAT (id', s))::_ when id = id' -> s
	| (ATTR_LOC (id', _))::_ when id = id' -> error ()
	| _::tl -> attr_stat id tl def


(** Get an attribute as a location.œ.
	@param id		Identifier of the looked attribute.
	@param attrs	List of attributes.
	@param def		Default value if the attribute is not found.
	@return			Found attribute value or the default.
	@raise Error	If the attribute exists but does not have the right type. *)
let rec attr_loc id attrs def =
	let error _ =
		pre_error (Printf.sprintf "attribute \"%s\" should be a location" id) in
	match attrs with
	| [] -> def
	| (ATTR_LOC (id', e))::_ when id = id' -> e
	| (ATTR_STAT (id', _))::_ when id = id' -> error ()
	| (ATTR_EXPR (id', _))::_ when id = id' -> error ()
	| _::tl -> attr_loc id tl def


(** Get an attribute as a statement.
	@param id		Identifier of the looked attribute.
	@param attrs	List of attributes.
	@param def		Default value if the attribute is not found.
	@return			Found attribute value or the default.
	@raise Error	If the attribute exists but does not have the right type. *)
let rec attr_stat id attrs def =
	let error _ =
		pre_error (Printf.sprintf "attribute \"%s\" should be a statement" id) in
	match attrs with
	| [] -> def
	| (ATTR_STAT (id', s))::_ when id = id' -> s
	| (ATTR_EXPR (id', _))::_ when id = id' -> error ()
	| (ATTR_LOC (id', _))::_ when id = id' -> error ()
	| _::tl -> attr_stat id tl def


(** Get the name of an attribute.
		@param attr Attribute to get name from.
		@return     Attribute name or empty string for USES. *)
let attr_name attr =
	match attr with
	| ATTR_EXPR (n, _)
	| ATTR_STAT (n, _)
	| ATTR_LOC (n, _) -> n
	| _ -> ""


(** Change the value of an attribute in the attribute list.
		@param value Attribute Value.
		@param attrs Attribute list to change.
		@return      Changed attribute list. *)
let set_attr value attrs =
	let id = attr_name value in
	let rec process res attrs =
		match attrs with
		| [] -> res
		| h::t when (attr_name h) = id -> process res t
		| h::t -> process (h::res) t in
	 value :: (process [] attrs)


(** Set the value of an expression attribute in the main symbol table.
	@param id	Attribute identifier.
	@param e	Attribute expression value. *)
let set_expr_attr id e =
	rm_symbol id;
	add_symbol id (ATTR (ATTR_EXPR (id, e)))


(** Set the value of a statement attribute in the main symbol table.
	@param id	Attribute identifier.
	@param e	Attribute statement value. *)
let set_stat_attr id s =
	rm_symbol id;
	add_symbol id (ATTR (ATTR_STAT (id, s)))


(** Apply the given functions to all specifications.
	@param f	Function to apply. *)
let iter f =
	StringHashtbl.iter f syms


(** Apply the given function to all specification and with the given data.
	@param f	Function to apply.
	@param d	Initial data.
	@return 	Result data *)

let fold f d =
	StringHashtbl.fold f syms d


(** Get the root node of the ISA.
	@return			root name
	@raise Error	If no root can be found. *)
let get_root _ =
	if is_defined "multi" then
		"multi"
	else if is_defined "instruction" then
		"instruction"
	else
		raise (Error (fun out -> Printf.fprintf out "one of root node, \"instruction\" or \"multi\" must be defined !"))


(* regex for format decoding *)
let format_regex = Str.regexp (
	  "%[0-9]*\\(h\\|l\\|hh\\|ll\\)?[dioxXub]\\|"	(* integer formats *)
	^ "%[0-9]*\\(\\.[0-9]+\\)?L?[aAeEfFgG]\\|"		(* float formats *)
	^ "%[0-9]*s\\|"									(* string format *)
	^ "%[0-9]*b\\|"									(* binary format *)
	^ "%@\\|"										(* label format *)
	^ "%l\\|"										(* label format *)
	^ "%%")											(* double percent *)


(**	Cut a format expression string into format specifiers and simple text,
	format specifiers are similar to those in C language,
	syntax: %[width specifier]b|d|x|f | %s, %% is for printing '%' character
	@param	l	the Str.split_result list to transform
	@return		a list of delimiters (format specifiers) and text
*)
let split_format_string s =
	let process_double_percent e =
		match e with
		| Str.Delim("%%") -> Str.Text("%%")
		| Str.Delim("%l") -> Str.Delim("%@")
		| _ -> e in
	List.map process_double_percent (Str.full_split format_regex s)


(** Test if the given format is an integer format.
	@param s	Format string s to test (must start with '%' and be at least two characters long).
	@return		True if it is an integer format, false else. *)
let is_int_format s =
	List.mem (s.[(String.length s) - 1]) ['d'; 'i'; 'o'; 'x'; 'X'; 'u']


(** Test if the given format is a float format.
	@param s	Format string s to test (must start with '%' and be at least two characters long).
	@return		True if it is a float format, false else. *)
let is_float_format s =
	List.mem (s.[(String.length s) - 1]) ['a'; 'A'; 'e'; 'E'; 'f'; 'F'; 'g'; 'G']

(** Test if the given format is a text format.
	@param s	Format string s to test (must start with '%' and be at least two characters long).
	@return		True if it is a text format, false else. *)
let is_text_format s =
	(s.[(String.length s) - 1]) = 's'

(** Test if the given format is a binary format.
	@param s	Format string s to test (must start with '%' and be at least two characters long).
	@return		True if it is a binary format, false else. *)
let is_bin_format s =
	(s.[(String.length s) - 1]) = 'b'


(** Type to perform mixed printing of IRG information. *)
type printable =
	| PTEXT of string
	| PSTAT of stat
	| PEXPR of expr
	| PLOC of location
	| PTYPE of type_expr
	| PFUN of (out_channel -> unit)
	| PSPEC of spec
	| PINT of int
	| PINT32 of Int32.t
	| PINT64 of Int64.t
	| PCST of const
	| PLN


(** Print a message made of IRG items.
	@param out Output channel.
	@param lst List of items to print. *)
let output lst out =
	let output_item item =
		match item with
		| PTEXT t 	-> output_string out t
		| PSTAT s 	-> output_statement out s
		| PEXPR e	-> output_expr out e
		| PLOC  l 	-> output_location out l
		| PTYPE t 	-> output_type_expr out t
		| PFUN f	-> f out
		| PSPEC s	-> output_spec out s
		| PINT i	-> output_string out (string_of_int i)
		| PINT32 i	-> output_string out (Int32.to_string i)
		| PINT64 i	-> output_string out (Int64.to_string i)
		| PLN 		-> output_char out '\n'
		| PCST c	-> output_const out c in
	List.iter output_item lst


(** Print a message made of IRG items.
	@param lst List of items to print. *)
let print lst = output lst stdout


(** Print an error message made of IRG items.
	@param lst List of items to print. *)
let prerr lst = output lst stderr


(** Print a message made of IRG items and output a new line.
	@param out Output channel.
	@param lst List of items to print. *)
let outputln lst out =
	output lst out;
	output_string out "\n"


(** Print a message made of IRG items and output a new line.
	@param lst List of items to print. *)
let println lst = outputln lst stdout


(** Print an error message made of IRG items and output a new line.
	@param lst List of items to print. *)
let prerrln lst = outputln lst stderr


(** Get attributes of the given specification.
	@param spec		Spec to look in.
	@return			Attribute list (or empty list). *)
let attrs_of spec =
	match spec with
	| MEM (_, _, _, attrs)
	| REG (_, _, _, attrs)
	| AND_MODE (_, _, _, attrs)
	| AND_OP (_, _, attrs)		-> attrs
	| _ 						-> []


(** Get statement attribute from the main symbol table.
	@param id		Attribute identifier.
	@param def		Default value.
	@return			Found statement value or default value.
	@raise PreError	If symbol does not match. *)
let get_stat_attr id def =
	match get_symbol id with
	| UNDEF -> def
	| ATTR (ATTR_STAT (_, s)) -> s
	| _ -> error (fun out -> Printf.fprintf out "%s should a statement attribute" id)


(** Get an expression attribute from the main symbol table.
	@param id		Attribute identifier.
	@param def		Default value.
	@return			Found statement value or default value.
	@raise PreError	If symbol does not match. *)
let get_expr_attr id def =
	match get_symbol id with
	| UNDEF -> def
	| ATTR (ATTR_EXPR (_, e)) -> e
	| _ -> error (fun out -> Printf.fprintf out "%s should an expression attribute" id)


(** Raise an error with message displayed by prerrln.
	@param lst		List of arguments to display.
	@raise Error 	Ever.*)
let error_with_msg lst =
	raise (Error (fun out -> output lst out))


(** Get expression with line information removed.
	@param expr		Expression to examine.
	@return			Expression without line information. *)
let rec escape_eline expr =
	match expr with
	| ELINE (_, _, expr) -> escape_eline expr
	| _ -> expr


(** No source line information .*)
let no_line = ("", 0)

type line =
	| LEXPR of expr
	| LSTAT of stat


(** Find the closer line source information.
	@param expr		Expression to look in.
	@return			(source file, source line) or ("", 0). *)
let rec line_from_expr expr =
	match expr with
	| NONE
	| REF _
	| FIELDOF _
	| CONST _
		-> no_line
	| COERCE (_, e)
	| ITEMOF (_, _, e)
	| UNOP (_, _, e)
	| CAST (_, e)
		-> line_from_expr e
	| FORMAT (_, args)
	| CANON_EXPR (_, _, args)
		-> line_from_list (List.map (fun e -> LEXPR e) args)
	| IF_EXPR (_, e1, e2, e3)
	| BITFIELD (_, e1, e2, e3)
		-> line_from_list [LEXPR e1; LEXPR e2; LEXPR e3]
	| BINOP (t, _, e1, e2)
		-> line_from_list [LEXPR e1; LEXPR e2]
	| SWITCH_EXPR (_, c, cs, d)
		-> line_from_list ([LEXPR c; LEXPR d] @ (List.flatten (List.map (fun (c, e) -> [LEXPR c; LEXPR e]) cs)))
	| ELINE (f, l, _)
		-> (f, l)		


(** Find the closer line source information.
	@param stat		Statement to look in.
	@return			(source file, source line) or ("", 0). *)
and line_from_stat stat =
	match stat with
	| NOP
	| EVAL _
	| ERROR _
	| LOCAL _					-> no_line
	| SEQ (s1, s2) 				-> line_from_list [LSTAT s1; LSTAT s2]
	| SET (l, e) 				-> line_from_expr e
	| CANON_STAT (_, args) 		-> line_from_list (List.map (fun a -> LEXPR a) args)
	| IF_STAT (c, s1, s2) 		-> line_from_list [LEXPR c; LSTAT s1; LSTAT s2]
	| SWITCH_STAT (c, cs, d)	-> line_from_list ([LEXPR c; LSTAT d] @ (List.map (fun (_, s) -> LSTAT s) cs))
	| LINE (f, l, _) 			-> (f, l)
	| FOR(_, _, _, _, _, b)		-> line_from_stat b

and line_from_list lst =
	match lst with
	| [] -> no_line
	| (LEXPR e)::t ->
		let l = line_from_expr e in
		if l <> no_line then l else line_from_list t
	| (LSTAT s)::t ->
		let l = line_from_stat s in
		if l <> no_line then l else line_from_list t


(** Raise an error relative to a statement.
	@param stat		Errored statement.
	@param fn 		Function to display error. *)
let stat_error stat fn =
	let (f, l) = line_from_stat stat in
	complete_error fn f l


(** Raise an error relative to an expression.
	@param expr		Errored Expression.
	@param fn 		Function to display error. *)
let expr_error expr fn =
	let (f, l) = line_from_expr expr in
	complete_error fn f l


(** Test if the given ID design a register.
	@param id	Identifier.
	@return		True if it is a register, false else. *)
let is_reg id =
	match get_symbol id with
	| REG _ -> true
	| _ -> false




(************ Local Variable Manegement ***********)

(** List of defined locale variables. *)
let local_list: string list ref = ref []


(** Helper function to handle let instruction: the given variable is
	added to the symbol table. At the end of evaluation of action,	
	@param v	Name of the variable to add.
	@param t	Type of the variable to add.
	@return		Result of f.;*)
let handle_local (v: string) (t: type_expr) =
	local_list := v :: !local_list;
	add_symbol v (VAR (v, 1, t, []))


(** Clean local variable from the symbol table. *)
let clean_local _ =
	List.iter rm_symbol !local_list;
	local_list := []



