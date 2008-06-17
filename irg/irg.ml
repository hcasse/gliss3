(*
 * $Id: irg.ml,v 1.1 2008/06/17 08:08:29 casse Exp $
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

exception RedefinedSymbol of string

(** Type expression *)
type type_expr =
	  NO_TYPE
	| BOOL
	| INT of int
	| CARD of int
	| FIX of int * int
	| FLOAT of int * int
	| RANGE of int32 * int32


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

type const =
	  NULL
	| CARD_CONST of Int32.t
	| STRING_CONST of string
	| FIXED_CONST of float

type expr =
	  NONE
	| COERCE of type_expr * expr
	| FORMAT of string * expr list
	| CANON_EXPR of string * expr list
	| REF of string
	| FIELDOF of expr * string
	| ITEMOF of expr * expr
	| BITFIELD of expr * expr * expr
	| UNOP of unop * expr
	| BINOP of binop * expr * expr
	| IF_EXPR of expr * expr * expr
	| SWITCH_EXPR of expr * (const * expr) list * expr
	| CONST of const
	

(** Statements *)
type location =
	  LOC_REF of string
	| LOC_ITEMOF of location * expr
	| LOC_BITFIELD of location * expr * expr
	| LOC_CONCAT of location * location

type mem_attr =
	  VOLATILE of int
	| PORTS of int * int
	| ALIAS of location
	| INIT of const
	| USES (*of uses*)

type stat =
	  NOP
	| SEQ of stat * stat
	| EVAL of string
	| EVALIND of string * string
	| SET of location * expr
	| CANON_STAT of string * expr list
	| ERROR of string
	| IF_STAT of expr * stat * stat
	| SWITCH_STAT of expr * (int * stat) list * stat


(** attribute specifications *)
type attr =
	  ATTR_EXPR of string * expr
	| ATTR_STAT of string * stat
	| ATTR_USES


(** Specification of an item *)
type spec =
	  UNDEF
	| LET of string * const
	| TYPE of string * type_expr
	| MEM of string * int * type_expr * mem_attr list
	| REG of string * int * type_expr * mem_attr list
	| VAR of string * int * type_expr
	| AND_MODE of string * (string * typ) list * expr * attr list
	| OR_MODE of string * string list
	| AND_OP of string * (string * typ) list * attr list
	| OR_OP of string * string list
	| RES of string
	| EXN of string


(* Symbol table *)
module HashString =
struct
	type t = string
	let equal (s1 : t) (s2 : t) = s1 = s2
	let hash (s : t) = Hashtbl.hash s
end
module StringHashtbl = Hashtbl.Make(HashString)
let syms : spec StringHashtbl.t = StringHashtbl.create 211


(** Get the symbol matching the given name or UNDEF if not found.
 *)
let get_symbol n =
	try
		StringHashtbl.find syms n
	with Not_found ->
		UNDEF


(** Add a symbol the namespace.
	@param name	Name of the symbol to add.
	@param sym	Symbol to add.
	@raise RedefinedSymbol	If the symbol is already defined. *)
let add_symbol name sym = 
	if StringHashtbl.mem syms name
	then raise (RedefinedSymbol name)
	else StringHashtbl.add syms name sym



(** Print a constant.
	@param cst	Constant to display. *)
let print_const cst =
	match cst with
	  NULL ->
	    print_string "<null>"
	| CARD_CONST v ->
		print_string (Int32.to_string v)
	| STRING_CONST v ->
		Printf.printf "\"%s\"" v   
	| FIXED_CONST v ->
		print_float v


(** Print a type expression.
	@param t	Type expressio to display. *)
let print_type_expr t =
	match t with
	  NO_TYPE ->
		print_string "<no type>"
	| BOOL ->
		print_string "bool"
	| INT s ->
		Printf.printf "int(%d)" s
	| CARD s ->
		Printf.printf "card(%d)" s
	| FIX(s, f) ->
		Printf.printf "fix(%d, %d)" s f
	| FLOAT(s, f) ->
		Printf.printf "float(%d, %d)" s f
	| RANGE(l, u) ->
		Printf.printf "[%s..%s]" (Int32.to_string l) (Int32.to_string u)


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
	@param expr	Expression to print. *)
let rec print_expr e =

	let print_arg fst arg =
		if not fst then print_string ", ";
		print_expr arg;
		false in

	match e with
	  NONE ->
	  	print_string "<none>"
	| COERCE (t, e) ->
		print_string "coerce(";
		print_type_expr t;
		print_string ", ";
		print_expr e;
		print_string ")"
	| FORMAT (fmt, args) ->
		print_string "format(\"";
		print_string fmt;
		print_string "\", ";
		let _ = List.fold_left print_arg true args in
		print_string ")"
	| CANON_EXPR (n, args) ->
		print_string "\"";
		print_string n;
		print_string "\"(";
		let _ = List.fold_left print_arg true args in
		print_string ")"
	| FIELDOF( e, n) ->
		print_expr e;
		print_string ".";
		print_string n
	| REF (id) ->
		print_string id
	| ITEMOF (e, idx) ->
		print_expr e;
		print_string "[";
		print_expr idx;
		print_string "]"
	| BITFIELD (e, l, u) ->
		print_expr e;
		print_string "[";
		print_expr l;
		print_string "..";
		print_expr u;
		print_string "]"
	| UNOP (op, e) ->
		print_string (string_of_unop op);
		print_expr e
	| BINOP (op, e1, e2) ->
		print_string "(";
		print_expr e1;
		print_string ")";
		print_string (string_of_binop op);
		print_string "(";
		print_expr e2;
		print_string ")"
	| IF_EXPR (c, t, e) ->
		print_string "if ";
		print_expr c;
		print_string " then ";
		print_expr t;
		print_string " else ";
		print_expr e;
		print_string " endif"
	| SWITCH_EXPR (c, cases, def) ->
		print_string "switch(";
		print_expr c;
		print_string ")";
		List.iter (fun (c, e) ->
				print_string "case ";
				print_const c;
				print_string ": ";
				print_expr e
			) cases;
		print_string "default: ";
		print_expr def
	| CONST c ->
		print_const c


(** Print a location.
	@param loc	Location to print. *)
let rec print_location loc =
	match loc with
	  LOC_REF id ->
	  	print_string id
	| LOC_ITEMOF (e, idx) ->
		print_location e;
		print_string "[";
		print_expr idx;
		print_string "]"
	| LOC_BITFIELD (e, l, u) ->
		print_location e;
		print_string "[";
		print_expr l;
		print_string "..";
		print_expr u;
		print_string "]"
	| LOC_CONCAT (l1, l2) ->
		print_location l1;
		print_string "..";
		print_location l2


(** Print a memory attibute.
	@param attr	Memory attribute to print. *)
let print_mem_attr attr =
	match attr with
	  VOLATILE n ->
		Printf.printf "volatile(%d)" n
	| PORTS (l, u) ->
		Printf.printf "ports(%d, %d)" l u
	| ALIAS l ->
		print_string "alias "; print_location l
	| INIT v ->
		print_string "init = ";
		print_const v
	| USES ->
		print_string "uses"


(** Print a list of memory attributes.
	@param attrs	List of attributes. *)
let print_mem_attrs attrs =
	List.iter (fun attr -> print_string " "; print_mem_attr attr) attrs


(** Print a type.
	@param typ	Type to print. *)
let print_type typ =
	match typ with
	  TYPE_ID id -> print_string id
	| TYPE_EXPR te -> print_type_expr te


(** Print an attribute.
	@param attr	Attribute to print. *)
let print_attr attr =
	match attr with
	  ATTR_EXPR (id, expr) ->
	  	Printf.printf "\t%s = " id;
		print_expr expr;
		print_newline ()
	| ATTR_STAT (id, stat) ->
		() 
	| ATTR_USES ->
		()



(** Print a specification item.
	@param spec	Specification item to print. *)
let print_spec spec =
	match spec with
	  LET (name, cst) ->
	  	Printf.printf "let %s = " name;
		print_const cst;
		print_newline ()
	| TYPE (name, t) ->
		Printf.printf "type %s = " name;
		print_type_expr t;
		print_newline ()
	| MEM (name, size, typ, attrs) ->
		Printf.printf "mem %s [%d, " name size;
		print_type_expr typ;
		print_string "]";
		print_mem_attrs attrs;
		print_newline ()
	| REG (name, size, typ, attrs) ->
		Printf.printf "reg %s [%d, " name size;
		print_type_expr typ;
		print_string "]";
		print_mem_attrs attrs;
		print_newline ()
	| VAR (name, size, typ) ->
		Printf.printf "var %s [%d, " name size;
		print_type_expr typ;
		print_string "]\n";
	| RES name ->
		Printf.printf "resource %s\n" name
	| EXN name ->
		Printf.printf "exception %s\n" name
	| AND_MODE (name, pars, res, attrs) ->
		print_string "mode ";
		print_string name;
		print_string " (";
		let _ = List.fold_left
			(fun fst (id, typ) ->
				if not fst then print_string ", ";
				print_string id;
				print_string ": ";
				print_type typ;
				false
			)
			true pars in
		print_string ")";
		if res <> NONE then begin
			print_string " = ";
			print_expr res
		end;
		print_newline ();
		List.iter print_attr attrs
	| OR_MODE (name, modes) ->
		()
	| AND_OP (name, pars, attrs) ->
		()
	| OR_OP (name, modes) ->
		()
	| _ ->
		assert false

