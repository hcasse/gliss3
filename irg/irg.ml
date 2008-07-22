(*
 * $Id: irg.ml,v 1.5 2008/07/22 09:49:09 jorquera Exp $
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
 * along with OGliss; if not, write to the Free Software
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
	| STRING
	| ENUM of string list
	| UNKNOW_TYPE		(* Used for OR_MODE only. The evaluation is done in a dynamic way *)

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
	| CARD_CONST_64 of Int64.t
	| STRING_CONST of string
	| FIXED_CONST of float

type expr =
	  NONE
	| COERCE of type_expr * expr 
	| FORMAT of string * expr list 
	| CANON_EXPR of type_expr * string * expr list
	| REF of string
	| FIELDOF of type_expr * expr * string
	| ITEMOF of type_expr * expr * expr
	| BITFIELD of type_expr * expr * expr * expr
	| UNOP of type_expr * unop * expr
	| BINOP of type_expr * binop * expr * expr
	| IF_EXPR of type_expr * expr * expr * expr
	| SWITCH_EXPR of type_expr * expr * (expr * expr) list * expr
	| CONST of type_expr*const
	
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
	| SWITCH_STAT of expr * (expr * stat) list * stat
(* stats rajoutés *)
	|SETSPE of location * expr	(* Used for allowing assigment of parameters (for exemple in predecode). 
					   This is NOT in the nML standard and is only present for allowing 
					   compatibility *) 

(** attribute specifications *)
type attr =
	  ATTR_EXPR of string * expr
	| ATTR_STAT of string * stat
	| ATTR_USES


(** Specification of an item *)
type spec =
	  UNDEF
	| LET of string * const (* typed with construction *)
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
(* spec ajoutées *)
	| PARAM of string * typ
	| ENUM_POSS of string*string*Int32.t*bool



(* cannonical functions *)

(*
let cannon_fun={name : string; param : type_expr list ; type_res:type_expr}

let cannon_tabl = [ {name="sin";param=[INT 32];type_res=FLOAT (24,8)} ]

let call_cannonical name param=
*)



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


(** Add a symbol to the namespace.
	@param name	Name of the symbol to add.
	@param sym	Symbol to add.
	@raise RedefinedSymbol	If the symbol is already defined. *)
let add_symbol name sym = 
	if StringHashtbl.mem syms name
	(* symbol already exists *)
	then raise (RedefinedSymbol name)
	(* add the symbol to the hashtable *)
	else StringHashtbl.add syms name sym

(*
let is_defined name =
	try(
		StringHashtbl.find syms name;
		true
	)with Not_found->false*)

let is_defined name = StringHashtbl.mem syms name

let add_param (name,t) =
	StringHashtbl.add syms name (PARAM (name,t))

let empiler_param l =List.iter add_param l

let depiler_param l =List.iter (StringHashtbl.remove syms) (List.map fst l)

let complete_incomplete_enum_poss id =
	StringHashtbl.fold (fun e v d-> match v with 
				ENUM_POSS (n,_,t,false)-> StringHashtbl.replace syms e (ENUM_POSS (n,id,t,true))
				|_->d 
			) syms ()


(** Print a constant.
	@param cst	Constant to display. *)
let print_const cst =
	match cst with
	  NULL ->
	    print_string "<null>"
	| CARD_CONST v ->
		print_string (Int32.to_string v)
	| CARD_CONST_64 v->
		print_string (Int64.to_string v)
	| STRING_CONST v ->
		Printf.printf "\"%s\"" v   
	| FIXED_CONST v ->
		print_float v


(** Print a type expression.
	@param t	Type expression to display. *)
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
	| STRING ->
		print_string "string"
	| ENUM l->
		print_string "enum (";
		Printf.printf "%s" (List.hd (List.rev l));
		List.iter (fun i->(Printf.printf ",%s" i)) (List.tl (List.rev l));
		print_string ")"
	|UNKNOW_TYPE->print_string "unknow_type"


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
	| CANON_EXPR (_, n, args) ->
		print_string "\"";
		print_string n;
		print_string "\"(";
		let _ = List.fold_left print_arg true args in
		print_string ")"
	| FIELDOF(_, e, n) ->
		print_expr e;
		print_string ".";
		print_string n
	| REF id ->
		print_string id
	| ITEMOF (_, e, idx) ->
		print_expr e;
		print_string "[";
		print_expr idx;
		print_string "]"
	| BITFIELD (_,e, l, u) ->
		print_expr e;
		print_string "<";
		print_expr l;
		print_string "..";
		print_expr u;
		print_string ">"
	| UNOP (_,op, e) ->
		print_string (string_of_unop op);
		print_expr e
	| BINOP (_,op, e1, e2) ->
		print_string "(";
		print_expr e1;
		print_string ")";
		print_string (string_of_binop op);
		print_string "(";
		print_expr e2;
		print_string ")"
	| IF_EXPR (_,c, t, e) ->
		print_string "if ";
		print_expr c;
		print_string " then ";
		print_expr t;
		print_string " else ";
		print_expr e;
		print_string " endif"
	| SWITCH_EXPR (_,c, cases, def) ->
		print_string "switch(";
		print_expr c;
		print_string ")";
		print_string "{ ";
		List.iter (fun (c, e) ->
				print_string "case ";
				print_expr c;
				print_string ": ";
				print_expr e;
				print_string " "
			) (List.rev cases);
		print_string "default: ";
		print_expr def;
		print_string " }"
	| CONST (_,c) ->
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

(** Print a statement
	@param stat	Statement to print.*)
let rec print_statement stat=
	match stat with
	  NOP -> print_string "\t\t <NOP>;\n"
	| SEQ (stat1, stat2)->print_statement stat1; print_statement stat2
	| EVAL ch-> Printf.printf "\t\t%s;\n" ch
	| EVALIND (ch1, ch2)->Printf.printf "\t\t%s.%s;\n" ch1 ch2
	| SET (loc, exp)->print_string "\t\t";print_location loc; print_string "=";print_expr exp; print_string ";\n"
	| CANON_STAT (ch, expr_liste)-> Printf.printf "\t\t \"%s\"" ch; List.iter print_expr expr_liste ;print_string ";\n"
	| ERROR ch->Printf.printf "\t\t error %s;\n" ch
	| IF_STAT (exp,statT,statE)-> 	print_string "\t\t if "; print_expr exp;print_string "\n";
					print_string "\t\t then \n"; print_statement statT;
					print_string "\t\t else \n";print_statement statE;
					print_string "\t\t endif;\n"
	|SWITCH_STAT (exp,stat_liste,stat)->print_string "\t\t switch (";print_expr exp;print_string ") {\n";
						List.iter (fun (v,s)->	print_string "\t\t\t case";print_expr v;print_string " : \n\t\t";
									print_statement s) (List.rev stat_liste);
						print_string "\t\t\t default : \n\t\t";print_statement stat;
						print_string "\t\t }; \n"
	|SETSPE (loc, exp)->print_string "\t\t";print_location loc; print_string "=";print_expr exp; print_string ";\n"



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

	| ATTR_STAT (id, stat) -> Printf.printf "\t%s = {\n" id ;
				  print_statement stat;
				  Printf.printf "\t}\n";
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
		print_string "\n";
		List.iter print_attr (List.rev attrs) ;
		print_newline ();
	| OR_MODE (name, modes) -> Printf.printf "mode %s = " name ;
				   List.iter (fun a -> Printf.printf " %s | " a) (List.rev (List.tl modes)) ;
				   Printf.printf "%s" (List.hd (modes));
				   Printf.printf "\n";
		()
	| AND_OP (name, pars, attrs) -> Printf.printf "op %s (" name ;
					if (List.length pars)>0
					then begin
						List.iter (fun a -> begin 	Printf.printf "%s : " (fst a) ; 
										print_type (snd a); 
										Printf.printf ", ";
								   end) (List.rev (List.tl pars));
						Printf.printf "%s : " (fst (List.hd pars)); 
						print_type (snd (List.hd pars));
					end;
					Printf.printf ")\n";
					List.iter print_attr (List.rev attrs) ;
		()
	| OR_OP (name, modes) -> Printf.printf "op %s = " name ;		 
				 List.iter (fun a -> Printf.printf " %s | " a) (List.rev (List.tl modes));
				 Printf.printf "%s" (List.hd modes);
				 Printf.printf "\n";
		()

	| PARAM (name,t)->Printf.printf "param %s (" name;print_type t;print_string ")\n";

		()
	| ENUM_POSS (name,s,_,_)->Printf.printf "possibility %s of enum %s\n" name s;
		()

	| _ ->
		assert false

