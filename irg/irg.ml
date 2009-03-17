(*
 * $Id: irg.ml,v 1.22 2009/03/17 16:23:53 barre Exp $
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
exception IrgError of string

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
	| ITEMOF of type_expr * string * expr
	| BITFIELD of type_expr * expr * expr * expr
	| UNOP of type_expr * unop * expr
	| BINOP of type_expr * binop * expr * expr
	| IF_EXPR of type_expr * expr * expr * expr
	| SWITCH_EXPR of type_expr * expr * (expr * expr) list * expr
	| CONST of type_expr*const
	| ELINE of string * int * expr
	
(** Statements *)
type location =
	| LOC_NONE												(** null location *)
	| LOC_REF of type_expr * string * expr * expr * expr	(** (type, memory name, index, lower bit, upper bit) *)
	| LOC_CONCAT of type_expr * location * location			(** concatenation of locations *)


(** argument of attributes *)
type attr_arg =
	| ATTR_ID of string * attr_arg list
	| ATTR_VAL of const


(** A memory or register attribute. *)
type mem_attr =
	| VOLATILE of int					(** volatile attribute *)
	| PORTS of int * int				(** ports attribute *)
	| ALIAS of location					(** alias attribute *)
	| INIT of const						(** init attribute *)
	| USES (*of uses*)					(** use attribute (unsupported) *)
	| ATTR of string * attr_arg list	(** generic memory attribute *)


(** A statement in an action. *)
type stat =
	  NOP
	| SEQ of stat * stat
	| EVAL of string
	| EVALIND of string * string
	| SET of location * expr
	| CANON_STAT of string * expr list
	| ERROR of string	(* a changer : stderr ? *)
	| IF_STAT of expr * stat * stat
	| SWITCH_STAT of expr * (expr * stat) list * stat
	| SETSPE of location * expr	(* Used for allowing assigment of parameters (for exemple in predecode attribute). 
					   This is NOT in the nML standard and is only present for compatibility *) 
	| LINE of string * int * stat	(* Used to memorise the position of a statement *)


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
	| PARAM of string * typ
	| ENUM_POSS of string*string*Int32.t*bool	(*	Fields of ENUM_POSS :
								the first parameter is the symbol of the enum_poss, 
								the second is the symbol of the ENUM where this ENUM_POSS is defined (must be completed - cf function "complete_incomplete_enum_poss"),
								the third is the value of this ENUM_POSS,
								the fourth is a flag to know if this ENUM_POSS is completed already (cf function "complete_incomplete_enum_poss")	*)

let name_of spec =
	match spec with
	  UNDEF -> "<undef>"
	| LET (name, _) -> name
	| TYPE (name, _) -> name
	| MEM (name, _, _, _) -> name
	| REG (name, _, _, _) -> name
	| VAR (name, _, _) -> name
	| AND_MODE (name, _, _, _) -> name
	| OR_MODE (name, _) -> name
	| AND_OP (name, _, _) -> name
	| OR_OP (name, _) -> name
	| RES (name) -> name
	| EXN (name) -> name
	| PARAM (name, _) -> name
	| ENUM_POSS (name, _, _, _) -> name
	


(* Symbol table *)
module HashString =
struct
	type t = string
	let equal (s1 : t) (s2 : t) = s1 = s2
	let hash (s : t) = Hashtbl.hash s
end
module StringHashtbl = Hashtbl.Make(HashString)
let syms : spec StringHashtbl.t = StringHashtbl.create 211


(** Get the symbol matching the given name or UNDEF if not found.*)
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

(**	Check if a given name is defined in the namespace
		@param name	The name to check *)
let is_defined name = StringHashtbl.mem syms name

(**	Add a parameter in the namespace.
		This function don't raise RedefinedSymbol if the name already exits.
		It is used to temporary overwrite existing symbols with the same name than a parameter

		@param name	Name of the parameter to add.
		@param t	Type of the parameter to add.	*)
let add_param (name,t) =StringHashtbl.add syms name (PARAM (name,t))

(**	Remove a symbol from the namespace.
		@param name	The name of the symbol to remove. *)
let rm_symbol name=StringHashtbl.remove syms name

(**	Add a list of parameters to the namespace.
		@param l	The list of parameters to add.	*)
let param_stack l= List.iter add_param l

(**	Remove a list of parameters from the namespace.
		@param l	The list of parameters to remove.	*)
let param_unstack l= List.iter (StringHashtbl.remove syms) (List.map fst l)

(**	This function is used to make the link between an ENUM_POSS and the corresponding ENUM.
		It must be used because when the parser encounter an ENUM_POSS, it doesn't have reduce	(* a changer : stderr ? *)d the ENUM already.
		The ENUM can be reduced only when all the ENUM_POSS have been.
		So when reduced, the ENUM_POSS have an boolean attribute "completed" set at false and their enum attribute is empty.
		When the ENUM is reduced, we fill the enum attribute to make the link, and set the "completed" to true
		
		@param id	The id of the enum
*)
let complete_incomplete_enum_poss id =
	StringHashtbl.fold (fun e v d-> match v with 
				ENUM_POSS (n,_,t,false)-> StringHashtbl.replace syms e (ENUM_POSS (n,id,t,true))
				|_->d 
			) syms ()



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

type canon_fun={name : canon_name_type; type_param : type_expr list ; type_res:type_expr}

(* the canonical functions space *)
let canon_table : canon_fun CanonHashtbl.t = CanonHashtbl.create 211

(* list of all defined canonical functions *)
let canon_list = [
			{name= UNKNOW;type_param=[];type_res=UNKNOW_TYPE};(* this is the "default" canonical function, used for unknown functions *)
			{name=NAMED "sin";type_param=[FLOAT (24,8)];type_res=FLOAT (24,8)};
			{name=NAMED "print";type_param=[STRING];type_res=NO_TYPE} 
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

(* --- end canonical functions --- *)


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
		output_string out (Int32.to_string v)
	| CARD_CONST_64 v->
		output_string out (Int64.to_string v)
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
		Printf.fprintf out "%s" (List.hd (List.rev l));
		List.iter (fun i->(Printf.fprintf out ",%s" i)) (List.tl (List.rev l));
		output_string out ")"
	|UNKNOW_TYPE -> output_string out "unknow_type"


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
		output_expr out e;
		output_string out ".";
		output_string out n
	| REF id ->
		output_string out id
	| ITEMOF (_, name, idx) ->
		output_string out name;
		output_string out "[";
		output_expr out idx;
		output_string out "]"
	| BITFIELD (t, e, l, u) ->
		output_expr out e;
		output_string out "<";
		output_expr out l;
		output_string out "..";
		output_expr out u;
		output_string out ">"
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
	| IF_EXPR (_,c, t, e) ->
		output_string out "if ";
		output_expr out c;
		output_string out " then ";
		output_expr out t;
		output_string out " else ";
		output_expr out e;
		output_string out " endif"
	| SWITCH_EXPR (_,c, cases, def) ->
		output_string out "switch(";
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
	| ELINE (_, _, e) ->
		output_expr out e
	| CONST (_,c) ->
		output_const out c


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
	| LOC_REF (_, id, idx, lo, up) ->
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
			end
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
	| EVAL ch ->
		Printf.fprintf out "\t\t%s;\n" ch
	| EVALIND (ch1, ch2) ->
		Printf.fprintf out "\t\t%s.%s;\n" ch1 ch2
	| SET (loc, exp) ->
		output_string out "\t\t";
		output_location out loc;
		output_string out "=";
		output_expr out exp;
		output_string out ";\n"
	| CANON_STAT (ch, expr_liste) ->
		Printf.fprintf out "\t\t\"%s\" (" ch;
		List.iter (output_expr out) expr_liste ;
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
	| SETSPE (loc, exp) ->
		output_string out "\t\tSETSPE:";
		output_location out loc;
		output_string out "=";
		output_expr out exp;
		output_string out ";\n"
	| LINE (_,_,s) ->
		output_statement out s


(** Print a statement
	@param stat	Statement to print.*)
let rec print_statement stat= output_statement stdout stat



(** Print a memory attibute.
	@param attr	Memory attribute to print. *)
let print_mem_attr attr =
	let rec print_call id args =
		print_string id;
		if args <> [] then
			begin
				ignore(List.fold_left (fun sep arg ->
						print_string sep;
						print_arg arg;
						", "
					) "(" args);
				print_string ")"
			end
	and print_arg arg =
		match arg with
		| ATTR_ID (id, args) -> print_call id args
		| ATTR_VAL cst -> print_const cst in

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
	| ATTR (id, args) ->
		print_call id args


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

	| UNDEF ->
		print_string "<UNDEF>";
		()


(* now let's try to obtain all the real OPs from a given OP by "unrolling" all MODES *)


(* some useful functions *)

let get_isize _ =
	let s = get_symbol "gliss_isize"
	in
	match s with
	(* if gliss_isize not defined, we assume we have a cisc isa *)
	UNDEF ->
		[]
	| LET(st, cst) ->
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

let get_type p =
	match p with
	(str, TYPE_ID(n)) ->
		n
	| _ ->
		"[err741]"		

let get_name p =
	match p with
	(str, _) ->
		str

let print_param p =
	match p with
	(str, TYPE_ID(n)) ->
		Printf.printf " (%s:%s) " (get_name p) (get_type p)
	| (str, TYPE_EXPR(t)) ->
		begin
		Printf.printf " (%s:" (get_name p);
		print_type (TYPE_EXPR(t));
		print_string ") "
		end

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



(** get the statement associated with attribute name_attr of the spec sp *)
let get_stat_from_attr_from_spec sp name_attr =
	let rec get_attr n a =
		match a with
		[] -> 
		(* if attr not found => means an empty attr (?) *)
			NOP
		| ATTR_STAT(nm, s)::t ->
			if nm = n then
				s
			else
				get_attr n t
		| _::t -> get_attr n t
	in
		match sp with
		  AND_OP(_, _, attrs) ->
			get_attr name_attr attrs
		| AND_MODE(_, _, _, attrs) ->
			get_attr name_attr attrs
		| _ ->
			NOP
			
			(*let sp_name = name_of sp in
			raise (IrgError ("access to " ^ sp_name ^ "." ^ name_attr
				^ " while " ^ sp_name ^ " is neither an OP or a MODE"))*)
	

(* symbol substitution is needed *)

(** get the expression associated with attribute name_attr of the OP op *)
let get_expr_from_attr_from_op_or_mode sp name_attr =
	let rec get_attr n a =
		match a with
		[] -> 
		(* if attr not found => means an empty attr (?) *)
			NONE
		| ATTR_EXPR(nm, e)::t ->
			if nm = n then
				e
			else
				get_attr n t
		| _::t -> get_attr n t
	in
		match sp with
		AND_OP(_, _, attrs) ->
			get_attr name_attr attrs
		| AND_MODE(_, _, _, a_l) ->
			get_attr name_attr a_l
		| _ ->
			NONE
			(*failwith "cannot get an expr attribute from not an AND OP or an AND MODE"*)

let rec substitute_in_expr name op ex =
	let is_and_mode sp =
		match sp with
		AND_MODE(_, _, _, _) -> true
		| _ -> false
	in
	let get_mode_value sp =
		match sp with
		AND_MODE(_, _, v, _) -> v
		| _-> NONE
	in
	match ex with
	NONE ->
		NONE
	| COERCE(te, e) ->
		COERCE(te, substitute_in_expr name op e)
	| FORMAT(s, e_l) ->
		(* done by another function which replace everything in one shot *)
		FORMAT(s, e_l)
	| CANON_EXPR(te, s, e_l) ->
		CANON_EXPR(te, s, List.map (substitute_in_expr name op) e_l )
	| REF(s) ->
		(* change if op is a AND_MODE and s refers to it *)
		if (name=s)&&(is_and_mode op) then
			get_mode_value op
		else
			REF(s)
	| FIELDOF(te, e, s) ->
		if e = REF(name) then
			get_expr_from_attr_from_op_or_mode op s
		else
			FIELDOF(te, substitute_in_expr name op e, s)
	| ITEMOF(te, e1, e2) ->
		ITEMOF(te, e1, substitute_in_expr name op e2)
	| BITFIELD(te, e1, e2, e3) ->
		BITFIELD(te, substitute_in_expr name op e1, substitute_in_expr name op e2, substitute_in_expr name op e3)
	| UNOP(te, un_op, e) ->
		UNOP(te, un_op, substitute_in_expr name op e)
	| BINOP(te, bin_op, e1, e2) ->
		BINOP(te, bin_op, substitute_in_expr name op e1, substitute_in_expr name op e2)
	| IF_EXPR(te, e1, e2, e3) ->
		IF_EXPR(te, substitute_in_expr name op e1, substitute_in_expr name op e2, substitute_in_expr name op e3)
	| SWITCH_EXPR(te, e1, ee_l, e2) ->
		SWITCH_EXPR(te, substitute_in_expr name op e1, List.map (fun (x,y) -> (substitute_in_expr name op x, substitute_in_expr name op y)) ee_l, substitute_in_expr name op e2)
	| CONST(te, c)
		-> CONST(te, c)
	| ELINE (file, line, e) -> ELINE (file, line, substitute_in_expr name op e)


(** search the symbol name in the given statement,
the symbol is supposed to stand for a variable of type given by op,
all occurrences of names are translated to refer to the op *)
let rec substitute_in_stat name op statement =
	match statement with
	NOP ->
		NOP
	| SEQ(s1, s2) ->
		SEQ(substitute_in_stat name op s1, substitute_in_stat name op s2)
	| EVAL(s) ->
		EVAL(s)
	| EVALIND(n, attr) ->
		if n = name then
			get_stat_from_attr_from_spec op attr
		else
			EVALIND(n, attr)
	| SET(l, e) ->
		SET(l, substitute_in_expr name op e)
	| CANON_STAT(n, el) ->
		CANON_STAT(n, el)
	| ERROR(s) ->
		ERROR(s)
	| IF_STAT(e, s1, s2) ->
		IF_STAT(substitute_in_expr name op e, substitute_in_stat name op s1, substitute_in_stat name op s2)
	| SWITCH_STAT(e, es_l, s) ->
		(* is it really useful ? *)
		SWITCH_STAT(substitute_in_expr name op e, List.map (fun (ex, st) -> (ex, substitute_in_stat name op st)) es_l, substitute_in_stat name op s)
	| SETSPE(l, e) ->
		(* don't know what it is exactly, cannot find an example *)
		SETSPE(l, substitute_in_expr name op e)
	| LINE(s, i, st) ->
		LINE(s, i, substitute_in_stat name op st)
		

let rec change_name_of_var_in_expr ex var_name new_name =
	match ex with
	NONE ->
		NONE
	| COERCE(t_e, e) ->
		COERCE(t_e, change_name_of_var_in_expr e var_name new_name)
	| FORMAT(s, e_l) ->
		FORMAT(s, List.map (fun x -> change_name_of_var_in_expr x var_name new_name) e_l)
	| CANON_EXPR(t_e, s, e_l) ->
		CANON_EXPR(t_e, s, List.map (fun x -> change_name_of_var_in_expr x var_name new_name) e_l)
	| REF(s) ->
		if s = var_name then
			REF(new_name)
		else
			REF(s)
	| FIELDOF(t_e, e, s) ->
		FIELDOF(t_e, change_name_of_var_in_expr e var_name new_name, s)
	| ITEMOF(t_e, e1, e2) ->
		ITEMOF(t_e, e1, change_name_of_var_in_expr e2 var_name new_name)
	| BITFIELD(t_e, e1, e2, e3) ->
		BITFIELD(t_e, change_name_of_var_in_expr e1 var_name new_name, change_name_of_var_in_expr e2 var_name new_name, change_name_of_var_in_expr e3 var_name new_name)
	| UNOP(t_e, u, e) ->
		UNOP(t_e, u, change_name_of_var_in_expr e var_name new_name)
	| BINOP(t_e, b, e1, e2) ->
		BINOP(t_e, b, change_name_of_var_in_expr e1 var_name new_name, change_name_of_var_in_expr e2 var_name new_name)
	| IF_EXPR(t_e, e1, e2, e3) ->
		IF_EXPR(t_e, change_name_of_var_in_expr e1 var_name new_name, change_name_of_var_in_expr e2 var_name new_name, change_name_of_var_in_expr e3 var_name new_name)
	| SWITCH_EXPR(te, e1, ee_l, e2) ->
		SWITCH_EXPR(te, change_name_of_var_in_expr e1 var_name new_name, List.map (fun (x,y) -> (change_name_of_var_in_expr x var_name new_name, change_name_of_var_in_expr y var_name new_name)) ee_l, change_name_of_var_in_expr e2 var_name new_name)
	| CONST(t_e, c) ->
		CONST(t_e, c)
	| ELINE(file, line, e) -> ELINE(file, line, change_name_of_var_in_expr e var_name new_name)



let rec change_name_of_var_in_location loc var_name new_name =
	match loc with
	| LOC_NONE -> loc
	| LOC_REF(t, s, i, l, u) ->
		LOC_REF (
			t,
			(if s = var_name then new_name else s),
			change_name_of_var_in_expr i var_name new_name,
			change_name_of_var_in_expr l var_name new_name,
			change_name_of_var_in_expr u var_name new_name)
	| LOC_CONCAT(t, l1, l2) ->
		LOC_CONCAT(t, change_name_of_var_in_location l1 var_name new_name, change_name_of_var_in_location l2 var_name new_name)

let rec change_name_of_var_in_stat sta var_name new_name =
	match sta with
	NOP ->
		NOP
	| SEQ(s1, s2) ->
		SEQ(change_name_of_var_in_stat s1 var_name new_name, change_name_of_var_in_stat s2 var_name new_name)
	| EVAL(str) ->
		EVAL(str)
	| EVALIND(v, attr_name) ->
		if v = var_name then
			EVALIND(new_name, attr_name)
		else
			EVALIND(v, attr_name)
	| SET(l, e) ->
		SET(change_name_of_var_in_location l var_name new_name, change_name_of_var_in_expr e var_name new_name)
	| CANON_STAT(str, e_l) ->
		CANON_STAT(str, List.map (fun x -> change_name_of_var_in_expr x var_name new_name) e_l)
	| ERROR(str) ->
		ERROR(str) (* ??? *)
	| IF_STAT(e, s1, s2) ->
		IF_STAT(change_name_of_var_in_expr e var_name new_name, change_name_of_var_in_stat s1 var_name new_name, change_name_of_var_in_stat s2 var_name new_name)
	| SWITCH_STAT(e, es_l, s) ->
		SWITCH_STAT(change_name_of_var_in_expr e var_name new_name, List.map (fun (x,y) -> (change_name_of_var_in_expr x var_name new_name, change_name_of_var_in_stat y var_name new_name)) es_l, change_name_of_var_in_stat s var_name new_name)
	| SETSPE(l, e) ->
		SETSPE(change_name_of_var_in_location l var_name new_name, change_name_of_var_in_expr e var_name new_name)
	| LINE(str, n, s) ->
		LINE(str, n, change_name_of_var_in_stat s var_name new_name) (* ??? *)


let change_name_of_var_in_attr a var_name new_name =
	match a with
	ATTR_EXPR(str, e) ->
		ATTR_EXPR(str, change_name_of_var_in_expr e var_name new_name)
	| ATTR_STAT(str, s) ->
		ATTR_STAT(str, change_name_of_var_in_stat s var_name new_name)
	| ATTR_USES ->
		ATTR_USES

let prefix_attr_var a param pfx =
	match param with
	(str, _) ->
		change_name_of_var_in_attr a str (pfx^"_"^str)

let rec prefix_all_vars_in_attr a p_l pfx =
	match p_l with
	h::q ->
		prefix_all_vars_in_attr (prefix_attr_var a h pfx) q pfx
	| [] ->
		a

let prefix_var_in_mode_value mode_value param pfx =
	match param with
	(str, _) ->
		change_name_of_var_in_expr mode_value str (pfx^"_"^str)

let rec prefix_all_vars_in_mode_value mode_value p_l pfx =
	match p_l with
	h::q ->
		prefix_all_vars_in_mode_value (prefix_var_in_mode_value mode_value h pfx) q pfx
	| [] ->
		mode_value

(* prefix the name of every param of the given spec by a given prefix,
every occurrence in every attr must be prefixed *)
let rec prefix_name_of_params_in_spec sp pfx =
	let prefix_param param =
		match param with
		(name, t) ->
			(pfx^"_"^name, t)
	in
	match sp with
	AND_OP(name, params, attrs) ->
		AND_OP(name, List.map prefix_param params, List.map (fun x -> prefix_all_vars_in_attr x params pfx) attrs)
	| AND_MODE(name, params, mode_val, attrs) ->
		AND_MODE(name, List.map prefix_param params, prefix_all_vars_in_mode_value mode_val params pfx, List.map (fun x -> prefix_all_vars_in_attr x params pfx) attrs)
	| _ ->
		sp

	
	
(* str_format is a regexp representing a %... from a format expr 
 expr_field is supposed to be an expr of type FIELDOF ("x.syntax") or else corresponding to str_format
 spec_type is the spec of the base of the expr (here : the spec of "x"), has meaning only for some types of expr *)
let rec replace_format_by_attr str_format expr_field spec_type =
	(*return the "useful" string from an expr of type format(...) or simple type *)
	let rec get_str_from_format_expr f =
		match f with
		FORMAT(s, _) -> s
		| CONST(STRING, s) ->
			(match s with
			STRING_CONST(str) -> str
			| _ -> "[err1008]"
			)
		| REF(s) -> s
		| ELINE (_, _, e) -> get_str_from_format_expr e
		| _ -> "[err1012]"
	in
	match str_format with
	Str.Text(t) -> t
	| Str.Delim(t) ->
		(match expr_field with
		(* replace expr like "x.image" "y.syntax" etc *)
		FIELDOF(_, _, s) ->
			get_str_from_format_expr (get_expr_from_attr_from_op_or_mode spec_type s)
		(* replace format by text for string constants *)
		| CONST(STRING, cst) ->
			(match cst with
			STRING_CONST(str) -> str
			| _ -> ""
			)
		| ELINE (_, _, e) -> replace_format_by_attr str_format e spec_type
		(* leave unchanged for simple expr of simple types, like "tmp", "x" *)
		| _ -> t)



(* replace an "x.image" (x is an op foo) param by the params of the "image" attribute in the "foo" spec 
the attribute in foo is supposed to be a format (returns the parma of the format) or a string const (returns no params) *)
let rec replace_field_expr_by_param_list expr_field spec_type =
	let rec get_param_list_from_format_expr f =
		match f with
		FORMAT(_, l) -> l
		| ELINE (_, _, e) -> get_param_list_from_format_expr e
		| _ -> []
	in
	match expr_field with
	FIELDOF(_, _, s) ->
		get_param_list_from_format_expr (get_expr_from_attr_from_op_or_mode spec_type s)
	| ELINE (_, _, e) -> replace_field_expr_by_param_list e spec_type
	| q -> q::[]

let get_param_of_spec s =
	match s with
	AND_OP(_, l, _) -> l
	| AND_MODE(_, l, _, _) -> l
	| _ -> []

(*  *)
let replace_param_list p_l =
	let prefix_name prfx param =
		match param with
		(name, t) ->
			(prfx^"_"^name, t)
	in
	let replace_param param =
		match param with
		(nm , TYPE_ID(s)) ->
			List.map (prefix_name nm) (get_param_of_spec (get_symbol s))
		| (_, _) ->
			[param]
	in
	List.flatten (List.map replace_param p_l)
		
	
let rec search_spec_of_name name param_list =
	let spec_from_type t =
		match t with
		TYPE_ID(n) -> get_symbol n
		| _ -> UNDEF
	in
	let rec rec_aux nam p_l =
		match p_l with
		(n, t)::q ->
			if n = nam then
				spec_from_type t
			else
				search_spec_of_name name q
		| [] -> print_string "params : "; print_param_list param_list; failwith (String.concat "" ["internal error: search_spec_of_name : "; name])
	in
	rec_aux name param_list




(* e is an expr appearing in the params given (supposed to be the params of another spec)
if e is like "x.image", it returns the spec of "x" where all var names are prefixed by the name of the former param,
avoid same name for different vars if instantiating several params of same type *)
let get_spec_from_expr e spec_params =
	let rec rec_aux ex p_l =
		match ex with
		FIELDOF(_, expre, _) -> rec_aux expre spec_params
		| REF(name) -> prefix_name_of_params_in_spec (search_spec_of_name name spec_params) name
		| ELINE (_, _, e) -> rec_aux e p_l
		| _ -> UNDEF
	in
	rec_aux e spec_params


let rec regexp_list_to_str_list l =
	match l with
	[] -> []
	| Str.Text(txt)::b -> txt::(regexp_list_to_str_list b)
	| Str.Delim(txt)::b -> txt::(regexp_list_to_str_list b)
	
let string_to_regexp_list s =
	Str.full_split (Str.regexp "%[0-9]*[dbxsf]") s

let str_list_to_str l =
	String.concat "" l

let print_reg_exp e =
	match e with
	Str.Text(t) ->
		print_string t
	| Str.Delim(t) ->
		print_string t

let rec print_reg_list e_l =
	match e_l with
	[] ->
		()
	| a::b ->
		begin
		print_reg_exp a;
		print_string ":";
		print_reg_list b
		end


(* the reg_list is supposed to represent an expr from a format (eg : "001101%8b00%d")
expr_list is the params of the same format (eg : x, z, y.image, TMP_VAR)
the format is supposed to be an attribute of a spec
whose params are given in spec_params 
returns a string list where each format has been replaced by the correct image or syntax (or else) *)
let transform_str_list reg_list expr_list spec_params =
	let rec print_reg_list rl =
		match rl with
		[] ->
			print_string "\n"
		| h::q ->
			(match h with
			Str.Text(t) ->
				begin
				Printf.printf "(T)%s#" t;
				print_reg_list q
				end
			| Str.Delim(t) ->
				begin
				Printf.printf "(D)%s#" t;
				print_reg_list q
				end
			)
	in
	(*let print_expr_list el =
		List.iter (fun x -> begin print_expr x; print_string ", " end) el
	in*)
	let rec rec_aux r_l e_l p_l =
		match r_l with
		[] -> []
		| a::t ->
			(match e_l with
			[] -> regexp_list_to_str_list r_l
			| b::u ->
				(match a with
				Str.Text(txt) ->
				begin
				txt::(rec_aux t e_l p_l)
				end
				(* we suppose everything is well formed, each format has one param, params are given in format's order *)
				| Str.Delim(txt) -> 
				begin
				(replace_format_by_attr a b (get_spec_from_expr b p_l))::(rec_aux t u p_l)
				end
				)
			)
	in
	rec_aux reg_list expr_list spec_params


(* instantiate all var in expr_frmt (of type format(ch, p1, p2, ..., pn) )
the vars to instantiate are given in a list of couples (name of var, spec of the var)
the vars must have been instantiated to real op (not OR op which have no attribute at all) *)
let change_format_attr expr_frmt param_list =
	let rec get_str_from_format_expr f =
		match f with
		FORMAT(s, _) -> s
		| CONST(STRING, s) ->
			(match s with
			STRING_CONST(str) -> str
			| _ -> ""
			)
		| REF(s) -> s
		| ELINE(_, _, e) -> get_str_from_format_expr e
		| _ -> "[err1357]"
	in
	let rec get_param_from_format_expr f =
		match f with
		FORMAT(_, p) -> p
		| ELINE(_, _, e) -> get_param_from_format_expr e
		| _ -> []
	in
	let str_frmt = string_to_regexp_list (get_str_from_format_expr expr_frmt)
	in
	let param_frmt = get_param_from_format_expr expr_frmt
	in
	let rec reduce_frmt f =
		match f with
		FORMAT(str, p) ->
			if p = [] then
				CONST(STRING, STRING_CONST(str))
			else
				f
		| ELINE (file, line, e) ->
			(* rough fix for a bug where we obtain a lot of EL imbricated outside a transformed format *)
			(match e with
			ELINE(_, _, _) ->
				reduce_frmt e
			| _ ->
				ELINE(file, line, reduce_frmt e)
			)
		| _ -> f
	in
	reduce_frmt (FORMAT(str_list_to_str (transform_str_list str_frmt param_frmt param_list), List.flatten (List.map (fun x -> replace_field_expr_by_param_list x (get_spec_from_expr x param_list)) param_frmt)))
	

(* replace the type by the spec if the param refers to an op or mode,
the param is dropped if it is of a simple type *)
let rec string_typ_list_to_string_spec_list l =
	match l with
	[] ->
		[]
	| a::b ->
		(match a with
		(name, TYPE_ID(t)) ->
			(name, get_symbol t)::(string_typ_list_to_string_spec_list b)
		| _ ->
			string_typ_list_to_string_spec_list b
		)


let rec instantiate_in_stat sta param_list =
	match param_list with
	[] ->
		sta
	| (name, TYPE_ID(t))::q ->
		instantiate_in_stat (substitute_in_stat name (prefix_name_of_params_in_spec (get_symbol t) name) sta) q
	| (name, TYPE_EXPR(e))::q ->
		instantiate_in_stat sta q
		
let rec instantiate_in_expr ex param_list =
	let rec aux e p_l =
		match p_l with
		[] ->
			e
		| (name, TYPE_ID(t))::q ->
			aux (substitute_in_expr name (prefix_name_of_params_in_spec (get_symbol t) name) e) q
		| (name, TYPE_EXPR(ty))::q ->
			aux e q
	in
	match ex with
	FORMAT(_, _) ->
		change_format_attr ex param_list
	| ELINE(a, b, e) ->
		ELINE(a, b, instantiate_in_expr e param_list)
	| _ ->
		aux ex param_list


let instantiate_param param =
	let rec aux p =
		match p with
		(name, TYPE_ID(typeid))::q ->
			(match get_symbol typeid with
			OR_OP(_, str_l) ->
				(List.flatten (List.map (fun x -> aux [(name, TYPE_ID(x))]) str_l)) @ (aux q)
			| OR_MODE(_, str_l) ->
				(List.flatten (List.map (fun x -> aux [(name, TYPE_ID(x))]) str_l)) @ (aux q)
			| AND_OP(_, _, _) ->
				p @ (aux q)
			| AND_MODE(_, _, _, _) ->
				p @ (aux q)
			| _ ->
				p @ (aux q) (* will this happen? *)
			)
		| [] ->
			[]			
		| _ ->
			p
	in
	match param with
	(name, TYPE_EXPR(te)) ->
		[param]
	| (_ , _) ->
		aux [param]


let rec cross_prod l1 l2 =
	match l1 with
	[] ->
		[]
	| a::b ->
		if l2=[] then
			[]
		else
			(List.map (fun x -> [a;x]) l2) @ (cross_prod b l2)
			
let rec list_and_list_list_prod l ll =
	match l with
	[] ->
		[]
	| a::b ->
		if ll=[] then
			[]
		else
			(List.map (fun x -> a::x) ll) @ (list_and_list_list_prod b ll)

let list_prod p_ll =
	let rec expand l =
		match l with
		[] ->
			[]
		| a::b ->
			[a]::(expand b)
	in
	match p_ll with
	[] ->
		[]
	| a::b ->
		(match b with
		[] ->
			expand a
		| c::d ->
			if List.length d = 0 then
				cross_prod a c
			else
				List.fold_left (fun x y -> list_and_list_list_prod y x) (cross_prod a c) d
		)

let instantiate_param_list p_l =
	let a = List.map instantiate_param p_l
	in
	list_prod a

let instantiate_attr a params=
	match a with
	ATTR_EXPR(n, e) ->
		ATTR_EXPR(n, instantiate_in_expr e params)
	| ATTR_STAT(n, s) ->
		ATTR_STAT(n, instantiate_in_stat s params)
	(* useless until now *)
	| ATTR_USES ->
		ATTR_USES

(* when instantiating the given param in the given spec
we must add to the spec the attribute of the param which are not in the given spec *)
let add_attr sp param =
	let get_attrs s =
		match s with
		AND_OP(_, _, a_l) -> a_l
		| AND_MODE(_, _, _, a_l) -> a_l
		| _ -> []
	in
	let compare_attrs a1 a2 =
		match a1 with
		ATTR_EXPR(n, _) ->
			(match a2 with
			ATTR_EXPR(nn, _) ->
				if nn=n then
					true
				else
					false
			| _ -> false
			)
		| ATTR_STAT(n, _) ->
			(match a2 with
			ATTR_STAT(nn, _) ->
				if nn=n then
					true
				else
					false
			| _ -> false
			)
		| ATTR_USES ->
			if a2 = (ATTR_USES) then
				true
			else
				false
	in
	(* returns the attr in param not present in sp *)
	let rec search_in_attrs a_l a_l_param =
		match a_l_param with
		[] -> []
		| a::b ->
			if List.exists (fun x -> compare_attrs a x) a_l then
				search_in_attrs a_l b
			else
				a::(search_in_attrs a_l b)
	in
	let attr_spec =
		get_attrs sp
	in
	let attr_param =
		match param with
		(name, TYPE_ID(s)) ->
			get_attrs (prefix_name_of_params_in_spec (get_symbol s) name)
		| (name, TYPE_EXPR(t)) ->
			[]
	in
		match sp with
		AND_OP(name, p_l, a_l) ->
			AND_OP(name, p_l, a_l@(search_in_attrs attr_spec attr_param))
		| AND_MODE(name, p_l, e, a_l) ->
			AND_MODE(name, p_l, e, a_l@(search_in_attrs attr_spec attr_param))
		| _ -> sp

(* add the attrs present in the params' specs but not in the main spec
to the main spec *)
let rec add_new_attrs sp param_list =
	match param_list with
	[] ->
		sp
	| a::b ->
		add_new_attrs (add_attr sp a) b


let instantiate_spec sp param_list =
	let is_type_def_spec sp =
		match sp with
		TYPE(_, _) ->
			true
		| _ ->
			false
	in
	(* replace all types by basic types (replace type definitions) *)
	let simplify_param p =
		match p with
		(str, TYPE_ID(n)) ->
			(* we suppose n can refer only to an OP or MODE, or to a TYPE *)
			let sp = get_symbol n in
			if is_type_def_spec sp then
				(match sp with
				TYPE(_, t_e) ->
					(str, TYPE_EXPR(t_e))
				| _ ->
					p
				)
			else
				p
		| (_, _) ->
			p
	in
	let simplify_param_list p_l =
		List.map simplify_param p_l
	in
	let new_param_list = simplify_param_list param_list
	in
	match sp with
	AND_OP(name, params, attrs) ->
		add_new_attrs (AND_OP(name, replace_param_list new_param_list, List.map (fun x -> instantiate_attr x new_param_list) attrs)) new_param_list
	| _ ->
		UNDEF


		
let instantiate_spec_all_combinations sp =
	let new_param_lists = instantiate_param_list (get_param_of_spec sp)
	in
		List.map (fun x -> instantiate_spec sp x) new_param_lists


let is_instantiable sp =
	let is_param_instantiable p =
		match p with
		(_, TYPE_ID(t)) ->
			true
		| (_, TYPE_EXPR(e)) ->
			false
	in
	let test param_list =
		List.exists is_param_instantiable param_list
	in
	match sp with
	AND_OP(_, p_l, _) ->
		test p_l
	| _ -> false


let rec instantiate_spec_list s_l =
	match s_l with
	[] ->
		[]
	| a::b ->
		if is_instantiable a then
			(instantiate_spec_all_combinations a)@(instantiate_spec_list b)
		else
			a::(instantiate_spec_list b)




(* this function instantiate all possible instructions given by the spec name in the hashtable *)			
let instantiate_instructions name =
	(* some AND_OP specs with empty attrs and no params are output, remove them
	TODO : see where they come from, this patch is awful. 
	here, we assume we have an empty spec as soon as the syntax or the image is void *)
	let is_void_attr a =
		match a with
		ATTR_EXPR(n, e) ->
			(match e with
			ELINE(_, _, ee) ->
				if n="syntax" && ee=NONE then
					true
				else
					if n="image" && ee=NONE then
						true
					else
						false
			| _ ->
				false
			)
		| _ ->
			false
	in
	let is_void_spec sp =
		match sp with
		AND_OP(_, _, attrs) ->
			List.exists is_void_attr attrs
		| _ ->
			false
	in
	let rec clean_attr a =
		match a with
		ATTR_EXPR(n, e) ->
			(match e with
			ELINE(f, l, ee) ->
				(match ee with
				ELINE(_, _, eee) ->
					clean_attr (ATTR_EXPR(n, ELINE(f, l, eee)))
				| _ ->
					ATTR_EXPR(n, ELINE(f, l, ee))
				)
			| _ ->
				a
			)
		| _ ->
			a
	in
	(* a try to fix over imbrication of ELINE *)
	let rec clean_eline s =
		match s with
		AND_OP(n, st_l, al) ->
			AND_OP(n, st_l, List.map clean_attr al)
		| _ ->
			s
	in
	let rec clean_instructions s_l =
		match s_l with
		[] ->
			[]
		| h::t ->
			if is_void_spec h then
				clean_instructions t
			else
				(clean_eline h)::(clean_instructions t)
	in
	let rec aux s_l =
		if List.exists is_instantiable s_l then
			aux (instantiate_spec_list s_l)
		else
			s_l
	in
	clean_instructions (aux [get_symbol name])




let test_instant_spec name =
	let rec print_spec_list l =
		match l with
		[] ->
			()
		| a::b ->
			begin
			print_spec a;
			print_string "\n";
			print_spec_list b
			end
	in
	print_spec_list (instantiate_instructions name)

let test_instant_param p =
	let rec print_param_list_list l =
	begin
		match l with
		[] ->
			()
		| h::q ->
			begin
			print_string "[";
			print_param_list h;
			print_string "]";
			print_param_list_list q
			end
	end
	in
	print_param_list_list (instantiate_param_list [("z",TYPE_EXPR(CARD(5))); ("x",TYPE_ID("_A")); ("y",TYPE_ID("_D"))])

let test_change_frmt name =
	let sp = get_symbol name
	in
	let frmt = get_expr_from_attr_from_op_or_mode sp "syntax"
	in
	let params = [("x",TYPE_ID("tutu1")); ("y",TYPE_ID("tata1"))]
	in
	print_expr (change_format_attr frmt params)

let test_format name =
	let sp = get_symbol name
	in
	let frmt = get_expr_from_attr_from_op_or_mode sp "syntax"
	in
	let spec_params = [("x",TYPE_ID("tutu1"));("y",TYPE_ID("tata1"))]
	(*get_param_of_spec sp*)
	in
	let get_str_from_format_expr f =
		match f with
		FORMAT(s, _) -> s
		| CONST(STRING, s) ->
			(match s with
			STRING_CONST(str) -> str
			| _ -> ""
			)
		| REF(s) -> s
		| _ -> ""
	in
	let get_param_from_format_expr f =
		match f with
		FORMAT(_, p) -> p
		| _ -> []
	in
	let print_string_list l =
	begin
		List.iter (fun x -> begin Printf.printf ":%s;\n" x end) l
	end
	in
	let print_res l =
	begin
		Printf.printf "avant:\n";
		print_string_list (regexp_list_to_str_list (string_to_regexp_list (get_str_from_format_expr frmt)));
		Printf.printf "après:\n";
		print_string_list l
	end
	in
	print_res (transform_str_list (string_to_regexp_list (get_str_from_format_expr frmt)) (get_param_from_format_expr frmt) spec_params)

let test_replace_param name =
	let sp = get_symbol name
	in
	let expre = FIELDOF(STRING, REF("x"), "image")
	in
	let rec print_expr_list e_l =
		List.map (fun x -> begin Printf.printf ":"; print_expr x; print_string ";\n" end) e_l
	in
	begin
		Printf.printf "res test param:\n";
		print_expr_list (replace_field_expr_by_param_list expre sp)
	end
	

