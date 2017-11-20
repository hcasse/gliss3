/*
 * $Id: parser.mly,v 1.26 2009/07/05 07:25:21 casse Exp $
 * Copyright (c) 2007-09, IRIT - UPS <casse@irit.fr>
 * SimNML parser
 *
 * This file is part of Gliss2.
 *
 * Gliss2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Gliss2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Foobar; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

%{
let eline e = Irg.ELINE (!(Lexer.file), !(Lexer.line), e)
let line s = Irg.LINE (!(Lexer.file), !(Lexer.line), s)


(** Raise an error at the current parsing position.
	@param f	Function to display error. *)
let error f =
		raise (Irg.Error (fun out -> Printf.fprintf out "%s:%d: " !(Lexer.file) !(Lexer.line); f out))


(** Handle an expression in the parser, i.e. put source file/line information
	or add file/line information when an error is raised.
	@param f			Function to perform expression building. It is called with () argument.
	@return				Built expression.
	@raise Irg.Error	If there is an error. *)
let handle_expr f =
	try
		Irg.ELINE (!(Lexer.file), !(Lexer.line), f ())
	with Irg.PreError ef ->
		error ef


(** Handle an statement in the parser, i.e. put source file/line information
	or add file/line information when an error is raised.
	@param f			Function to perform statement building. It is called with () argument.
	@return				Built statement.
	@raise Irg.Error	If there is an error. *)
let handle_stat f =
	try
		Irg.LINE (!(Lexer.file), !(Lexer.line), f ())
	with Irg.PreError ef ->
		error ef


(** Raise a syntax error exception.
	@param msg	Error message.
	@raise Irg.SyntaxError	Syntax error. *)
let syntax_error msg =
	raise (Irg.SyntaxError msg)


(** Get information to extend the symbol x.
	@param x				Name of the symbol to extend (must an AND-op or an AND-mode).
	@return					(specification of the symbol, list of parameters, list of attributes)
	@raise Irg.Error		If the symbol is not extensible. *)
let get_spec_extend x =
	let sym = Irg.get_symbol x in
	match sym with
	| Irg.AND_MODE (_, pars, _, attrs)
	| Irg.AND_OP (_, pars, attrs) ->
		(sym, pars, attrs)
	| Irg.UNDEF ->
		Irg.error (Irg.asis (Printf.sprintf "symbol %s does not exists" x))
	| _ ->
		Irg.error (Irg.asis (Printf.sprintf "can not extend %s" x))


(** Intersect parameter declaration.
	@param pars1		First list of parameters.
	@param pars2		Second list of parameters.
	@return				Intersection of lists. *)
let intersect_params pars1 pars2 : (string * Irg.typ) list =
	List.fold_left
		(fun res par -> if List.mem par pars1 then par::res else res)
		[]
		pars2


(** Intersect two list of attributes to provide a common context for
	extending a list of symbols.
	@param attrs1		First list of symbols.
	@param attrs2		Second list of symbols.
	@return				Intersection of both lists. *)
let intersect_attrs attrs1 attrs2 =
	let equal attr1 attr2 =
		match (attr1, attr2) with
		| (Irg.ATTR_EXPR (n1, _), Irg.ATTR_EXPR (n2, _))
		| (Irg.ATTR_LOC (n1, _), Irg.ATTR_LOC (n2, _))
		| (Irg.ATTR_STAT (n1, _), Irg.ATTR_STAT (n2, _)) when n1 = n2 -> true
		| _ -> false in
	let member_of attr attrs = List.exists (fun item -> equal attr item) attrs in
	List.fold_left (fun res attr -> if member_of attr attrs2 then attr::res else res) [] attrs1

%}

/* litterals */
%token<string>			ID
%token<Int32.t * int>	BIN_CONST
%token<Int64.t * int>	BIN_CONST_64
%token<Int32.t>			CARD_CONST
%token<Int64.t> 		CARD_CONST_64
%token<float>			FIXED_CONST
%token<string>			STRING_CONST

/* deprecated */
%token    DOLLAR

/* keywords */
%token    	ACTION
%token    	ALIAS
%token	  	ATTR
%token    	BOOL
%token		CANON
%token    	CARD
%token    	CASE
%token		COERCE
%token    	DEFAULT
%token		DO
%token    	ELSE
%token		ENDDO
%token    	ENDIF
%token    	ENUM
%token	  	ERROR
%token		EXTEND
%token    	FIX
%token    	FLOAT
%token		FOR
%token    	FORMAT
%token    	IF
%token		IN
%token	  	IMAGE
%token    	INITIALA
%token    	INT
%token<int>	LET
%token<int>	MEM
%token<int>	MODE
%token<int>	OP
%token<int>	REG
%token    	SWITCH
%token 	  	SYNTAX
%token    	THEN
%token<int>	TYPE
%token<int>	VAR
%token    	VOLATILE


/* hardware extension keywords */
%token    	PORTS
%token<int>	RESOURCE
%token    	USES

/* future use */
%token<int>	EXCEPTION
%token    	MACRO

/* operators */
%token	EOF
%token	NOT
%token	OR AND LEQ GEQ EQU NEQ
%token	LEFT_SHIFT RIGHT_SHIFT DOUBLE_STAR
%token	ROTATE_RIGHT ROTATE_LEFT DOUBLE_COLON DOUBLE_DOT
%token	BIT_LEFT BIT_RIGHT
%token	EQ EXCLAM PIPE CIRC AMPERS GT LT SHARP
%token	PLUS MINUS STAR SLASH PERCENT TILD COLON
%token	COMMA LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN SEMI DOT
%token	AROBAS

/* operator priority */
%right	EQ
%left	DOUBLE_COLON
%left	OR
%left	AND
%right	EXCLAM
%left	PIPE
%left	CIRC
%left	AMPERS
%left	EQU NEQ
%left	LT GT LEQ GEQ
%left	LEFT_SHIFT RIGHT_SHIFT ROTATE_LEFT ROTATE_RIGHT
%left	PLUS MINUS
%left	STAR SLASH PERCENT
%right	DOUBLE_STAR
%right	TILD
%left   BIT_LEFT BIT_RIGHT DOUBLE_DOT



%type <unit> top

%start top

%%

top:
	specs EOF		{ }
;

LocatedID:
	ID	{ Irg.add_pos $1 !Lexer.file !Lexer.line; $1 }

specs :
		MachineSpec		{   }
	|	specs MachineSpec	{   }
;

MachineSpec :
    LetDef 		{ Irg.add_symbol (fst $1) (snd $1) }
|   TypeSpec 		{ Sem.add_spec (fst $1) (snd $1) }
|   MemorySpec		{ Sem.add_spec (fst $1) (snd $1) }
|   RegisterSpec	{ Sem.add_spec (fst $1) (snd $1) }
|   VarSpec			{ Sem.add_spec (fst $1) (snd $1) }
|   ModeSpec		{ Sem.add_spec (fst $1) (snd $1); }
|   OpSpec			{ Sem.add_spec (fst $1) (snd $1); }
|   ResourceSpec	{ }
|   ExceptionSpec	{ }
|	ExtendSpec		{ }
| 	CanonSpec		{ Irg.add_symbol (fst $1) (snd $1); Irg.add_canon (fst $1) (snd $1) }



LetDef	:
	LET LocatedID EQ LetExpr			{  ($2, Sem.make_let $2 Irg.NO_TYPE $4) }
|	LET LocatedID COLON Type EQ LetExpr	{  ($2, Sem.make_let $2 $4 $6) }
;

ResourceSpec:
	RESOURCE ResourceList	{ $2 }
;

ResourceList:
	Resource			{ [$1] }
|	ResourceList COMMA Resource	{ $3::$1 }
;

Resource:
	LocatedID							{ Irg.add_symbol $1 (Irg.RES $1); $1 }
|	LocatedID LBRACK CARD_CONST RBRACK	{ Irg.add_symbol $1 (Irg.RES $1); $1 }
;

CanonSpec:
	CANON STRING_CONST LPAREN CanonParamList RPAREN
		{ ($2, Irg.CANON_DEF($2, Irg.CANON_FUNC, Irg.NO_TYPE, $4)) }
|	CANON Type STRING_CONST LPAREN CanonParamList RPAREN
		{ ($3, Irg.CANON_DEF($3, Irg.CANON_FUNC, $2, $5)) }
|	CANON STRING_CONST LPAREN RPAREN
		{ ($2, Irg.CANON_DEF($2, Irg.CANON_FUNC, Irg.NO_TYPE, [])) }
|	CANON Type STRING_CONST LPAREN RPAREN
		{ ($3, Irg.CANON_DEF($3, Irg.CANON_FUNC, $2, [])) }
|	CANON Type STRING_CONST
		{ ($3, Irg.CANON_DEF($3, Irg.CANON_CNST, $2, [])) }
;

CanonParamList:
	CanonParam						{ [$1] }
|	CanonParamList COMMA CanonParam	{ $3::$1 }
;

CanonParam:
	Type					{ $1 }
|	LocatedID COLON Type	{ $3 }
;

ExceptionSpec:
	EXCEPTION IdentifierList	{ $2 }
;

IdentifierList:
	LocatedID						{ [$1] }
|	IdentifierList COMMA LocatedID	{ $3::$1 }
;

TypeSpec:
	TYPE LocatedID EQ TypeExpr
		{ ($2, Irg.TYPE ($2, $4)) }
;

TypeExpr:
	BOOL
		{ Irg.BOOL }
|	INT LPAREN LetExpr RPAREN
		{ Irg.INT (Sem.to_int (Sem.eval_const $3)) }
|	CARD LPAREN LetExpr RPAREN
		{ Irg.CARD (Sem.to_int (Sem.eval_const $3)) }
|	FIX  LPAREN LetExpr COMMA LetExpr RPAREN
		{ Irg.FIX (
			Sem.to_int (Sem.eval_const $3),
			Sem.to_int (Sem.eval_const $5)) }
|	FLOAT LPAREN LetExpr COMMA LetExpr RPAREN
		{ Irg.FLOAT (
			Sem.to_int (Sem.eval_const $3),
			Sem.to_int (Sem.eval_const $5)) }
|	LBRACK LetExpr DOUBLE_DOT LetExpr RBRACK
		{
		let v1=Sem.to_int32 (Sem.eval_const $2)
		and v2=Sem.to_int32 (Sem.eval_const $4)
		in
		if ((Int32.compare v1 v2)<=0)
			then Irg.RANGE (v1,v2)
			else
				Irg.error (Irg.output [
					Irg.PTEXT "In a range type declaration, the first operand must be lesser than the second\n";
					Irg.PTEXT (Printf.sprintf "Value of the first operand : %d\n Value of the second operand : %d"
						(Int32.to_int v1) (Int32.to_int v2))])
		 }
|	ENUM LPAREN ValueList RPAREN
		{ Irg.ENUM (Sem.uniq (List.sort Int32.compare $3)) }
;

ValueList:	Value						{ $1 }
|			ValueList COMMA Value		{ $1 @ $3 }
;

Value:		LetExpr						{ [Sem.to_int32 (Sem.eval_const $1)] }
|			LetExpr DOUBLE_DOT LetExpr	{ Sem.enum_values (Sem.to_int32 (Sem.eval_const $1)) (Sem.to_int32 (Sem.eval_const $3)) } 
;

LetExpr:
	Expr 	{ $1 }
;

MemorySpec:
	MEM LocatedID LBRACK MemPart RBRACK OptionalMemAttrDefList
		{ ($2, Sem.check_alias (Irg.MEM ($2, fst $4, snd $4, $6))) }
;

RegisterSpec:
	REG LocatedID LBRACK RegPart RBRACK OptionalMemAttrDefList
		{ ($2, Sem.check_alias (Irg.REG ($2, fst $4, snd $4, $6))) }
;

VarSpec:
	VAR LocatedID LBRACK RegPart RBRACK OptionalMemAttrDefList
		{ ($2, Irg.VAR ($2, fst $4, snd $4, $6)) }
;

MemPart:
	LetExpr COMMA Type  	{ Sem.to_int (Sem.eval_const $1), $3 }
|	LetExpr 		{ Sem.to_int (Sem.eval_const $1), Irg.INT 8 }
;

RegPart:
	LetExpr COMMA Type  	{ Sem.to_int (Sem.eval_const $1), $3 }
|	Type			{ 1, $1 }
;

Type :
	TypeExpr		{ $1 }
|	ID			{ Sem.type_from_id $1 }
;

OptionalMemAttrDefList:
	/* empty */		{ [] }
|	MemAttrDefList		{ $1 }
;

MemAttrDefList:
	MemAttrDef			{ [$1] }
|	MemAttrDefList MemAttrDef	{ $2::$1 }
;

MemAttrDef:
	VOLATILE EQ LetExpr
		{ Irg.ATTR_EXPR ("volatile", Irg.CONST (Irg.NO_TYPE, (Sem.eval_const $3))) }
|	INITIALA EQ LetExpr
		{ Irg.ATTR_EXPR ("init", Irg.CONST (Irg.NO_TYPE, Sem.eval_const $3)) }
|	PORTS EQ CARD_CONST COMMA CARD_CONST
		{ Irg.ATTR_USES }
|	USES EQ UsesDef
		{ Irg.ATTR_USES }
|	ALIAS EQ MemLocation
		{ Irg.ATTR_LOC ("alias", $3) }
|	ID EQ Expr
		{ Irg.ATTR_EXPR ($1, $3) }
|	ID EQ LBRACE Sequence RBRACE
		{ let r = Irg.ATTR_STAT ($1, $4) in Sem.reset_local(); r }
;

MemLocation:
	MemLocBase
		{ Irg.LOC_REF (Sem.get_loc_ref_type (fst $1), fst $1, snd $1, Irg.NONE, Irg.NONE) }
|	MemLocBase BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{  Irg.LOC_REF (Sem.get_loc_ref_type (fst $1), fst $1, snd $1, $3, $5) }
;

MemLocBase:
	ID
		{ ($1, Irg.NONE) }
|	ID LBRACK Expr RBRACK
		{ ($1, Irg.CONST (Irg.CARD(32), Sem.eval_const $3)) }
;

ModeSpec:
	MODE LocatedID LPAREN ParamList RPAREN OptionalModeExpr  AttrDefList
		{
			Sem.check_image $2 $4;
			Irg.param_unstack $4;
			Irg.attr_unstack $7;
			($2, Irg.AND_MODE ($2, $4, $6, $7))
		}
|	MODE LocatedID EQ Identifier_Or_List
		{ $2, Irg.OR_MODE ($2, $4) }
;



OptionalModeExpr :
	/* empty */	{ Irg.NONE }
|	EQ Expr		{ $2 }
;

OpSpec:
	OP LocatedID LPAREN ParamList RPAREN AttrDefList
		{
			Sem.check_image $2 $4;
			Irg.param_unstack $4;
			Irg.attr_unstack $6;			
			($2, Irg.AND_OP ($2, $4, $6))
		}
|	OP LocatedID EQ Identifier_Or_List
		{ $2, Irg.OR_OP ($2, $4) }
|	OP LocatedID error
		{ raise (Irg.SyntaxError "missing '=' or '('") }
;


/* external attribute */
ExtendSpec:
	ExtendHeader AttrDefList
		{
			let (syms, pars, cattrs) = $1 in
			let extend_spec s =
				match s with
				| Irg.AND_MODE (id, pars, expr, attrs) ->
					Irg.rm_symbol id;
					Irg.add_symbol id (Irg.AND_MODE (id, pars, expr, attrs @ $2))
				| Irg.AND_OP (id, pars, attrs) ->
					Irg.rm_symbol id;
					Irg.add_symbol id (Irg.AND_OP (id, pars, attrs @ $2));
				| _ -> () in
			Irg.param_unstack pars;
			Irg.attr_unstack $2;
			Irg.attr_unstack cattrs;
			List.iter extend_spec syms
		}
;


ExtendHeader:
	EXTEND ExtendIDList	{ let (_, pars, attrs) = $2 in Irg.attr_stack attrs; Irg.param_stack pars; $2 }
;


ExtendIDList:
	ID
		{ let (sym, pars, attrs) = get_spec_extend $1 in ([sym], pars, attrs) }
|	ExtendIDList COMMA ID
		{
			let (sym, spars, sattrs) = get_spec_extend $3 in
			let (syms, pars, attrs) = $1 in
			(sym::syms, intersect_params pars spars, intersect_attrs sattrs attrs)
		}
;
/**/


Identifier_Or_List:
	ID								{ [$1] }
|	Identifier_Or_List  PIPE  ID 	{ $3::$1 }
;

ParamList:
	/* empty */						{ [] }
|	ParamListPart					{ Irg.add_param $1; [$1] }
|	ParamList COMMA ParamListPart	{ Irg.add_param $3; $3::$1 }
;

ParamListPart:
	ID COLON ParaType 				{ Sem.check_param_exists $1; ($1, $3) }
;

ParaType:
	TypeExpr	{ Irg.TYPE_EXPR $1 }
|	ID		{ Irg.TYPE_ID $1 }
;

AttrDefList:
	/* empty */			{ [] }
|	NOAttrDefList			{ $1 }
;

NOAttrDefList:
|	AttrDef					{ Irg.add_attr $1; [$1] }
|	NOAttrDefList AttrDef	{ Irg.add_attr $2; $2::$1 }
;

AttrDef :/* It is not possible to check if the ID and the attributes exits because this is used for op, in wich they can be defined later.
		   So it must be checked at the end of parsing */
	ID EQ Expr
		{ Irg.ATTR_EXPR ($1, $3) }
|	ID EQ LBRACE Sequence RBRACE
		{ let r = Irg.ATTR_STAT ($1, $4) in Sem.reset_local (); r }
|	SYNTAX EQ AttrExpr
		{ Irg.ATTR_EXPR  ("syntax", (Sem.change_string_dependences "syntax" $3)) }
|	IMAGE EQ AttrExpr
		{ Irg.ATTR_EXPR  ("image", (Sem.change_string_dependences "image" $3)) }
|	ACTION EQ LBRACE Sequence RBRACE
		{ let r = Irg.ATTR_STAT ("action", $4) in Sem.reset_local (); r }
|	USES EQ UsesDef
		{ Irg.ATTR_USES }
|	ID EQ error
		{ raise (Irg.SyntaxError "attributes only accept expressions, { } actions or use clauses.") }
|	ID error
 		{ raise (Irg.SyntaxError "missing '=' in attribute definition") }
;

AttrExpr :
|	Expr
		{ eline $1 }
;

/* format() now accepts any expression but some restriction may
	apply according the defined attribute.

FormatIdlist:
	Expr						{ [$1] }
|	FormatIdlist  COMMA Expr	{ $3::$1 }
;

FormatId:
	ID
		{
			if Irg.is_defined $1 then Irg.REF $1
			else raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID DOT IMAGE
		{
		if Irg.is_defined $1
		then
			 Irg.FIELDOF (Irg.STRING , $1, "image")
		else
			raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}

|	ID DOT IMAGE BIT_LEFT CARD_CONST DOUBLE_DOT CARD_CONST BIT_RIGHT
		{
		if Irg.is_defined $1
			then
				Irg.BITFIELD (Irg.STRING,Irg.FIELDOF (Irg.NO_TYPE, $1, "image"),
					Irg.CONST ((Irg.CARD 32),(Irg.CARD_CONST $5)),
					Irg.CONST ((Irg.CARD 32),(Irg.CARD_CONST $7)))
			else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID DOT SYNTAX
		{
		if Irg.is_defined $1
			then
				Irg.FIELDOF (Irg.STRING, $1, "syntax")
			else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
 we should autorise constant parameters in a format expression
	correct type will be checked in Sem
|	Constant
		{ eline (Irg.CONST (fst $1, snd $1)) }

|	DOLLAR PLUS ID
		{ }*/
;



Sequence:
	/* empty */ { Irg.NOP }
|	StatementList SEMI { $1 }
;

StatementList:
	Statement
		{ line $1 }
|	StatementList SEMI Statement
		{ handle_stat (fun _ -> Irg.SEQ ($1, line $3)) }
;

Statement:
	/* empty */
		{ Irg.NOP }
|	ACTION
		{ Irg.EVAL ("", "action") }
|	ID
		{ Irg.EVAL ("", $1) }
|	ID LPAREN
		{ raise (Irg.SyntaxError (Printf.sprintf "unreduced macro '%s'" $1)) }
|	ID DOT ACTION
		{ Irg.EVAL ($1, "action")  }
|	ID DOT ID
		{ Irg.EVAL ($1, $3) }
|	Location EQ Expr
		{ handle_stat (fun _ -> Sem.make_set $1 $3) }
|	ConditionalStatement
		{ $1 }
|	STRING_CONST LPAREN ArgList RPAREN
		{ Sem.test_canonical $1; Sem.build_canonical_stat $1 (List.rev $3) }
|	ERROR LPAREN STRING_CONST RPAREN
		{ handle_stat (fun _ -> Irg.ERROR $3) }
|	LET ID EQ Expr
		{ handle_stat (fun _ -> Sem.make_local $2 $4) }
|	LET ID COLON Type EQ Expr
		{ handle_stat (fun _ -> Sem.make_typed_local $2 $4 $6) }
|	ForHeader DO Sequence ENDDO
		{ handle_stat (fun _ -> Sem.make_for $1 $3) }
;

ForHeader: 
|	FOR ID IN Expr DOUBLE_DOT Expr
		{ Sem.prepare_for $2 Irg.NO_TYPE $4 $6 }
|	FOR ID COLON Type IN Expr DOUBLE_DOT Expr
		{ Sem.prepare_for $2 $4 $6 $8 }
;

ArgList :
	/* empty */	{ [] }
|	Expr	{ [$1] }
|	ArgList COMMA Expr 	{ $3::$1 }
;

Opt_Bit_Optr :
	/* empty */
		{ None }
|	BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ None }
;

Location :
	ID
		{ Sem.make_access_loc $1 Irg.NONE Irg.NONE Irg.NONE } 
|	ID BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ Sem.make_access_loc $1 Irg.NONE $3 $5 }
|	ID LBRACK Expr RBRACK
		{ Sem.make_access_loc $1 $3 Irg.NONE Irg.NONE }
|	ID LBRACK Expr RBRACK BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ Sem.make_access_loc $1 $3 $6 $8 }
|	Location DOUBLE_COLON Location
		{ Sem.make_concat_loc $1 $3 }
|	LPAREN Location RPAREN
 		{ $2 }
;


ConditionalStatement:
	IF Expr THEN Sequence OptionalElse ENDIF
		{ Irg.IF_STAT ($2, $4, $5) }
|	IF Expr THEN Sequence OptionalElse RBRACE
		{ raise (Irg.SyntaxError "missing endif") }
|	SWITCH LPAREN Expr RPAREN LBRACE CaseBody RBRACE
		{Irg.SWITCH_STAT ($3, fst $6, snd $6)}

OptionalElse:
	/* empty */	{ Irg.NOP }
|	ELSE Sequence	{ $2 }
;


CaseBody:
	CaseList	{ ($1,Irg.NOP) }
|	OptCaseList Default OptCaseList { ($1@$3,$2) }

OptCaseList:
	/* empty */	{ [] }
|	CaseList	{ $1 }
;

CaseList:
	  CaseStat		{ [$1]	}
	| CaseList CaseStat	{ $2::$1 };

CaseStat:
	CASE Expr COLON Sequence { ($2,$4) }
;

Default:
	DEFAULT COLON Sequence {$3}
;


Expr:
	COERCE LPAREN Type COMMA Expr RPAREN
		{ handle_expr (fun _ -> Sem.make_coerce $3 $5) }
|	COERCE error
		{ syntax_error "syntax error in coerce expression" }
|	FORMAT LPAREN STRING_CONST COMMA ArgList RPAREN
		{ eline (Sem.build_format $3 $5) }
|	FORMAT error
		{ syntax_error "syntax error in format expression" }
|	STRING_CONST LPAREN ArgList RPAREN
		{ Sem.test_canonical $1; eline (Sem.build_canonical_expr $1 (List.rev $3)) }
|	ID DOT SYNTAX
		{	if Irg.is_defined $1
			then eline (Irg.FIELDOF (Irg.STRING, $1,"syntax"))
			else error (Irg.asis (Printf.sprintf "the keyword %s is undefined\n" $1)) }
|	ID DOT IMAGE
		{	if Irg.is_defined $1
			then eline (Irg.FIELDOF (Irg.STRING, $1,"image"))
			else error (Irg.asis (Printf.sprintf "the keyword %s is undefined\n" $1)) }
|	ID DOT ID
		{ eline (Irg.FIELDOF(Sem.type_of_field $1 $3, $1, $3)) }
|	Expr DOUBLE_COLON Expr
		{
			eline (Sem.get_binop $1 $3 Irg.CONCAT)
		}
|	ID
		{ handle_expr (fun _ -> Sem.make_ref (Sem.unalias_local $1)) }
|	ID LPAREN
		{ raise (Irg.SyntaxError "unreduced macro here") }
|	ID LBRACK Expr RBRACK
		{
		let id = Sem.unalias_local $1 in
		if Irg.is_defined id then
			if (Sem.is_location id) || (Sem.is_loc_spe id)  || (Sem.is_loc_mode id)
				then
					eline (Irg.ITEMOF ((Sem.get_type_ident id), id, $3))
				else
					Irg.error (Irg.output [Irg.PTEXT $1; Irg.PTEXT " is not a valid location: type is "])
		else Irg.error (Irg.asis (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID LBRACK error
		{ syntax_error "unclosed bracket expression" }
|	Expr BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ handle_expr (fun _ -> Sem.make_bitfield $1 $3 $5) }
|	Expr PLUS Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.ADD) }
|	Expr MINUS Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.SUB) }
|	Expr STAR Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.MUL) }
|	Expr SLASH Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.DIV) }
|	Expr PERCENT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.MOD) }
|	Expr DOUBLE_STAR Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.EXP) }
|	Expr LEFT_SHIFT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.LSHIFT) }
|	Expr RIGHT_SHIFT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.RSHIFT) }
|	Expr ROTATE_LEFT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.LROTATE) }
|	Expr ROTATE_RIGHT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.RROTATE) }
|	Expr LT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.LT) }
|	Expr GT Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.GT) }
|	Expr LEQ Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.LE) }
|	Expr GEQ Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.GE) }
|	Expr EQU Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.EQ) }
|	Expr NEQ Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.NE) }
|	Expr AMPERS Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.BIN_AND) }
|	Expr CIRC Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.BIN_XOR) }
|	Expr PIPE Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.BIN_OR) }
|	EXCLAM Expr
		{ handle_expr (fun _ -> Sem.get_unop $2 Irg.NOT) }
|	TILD Expr
		{ handle_expr (fun _ -> Sem.get_unop $2 Irg.BIN_NOT) }
|	PLUS Expr %prec TILD
		{ $2 }
|	MINUS Expr %prec TILD
		{ handle_expr (fun _ -> Sem.get_unop $2 Irg.NEG) }
|	Expr AND Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.AND) }
|	Expr OR Expr
		{ handle_expr (fun _ -> Sem.get_binop $1 $3 Irg.OR) }
|	LPAREN Expr RPAREN
		{ $2 }
|	LPAREN error
		{ syntax_error "no expression after '('" }
|	LPAREN Expr error
		{ syntax_error "unclosed '('" }
|	IF Expr THEN Expr ELSE Expr ENDIF
		{ handle_expr (fun _ -> Sem.make_if_expr $2 $4 $6) }
|	IF error
		{ syntax_error "malformed if expression" }
|	SWITCH LPAREN Expr RPAREN LBRACE CaseExprBody RBRACE
		{ handle_expr (fun _ -> Sem.make_switch_expr $3 (fst $6) (snd $6)) }
|	SWITCH error
		{ syntax_error "malformed switch expression" }
|	Constant
		{ eline (Irg.CONST (fst $1, snd $1)) }
;


Constant :
	FIXED_CONST
		{ (Irg.FLOAT(23, 9), Irg.FIXED_CONST  $1) }
		/* changed for convenience. Avoid typing problem between immediates values and const */
|	CARD_CONST
		{ (Irg.CARD 32, Irg.CARD_CONST $1) }
|	CARD_CONST_64
		{ (Irg.CARD 64, Irg.CARD_CONST_64 $1) }
|	BIN_CONST
		{ (Irg.CARD (snd $1), Irg.CARD_CONST (fst $1)) }
|	BIN_CONST_64
		{ (Irg.CARD (snd $1), Irg.CARD_CONST_64 (fst $1)) }
|	STRING_CONST
		{
			if  (Irg.is_defined_canon $1) then
				(Sem.test_canonical $1;
				let e = Irg.get_canon $1 in
					(e.Irg.type_res, Irg.CANON($1)))
			else
				(Irg.STRING, Irg.STRING_CONST($1))
		}
;




Bit_Expr :
	ID
		{ Sem.make_ref $1 }
|	MINUS Bit_Expr
		{ eline (Sem.get_unop $2 Irg.NEG) }
|	PLUS Bit_Expr
		{ $2 }
|	TILD Bit_Expr
		{ eline (Sem.get_unop $2 Irg.BIN_NOT) }
|	Bit_Expr PLUS Bit_Expr
		{ eline (Sem.get_binop $1 $3 Irg.ADD) }
|	Bit_Expr MINUS Bit_Expr
		{ eline (Sem.get_binop $1 $3 Irg.SUB) }
|	Bit_Expr STAR Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.MUL) }
|	Bit_Expr SLASH Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.DIV) }
|	Bit_Expr PERCENT Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.MOD) }
|	Bit_Expr  DOUBLE_STAR Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.EXP) }
|	Bit_Expr AMPERS Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.BIN_AND) }
|	Bit_Expr PIPE Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.BIN_OR) }
|	Bit_Expr CIRC Bit_Expr
		{ eline(Sem.get_binop $1 $3 Irg.BIN_XOR) }
|	LPAREN Bit_Expr RPAREN
		{ $2 }
|	FIXED_CONST
		{ eline (Irg.CONST (Irg.FIX(8,24),Irg.FIXED_CONST $1)) }
|	CARD_CONST
		{ eline (Irg.CONST (Irg.CARD 32,Irg.CARD_CONST $1)) }
|	STRING_CONST
		{
			if  (Irg.is_defined_canon $1) then
				(Sem.test_canonical $1;
				let e = Irg.get_canon $1 in
					eline (Irg.CONST (e.Irg.type_res, Irg.CANON($1))))
			else
				eline (Irg.CONST (Irg.STRING, Irg.STRING_CONST($1)))
		}
;


CaseExprBody:
	CaseExprList { ($1,Irg.NONE) }
|	OptCaseExprList ExprDefault OptCaseExprList { ($1@$3,$2) }
;

OptCaseExprList:
	/* empty*/ { [] }
|	CaseExprList { $1 }
;

CaseExprList:
	CaseExprStat { [$1] }
|	CaseExprList   CaseExprStat { $2::$1 }
;

CaseExprStat:
	CASE Expr COLON Expr { ($2,$4) }
;

ExprDefault:
	DEFAULT COLON Expr { $3 }
;



/* UNUSED */
//
//OptionalElseExpr:
//	/* empty */ { Irg.NONE }
//|	ELSE Expr { $2 }
//;



UsesDef:
	UsesOrSequence { }
|	UsesDef COMMA UsesOrSequence { }
;

UsesOrSequence:
    UsesIfAtom { }
|	UsesOrSequence PIPE UsesIfAtom { }
;

UsesIfAtom:
	UsesIndirectAtom { }
|	IF Expr THEN UsesIfAtom OptionalElseAtom ENDIF { }
;

OptionalElseAtom :
	/* empty */ { }
|	ELSE UsesIfAtom { }
;

UsesIndirectAtom:
	UsesCondAtom { }
|	ID DOT USES 		{ }
|	LPAREN UsesDef RPAREN { }
|	UsesLocationList AND  ID  DOT  USES { }
|	UsesLocationList AND  LPAREN  UsesDef  RPAREN { }
;

UsesCondAtom:
	UsesAndAtom { }
|	LBRACE Expr RBRACE UsesAndAtom { }
;

UsesAndAtom :
	 UsesLocationList  UsesActionList { }
;

UsesActionList :
	/* empty */		{ }
|	ActionTimeList OptionalAction { }
|	TimeActionList  OptionalTime { }
;

ActionTimeList :
	SHARP LBRACE Expr RBRACE { }
|	ActionTimeList  COLON UsesActionAttr SHARP LBRACE Expr RBRACE { }
;

TimeActionList :
	COLON UsesActionAttr { }
|	TimeActionList  SHARP LBRACE Expr RBRACE COLON UsesActionAttr { }
;

OptionalAction :
	/* empty */ { }
|	COLON UsesActionAttr { }
;

OptionalTime :
	/* empty */ { }
|	SHARP LBRACE Expr RBRACE { }
;

UsesActionAttr:
	ID			{ }
|	ACTION    { }
;

UsesLocationList :
	UsesLocation	{ }
|	UsesLocationList  AMPERS  UsesLocation { }
;

UsesLocation :
	ID Opt_Bit_Optr { }
|	ID LBRACK Expr RBRACK Opt_SecDim Opt_Bit_Optr { }
;

Opt_SecDim :
	/* empty */		{ }
|	LBRACK RBRACK	{ }
;
