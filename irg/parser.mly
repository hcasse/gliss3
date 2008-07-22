/*
 * $Id: parser.mly,v 1.6 2008/07/22 09:53:49 jorquera Exp $
 * Copyright (c) 2007, IRIT - UPS <casse@irit.fr>
 *
 * Parser of OGEP.
 */

%{

%}

%token<string>	ID
%token<Int32.t>	CARD_CONST
%token<Int64.t> CARD_CONST_64
%token<float>	FIXED_CONST
%token<string>	STRING_CONST
%token<string> STRING_VALUE

%token    EOF
%token    DOLLAR
%token    MEM
%token    VOLATILE
%token    ALIAS
%token    PORTS
%token    COERCE
%token	  ERROR
%token    TYPE
%token    LET
%token    MACRO
%token    IF
%token    THEN
%token    ELSE
%token    ENDIF
%token    SWITCH
%token    CASE
%token    DEFAULT
%token    BOOL
%token    INT
%token    CARD
%token    FIX
%token    FLOAT
%token    ENUM
%token    MODE
%token    REG
%token    VAR
%token    OP
%token    NOT
%token    FORMAT
%token    LIST
%token    NOP
%token    USES
%token 	  SYNTAX
%token	  IMAGE
%token    ACTION
%token    INITIALA
%token    RESOURCE
%token    EXCEPTION
%token	  BINARY_CONST
%token	  HEX_CONST
%token    OR AND LEQ GEQ EQU NEQ
%token    LEFT_SHIFT RIGHT_SHIFT DOUBLE_STAR
%token    ROTATE_RIGHT ROTATE_LEFT DOUBLE_COLON DOUBLE_DOT
%token    BIT_LEFT BIT_RIGHT
%token    EQ EXCLAM PIPE CIRC AMPERS GT LT SHARP
%token    PLUS MINUS STAR SLASH PERCENT TILD COLON
%token	  COMMA LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN SEMI DOT

/* ADDED*/
%token MACRO

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
	specs EOF		{ print_string "Start Symbol reduced, end of recognition\n########################\n" }
;

specs :
		MachineSpec					{  }
	|	specs MachineSpec	{   }
;

MachineSpec :
	LetDef 			{ Irg.add_symbol (fst $1) (snd $1)}
|   TypeSpec 		{ Irg.add_symbol (fst $1) (snd $1) }
|   MemorySpec		{ Irg.add_symbol (fst $1) (snd $1) }
|   RegisterSpec	{ Irg.add_symbol (fst $1) (snd $1) }
|   VarSpec			{ Irg.add_symbol (fst $1) (snd $1) }
|   ModeSpec		{ (Irg.add_symbol (fst $1) (snd $1);

			(* Remove parameters from the symbol table *)
			match (snd $1) with
				Irg.AND_MODE (_,l,_,_)->Irg.depiler_param l
				|_-> ());

			(**)
			Irg.print_spec (snd $1);
			()
			(**)

			 }
|   OpSpec			{ Irg.add_symbol (fst $1) (snd $1);

					(* Remove parameters from the symbol table *)
					(match (snd $1) with
						Irg.AND_OP (_,l,_)->Irg.depiler_param l
						|_-> ());

					(**)
					Irg.print_spec (snd $1);
					()
					(**)
				}
|   ResourceSpec	{ }
|   ExceptionSpec	{ }

/* ADDED*/
|   MacroSpec {}
;

/*** MACRO DEF ***/

MacroSpec :
	MACRO ID ParamList EQ Statement	{}
|	MACRO ID ParamList EQ Expr	{}

/*** END MACRO DEF ***/





LetDef	:
	LET ID EQ LetExpr
		{ ($2(* ID *), Irg.LET ($2(* ID *), Sem.eval_const $4(* LetExpr *))) }
;

ResourceSpec:
	RESOURCE ResourceList	{ }
;

ResourceList:
	Resource					{ }
|	ResourceList COMMA Resource	{ }
;

Resource:
	ID
		{ Irg.add_symbol $1 (Irg.RES $1) }
|	ID LBRACK CARD_CONST RBRACK
		{ Irg.add_symbol $1 (Irg.RES $1) }
;	
    
ExceptionSpec:
	EXCEPTION IdentifierList
		{ List.iter (fun id -> Irg.add_symbol id (Irg.EXN id)) $2 }
;

IdentifierList:
	ID							{ [$1] }
|	IdentifierList COMMA ID		{ $3::$1 }
;

TypeSpec:
	TYPE ID EQ TypeExpr	{ 	Irg.complete_incomplete_enum_poss $2;	(* needed for enums *)
					($2, Irg.TYPE ($2, $4)) 
				}
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
		if ((Int32.compare v1 v2)<0)		(* A CHANGER en <= ??*)
			then Irg.RANGE (v1,v2)
			else 
				let dsp=fun _->(
					 Printf.printf "Value of the first operand : %d\n Value of the second operand : %d" (Int32.to_int v1) (Int32.to_int v2)
					)
				in
				raise (Sem.SemErrorWithFun ("In a range type declaration, the first operand must be lesser than the second", dsp))
		 }
|	ENUM LPAREN IdentifierList RPAREN
		{
			(*let i = List.fold_right (fun id i -> Irg.add_symbol id 
				(Irg.ENUM_POSS (id," ",(Int32.of_int i),false) ); i + 1)
				$3 0 in
			Irg.CARD (int_of_float (ceil ((log (float i)) /. (log 2.))))*)

			let rec temp l i= match l with
				[]->()
				|e::l-> Irg.add_symbol e (Irg.ENUM_POSS (e," ",(Int32.of_int i),false)); temp l (i+1)
			in
			temp $3 0;
			Irg.ENUM $3
		}
;

LetExpr:
	Expr 	{ $1 }	
;

MemorySpec:
	MEM ID LBRACK MemPart RBRACK OptionalMemAttrDefList
		{ $2, Irg.MEM ($2, fst $4, snd $4, $6) }
;

RegisterSpec:
	REG ID LBRACK RegPart RBRACK OptionalMemAttrDefList
		{ $2, Irg.REG ($2, fst $4, snd $4, $6) }
;

VarSpec:
	VAR ID LBRACK RegPart RBRACK
		{ $2, Irg.VAR ($2, fst $4, snd $4) }
;

MemPart:
	LetExpr COMMA Type  	{ Sem.to_int (Sem.eval_const $1), $3 }
|	LetExpr 				{ Sem.to_int (Sem.eval_const $1), Irg.INT 8 }
;

RegPart:
	LetExpr COMMA Type  	{ Sem.to_int (Sem.eval_const $1), $3 }
|	Type					{ 1, $1 }
;

Type :
	TypeExpr				{ $1 }
|	ID						{ Sem.type_from_id $1 }
;

OptionalMemAttrDefList:
	/* empty */				{ [] }
|	MemAttrDefList			{ $1 }
;

MemAttrDefList: 
	MemAttrDef					{ [$1] }
|	MemAttrDefList MemAttrDef	{ $2::$1 }
;

MemAttrDef:
	VOLATILE EQ LetExpr
		{ Irg.VOLATILE (Sem.to_int (Sem.eval_const $3)) }
|	PORTS EQ CARD_CONST COMMA CARD_CONST
		{ Irg.PORTS (Int32.to_int $3, Int32.to_int $3) }
|	ALIAS EQ MemLocation
		{ Irg.ALIAS $3 }
|	INITIALA EQ LetExpr
		{ Irg.INIT (Sem.eval_const $3) }
|	USES EQ UsesDef
		{ Irg.USES }
;

MemLocation:
	MemLocBase
		{ $1 }
|	MemLocBase BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ Irg.LOC_BITFIELD ($1, $3, $5) }
;

MemLocBase:
	ID
		{ Irg.LOC_REF $1 }
|	ID LBRACK Expr RBRACK
		{ Irg.LOC_ITEMOF (Irg.LOC_REF $1, $3) }
;

ModeSpec:
	MODE ID LPAREN ParamList RPAREN OptionalModeExpr  AttrDefList
		{ $2, Irg.AND_MODE ($2, $4, $6, $7) }
|	MODE ID EQ Identifier_Or_List
		{ $2, Irg.OR_MODE ($2, $4)  }
;



OptionalModeExpr :
	/* empty */	{ Irg.NONE }
|	EQ Expr		{ $2 }
;

OpSpec: 
	OP ID LPAREN ParamList RPAREN AttrDefList
		{ $2, Irg.AND_OP ($2, $4, $6) }
|	OP ID EQ Identifier_Or_List
		{ $2, Irg.OR_OP ($2, $4) }
		
;

Identifier_Or_List: 
	ID								{ [$1] }
|	Identifier_Or_List  PIPE  ID 	{ $3::$1 }
;

ParamList:
	/* empty */			{ [] }
|	ParamListPart			{Irg.add_param $1; [$1] }
|	ParamList COMMA ParamListPart	{Irg.add_param $3; $3::$1 }
;

ParamListPart: 
	ID COLON ParaType 			{ ($1, $3) }
;

ParaType:
	TypeExpr	{ Irg.TYPE_EXPR $1 }
|	ID		{ Irg.TYPE_ID $1 }
;

AttrDefList:	
	/* empty */		{ [] }
|	AttrDefList AttrDef	{ $2::$1 }
; 

AttrDef :
	ID EQ Expr
		{ Irg.ATTR_EXPR ($1, $3) }
|	ID EQ LBRACE Sequence RBRACE
		{ Irg.ATTR_STAT ($1, $4) }
|	SYNTAX EQ AttrExpr
		{Irg.ATTR_EXPR ("syntax", $3) }
|	IMAGE EQ AttrExpr
		{ Irg.ATTR_EXPR ("image", $3) }
|	ACTION EQ LBRACE Sequence RBRACE
		{ Irg.ATTR_STAT ("action", $4) }
|	USES EQ UsesDef
		{ Irg.ATTR_USES }
;

AttrExpr :
	ID DOT SYNTAX
		{ Irg.FIELDOF (Irg.STRING, Irg.REF $1, "syntax") }
|	ID DOT IMAGE
		{ Irg.FIELDOF (Irg.STRING,Irg.REF $1, "image") }
|	STRING_CONST
		{ Irg.CONST (Irg.STRING,Irg.STRING_CONST $1) }
|	FORMAT LPAREN STRING_CONST  COMMA  FormatIdlist RPAREN
		{ 
			Sem.build_format $3 $5
			(* Irg.FORMAT ($3, $5) *)
		}
;

FormatIdlist: 
	FormatId						{ [$1] }
|	FormatIdlist  COMMA FormatId	{ $3::$1 }
;

FormatId: 	/* Verifier que ID est def et que les attrs existent -> Impossible car ca sert pour les op qui ne sont pas forcement definites */
	ID
		{
		if Irg.is_defined $1 
		then
			 Irg.REF $1
		else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID DOT IMAGE
		{ 
		if Irg.is_defined $1 
		then
			 Irg.FIELDOF (Irg.STRING ,Irg.REF $1, "image") 		(* TYPE A CHANGER *)
		else
			raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
			
|	ID DOT IMAGE BIT_LEFT CARD_CONST DOUBLE_DOT CARD_CONST BIT_RIGHT
		{ 
		if Irg.is_defined $1 
			then
				Irg.BITFIELD (Irg.STRING,Irg.FIELDOF (Irg.NO_TYPE, Irg.REF $1, "image"), 	
					Irg.CONST ((Irg.CARD 32),(Irg.CARD_CONST $5)),
					Irg.CONST ((Irg.CARD 32),(Irg.CARD_CONST $7))) 	(* TYPE A CHANGER *)
			else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID DOT SYNTAX
		{ 
		if Irg.is_defined $1 
		then
			Irg.FIELDOF (Irg.STRING,Irg.REF $1, "syntax") 	(* TYPE A CHANGER *)
		else
			raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
			
/*|	DOLLAR PLUS ID
		{ }*/
;



/* UNUSED */
OptBitSelect:
	/* empty */	{ }
|	BIT_LEFT CARD_CONST DOUBLE_DOT CARD_CONST BIT_RIGHT { }
;

Sequence:
	/* empty */ { Irg.NOP }
|	StatementList SEMI { $1 }
;

StatementList: 
	Statement { $1 }
|	StatementList SEMI Statement { Irg.SEQ ($1, $3) }
;

Statement: 			/* A changer (pour les ATTR_STAT) */
	/* empty */
		{ Irg.NOP }
|	ACTION
		{ Irg.EVAL "action" }	/* Necessite une verification mais l'op n'est pas encore reduite...*/
|	ID
		{
		(*if Irg.is_defined $1 
			then
				Irg.EVAL $1
			else
				 raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))*)
		
		Irg.EVAL $1
		}
|	ID DOT ACTION
		{ Irg.EVALIND ($1, "action")  }	/* Necessite une verification mais l'op n'est pas encore reduite...*/
|	ID DOT ID
		{ Irg.EVALIND ($1, $3) }	/* Necessite une verification mais l'op n'est pas encore reduite...*/
|	Location EQ Expr
		{ 
			if (Sem.is_setspe $1)
				then Irg.SETSPE ($1,$3)
				else Irg.SET ($1, $3) 
		}
|	ConditionalStatement
		{ $1 }
|	STRING_CONST LPAREN ArgList RPAREN
		{ Irg.CANON_STAT ($1, $3) }
|	ERROR LPAREN STRING_CONST RPAREN
		{ Irg.ERROR $3 }
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
		{ if (Sem.is_location $1) || (Sem.is_loc_spe $1) || (Sem.is_loc_mode $1)
			then	Irg.LOC_REF $1 
			else	
				let dsp=fun _->(
					print_string "Type : ";
					Irg.print_spec (Irg.get_symbol $1)
					)
				in
				raise (Sem.SemErrorWithFun ((Printf.sprintf "%s is not a valid location" $1),dsp))
		}

|	ID BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ 
			if (Sem.is_location $1) || (Sem.is_loc_spe $1)	 || (Sem.is_loc_mode $1) 
			then
				Irg.LOC_BITFIELD (Irg.LOC_REF $1, $3, $5)
			else 
				let dsp = fun _->(
						print_string "Type : ";
						Irg.print_spec (Irg.get_symbol $1)
						)
				in
				raise (Sem.SemErrorWithFun ((Printf.sprintf "%s is not a valid location" $1),dsp))
		}
|	ID LBRACK Expr RBRACK
		{ 
			if (Sem.is_location $1) || (Sem.is_loc_spe $1) (* || (Sem.is_loc_mode $1) *)
			then
				Irg.LOC_ITEMOF (Irg.LOC_REF $1, $3) 
			else 
				let dsp = fun _->(
						print_string "Type : ";
						Irg.print_spec (Irg.get_symbol $1)
						)
				in
				raise (Sem.SemErrorWithFun ((Printf.sprintf "%s is not a valid location" $1),dsp))
		}
|	ID LBRACK Expr RBRACK BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT
		{ 
			if (Sem.is_location $1) || (Sem.is_loc_spe $1) (* || (Sem.is_loc_mode $1) *)
			then
				Irg.LOC_BITFIELD (Irg.LOC_ITEMOF (Irg.LOC_REF $1, $3), $6, $8) 
			else 
				let dsp = fun _->(
						print_string "Type : ";
						Irg.print_spec (Irg.get_symbol $1)
						)
				in
				raise (Sem.SemErrorWithFun ((Printf.sprintf "%s is not a valid location" $1),dsp))	
		}
|	Location DOUBLE_COLON Location
		{ Irg.LOC_CONCAT ($1, $3) }
;


/****************** Nouveau Conditionnal Statement ************/
ConditionalStatement: 
	IF Expr THEN Sequence OptionalElse ENDIF
		{ Irg.IF_STAT ($2, $4, $5) }
|	SWITCH LPAREN Expr RPAREN LBRACE CaseBody RBRACE
		{Irg.SWITCH_STAT ($3, fst $6, snd $6)}




OptionalElse:
	/* empty */		{ Irg.NOP }
|	ELSE Sequence	{ $2 }
;





/*******************  NOUVELLE DEF DU SWITCH *********************/

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

/******************* FIN NOUVELLE DEF DU SWITCH  *********************/




/******************* EXPRS TYPEES  *********************/

	/* A COMPLETER IMPERATIVEMENT */

Expr :
	COERCE LPAREN Type COMMA Expr RPAREN
		{ Irg.COERCE ($3,$5) }
|	FORMAT LPAREN STRING_CONST COMMA ArgList RPAREN
		{ 
		if Irg.is_defined $3 
			then
				Irg.FORMAT($3,$5)
			else 
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $3))
		}
|	STRING_CONST LPAREN ArgList RPAREN	/* A CHANGER (c'est les canonical functions) */
		{
			Irg.NONE 
		}
|	ID DOT SYNTAX
		{ 
		if Irg.is_defined $1
			then
				(*if Sem.have_attribute $1 "syntax"
					then*)
						Irg.FIELDOF (Irg.STRING,Irg.REF $1,"syntax")
					(*else
						raise (Sem.SemError (Printf.sprintf " %s doesn't have a syntax attribute\n" $1))*)
			else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID DOT IMAGE
		{ 
		if Irg.is_defined $1
			then
				(*if Sem.have_attribute $1 "image"
					then*)
						Irg.FIELDOF (Irg.STRING,Irg.REF $1,"image")
					(*else
						raise (Sem.SemError (Printf.sprintf " %s doesn't have an image attribute\n" $1))*)
			else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID DOT ID
		{ 	
		if Irg.is_defined $1
			then
				
				(*if Sem.have_attribute $1 $3
					then*)
						Irg.FIELDOF (Irg.STRING,Irg.REF $1,$3)
					(*else
						raise (Sem.SemError (Printf.sprintf " %s doesn't have a %s attribute\n" $1 $3))*)
				
			else
				raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	Expr DOUBLE_COLON Expr
		{ Irg.NONE }	/* A CHANGER */
|	ID 
		{ 	if Irg.is_defined $1 
				then 
					Irg.REF $1
				else
					raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
	 	}
|	ID BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT			
		{ 

		if Irg.is_defined $1 then
			if (Sem.is_location $1) || (Sem.is_loc_spe $1) || (Sem.is_loc_mode $1)
			then

				try(	(* A changer ? *)

					let v1 = Int32.to_int (Sem.to_int32 (Sem.eval_const $3))
				and v2 = Int32.to_int(Sem.to_int32 (Sem.eval_const $5))
					in
					if v1<=v2
					then
						Irg.BITFIELD (Irg.CARD (v2-v1),Irg.REF $1,$3, $5)
					else
						Irg.BITFIELD (Irg.CARD (v1-v2),Irg.REF $1,$3, $5)

				)with Sem.SemError _ ->Irg.BITFIELD (Irg.CARD (-1),Irg.REF $1,$3, $5)	(* A changer *)
		
			else
				let dsp = fun _->(
							print_string "Type : ";
							Irg.print_spec (Irg.get_symbol $1)
						)
				in
				raise (Sem.SemErrorWithFun ((Printf.sprintf "Can't apply bitfield on %s" $1),dsp))	
		else raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	ID LBRACK Expr RBRACK
		{ 
		if Irg.is_defined $1 then
			if (Sem.is_location $1) || (Sem.is_loc_spe $1)  || (Sem.is_loc_mode $1)
				then
					Irg.ITEMOF ((Sem.get_type_ident $1),Irg.REF $1, $3) 
				else 
					let dsp = fun _->(
							print_string "Type : ";
							Irg.print_spec (Irg.get_symbol $1)
							)
					in
					raise (Sem.SemErrorWithFun ((Printf.sprintf "%s is not a valid location" $1),dsp))	
		else raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))	
		}
|	ID LBRACK Expr RBRACK BIT_LEFT Bit_Expr DOUBLE_DOT Bit_Expr GT		
		{ 
		if Irg.is_defined $1 then
			if (Sem.is_location $1) || (Sem.is_loc_spe $1) (* || (Sem.is_loc_mode $1) *)
			then
				Irg.BITFIELD ((Sem.get_type_ident $1),Irg.ITEMOF ((Sem.get_type_ident $1),Irg.REF $1, $3), $6, $8) (* A changer *)
			else 
				let dsp = fun _->(
						print_string "Type : ";
						Irg.print_spec (Irg.get_symbol $1)
						)
				in
				raise (Sem.SemErrorWithFun ((Printf.sprintf "%s is not a valid location" $1),dsp))	
		else raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))		
		}
|	Expr PLUS Expr
		{ 
			Sem.get_binop $1 $3 Irg.ADD
		}
|	Expr MINUS Expr
		{
 			Sem.get_binop $1 $3 Irg.SUB
		}
|	Expr STAR Expr
		{
			Sem.get_binop $1 $3 Irg.MUL
		 }
|	Expr SLASH Expr
		{ 
			Sem.get_binop $1 $3 Irg.DIV
		 }
|	Expr PERCENT Expr
		{ 
			Sem.get_binop $1 $3 Irg.MOD
		 }
|	Expr DOUBLE_STAR Expr
		{ 
			Sem.get_binop $1 $3 Irg.EXP
		 }
|	Expr LEFT_SHIFT Expr
		{ 
			Sem.get_binop $1 $3 Irg.LSHIFT
		 }
|	Expr RIGHT_SHIFT Expr
		{ 
			Sem.get_binop $1 $3 Irg.RSHIFT
		}
|	Expr ROTATE_LEFT Expr
		{ 
			Sem.get_binop $1 $3 Irg.LROTATE
		 }
|	Expr ROTATE_RIGHT Expr
		{ 
			Sem.get_binop $1 $3 Irg.RROTATE
		 }
|	Expr LT Expr
		{ 
			Sem.get_binop $1 $3 Irg.LT
		}
|	Expr GT Expr
		{ 
			Sem.get_binop $1 $3 Irg.GT
		 } 
|	Expr LEQ Expr
		{ 
			Sem.get_binop $1 $3 Irg.LE
		}
|	Expr GEQ Expr
		{ 
			Sem.get_binop $1 $3 Irg.GE
		 }
|	Expr EQU Expr
		{ 
			Sem.get_binop $1 $3 Irg.EQ
		}
|	Expr NEQ Expr
		{ 
			Sem.get_binop $1 $3 Irg.NE
		}

|	Expr AMPERS Expr
		{ 
			Sem.get_binop $1 $3 Irg.BIN_AND
		}
|	Expr CIRC Expr
		{ 
			Sem.get_binop $1 $3 Irg.BIN_XOR
		}
|	Expr PIPE Expr
		{ 
			Sem.get_binop $1 $3 Irg.BIN_OR
		}
|	EXCLAM Expr
		{
			Sem.get_unop $2  Irg.NOT
		}	
|	TILD Expr
		{
			Sem.get_unop $2  Irg.BIN_NOT
		}

|	PLUS Expr %prec TILD
		{ $2 }
|	MINUS Expr %prec TILD		{
			Sem.get_unop $2  Irg.NEG
		}
|	Expr AND Expr
		{ 
			Sem.get_binop $1 $3 Irg.AND
		}
|	Expr OR Expr
		{ 
			Sem.get_binop $1 $3 Irg.OR
		}
|	LPAREN Expr RPAREN
		{ $2 }
|	FIXED_CONST
		{ 
			let m =8
			and e=24
			in
			Irg.CONST (Irg.FIX(m,e),Irg.FIXED_CONST  $1) }
|	CARD_CONST
		{
			let c=32
			in 
			Irg.CONST (Irg.CARD c,Irg.CARD_CONST $1) 
		}

|	CARD_CONST_64
		{
			Irg.CONST (Irg.CARD 64,Irg.CARD_CONST_64 $1)
		}

|	STRING_CONST
		{ Irg.CONST (Irg.STRING,Irg.STRING_CONST $1) }
|	STRING_VALUE
		{ Irg.CONST (Irg.STRING,Irg.STRING_CONST $1) }
/*|	DOLLAR { }
|	BINARY_CONST { }
|	HEX_CONST { }*/
/*|	IF Expr THEN Expr OptionalElseExpr ENDIF */
|	IF Expr THEN Expr ELSE Expr ENDIF
		{ let t1=(Sem.get_type_expr $4)
		  and t2=(Sem.get_type_expr $6)	
		  in
		if Sem.check_if_expr $4 $6
			then
				Irg.IF_EXPR (t1,$2, $4, $6)
			else
				(
				 let dsp =(fun _->
					print_string "Type of the first operand : ";
				 	Irg.print_type_expr t1;
					print_string "\n";
					print_string "Type of the second operand : ";
				 	Irg.print_type_expr t2;
				 	print_string "\n"
				 )
				 in
				 raise (Sem.SemErrorWithFun ("In this conditional expression, the first and second operands must be of compatible type",dsp))
				)
		}
|	SWITCH LPAREN Expr RPAREN LBRACE CaseExprBody RBRACE
		{
			
			Irg.SWITCH_EXPR (Sem.check_switch_expr $3 (fst $6) (snd $6),$3, fst $6, snd $6) 

		}	
;	

Bit_Expr :
	ID
		{ 
		if Irg.is_defined $1
			then
				Irg.REF $1
			else
				 raise (Sem.SemError (Printf.sprintf "the keyword %s is undefined\n" $1))
		}
|	Bit_Expr PLUS Bit_Expr
		{ 
			Sem.get_binop $1 $3 Irg.ADD
			(*Irg.BINOP (Irg.ADD, $1, $3) *)
		}
|	Bit_Expr MINUS Bit_Expr
		{ 
			Sem.get_binop $1 $3 Irg.SUB
			(*Irg.BINOP (Irg.SUB, $1, $3)*) 
		}
|	Bit_Expr STAR Bit_Expr
		{ 
			Sem.get_binop $1 $3 Irg.MUL
			(*Irg.BINOP (Irg.MUL, $1, $3)*)
		 }
|	Bit_Expr SLASH Bit_Expr
		{
			Sem.get_binop $1 $3 Irg.DIV
			(* Irg.BINOP (Irg.DIV, $1, $3)*)
		 }
|	Bit_Expr PERCENT Bit_Expr
		{ 
			Sem.get_binop $1 $3 Irg.MOD
			(*Irg.BINOP (Irg.MOD, $1, $3)*)
		 }
|	Bit_Expr  DOUBLE_STAR Bit_Expr
		{ 
			Sem.get_binop $1 $3 Irg.EXP
			(*Irg.BINOP (Irg.EXP, $1, $3)*)
		 }
|	LPAREN Bit_Expr RPAREN
		{ $2 }
|	FIXED_CONST
		{ Irg.CONST (Irg.FIX(8,24),Irg.FIXED_CONST $1) }
|	CARD_CONST
		{ Irg.CONST (Irg.CARD 32,Irg.CARD_CONST $1) }
|	STRING_CONST
		{ Irg.CONST (Irg.STRING,Irg.STRING_CONST $1) }
;


/******************* NOUVELLE DEF DU SWITCH FONCTIONNEL  *********************/

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

/******************* FIN NOUVELLE DEF DU SWITCH FONCTIONNEL  *********************/


/* UNUSED */
OptionalElseExpr:
	/* empty */ { Irg.NONE }
|	ELSE Expr { $2 }
;



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
| UsesLocationList  AMPERS  UsesLocation { }
;

UsesLocation :
	ID Opt_Bit_Optr { }
|	ID LBRACK Expr RBRACK Opt_SecDim Opt_Bit_Optr { }
;

Opt_SecDim : 
	/* empty */		{ }
|	LBRACK RBRACK	{ }
;

