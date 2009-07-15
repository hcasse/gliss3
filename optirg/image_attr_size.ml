(**
Image attribute size computing. 
Used to see if a node can be optimized.

Use Irg.cma
Use string_of_expr.cmo

Use a very simple ocamllex lexer.
(See formatlexer.mll)

*)

open Irg
open String_of_expr

(**
exception InconsistentSize
Raised if the attribute image can have different (inconsistent) sizes.
*)
exception InconsistentSize;;


(**
parse
Lexer Call

Test : 
let format= "%2b%s111001%5b%s"
let _ = parse format ;;

*)
let parse (chaine:string):(int * int list)  = 
  let lexbuf = Lexing.from_string  chaine in
   Formatlexer.formatSize 0 0 [] lexbuf


(**
get_expr_of_image
Param : a:attribute list from an And node.
Return : expression of attribute image from a
*)
let rec get_expr_of_image a =
                match a with
                [] -> 
                (* if attr not found => means an empty attr (?) *)
                      failwith "get_expr_of_image: No image found in this list"  
                | ATTR_EXPR(nm, e)::t ->
                        if nm = "image" then
                                e
                        else
                                get_expr_of_image t
                | _::t -> get_expr_of_image t;;

(**
Functions used to calculate the size of an image attribute. 

Raise InconsistentSize

Use string_of_expr.cmo

sizeOfExpr
	Param: e : the expresions that describes the attribute
	Return: the size
sizeOfFormat
	Param: an expression wich is a format 
	Return: the size
sizeOfNodeKey
	Param: a string wich represente the key in the hashtable.
	Return: the size
sizeOfSpec
	Param: a spec wich reprensent a node in the hashtable.
	Return: the size
*)
let rec sizeOfExpr 
	(listeParam:(string * typ) list)  
	(e:Irg.expr)
	:int = 
	begin
	match e with 
			FIELDOF(STRING, objName, attrName) -> 
				let modop = match (List.assoc objName listeParam) with 
					|	TYPE_ID(name) -> name
					|	_ -> failwith "sizeOfExpr: Field access atemp on simple type."
				in
				sizeOfNodeKey modop (* Aller chercher dans liste param le nom du mode/op correspondant à n *)
		|	ELINE(_,_,e) ->  sizeOfExpr listeParam e
		| 	FORMAT(st, expr_list) -> sizeOfFormat listeParam st expr_list
		| 	REF(st) -> sizeOfNodeKey (st)
		| 	IF_EXPR (_, _,  eThen,  eElse) -> 
				let sizeThen = sizeOfExpr listeParam eThen 
				and sizeElse = sizeOfExpr listeParam eElse in 
				if(sizeThen <> sizeElse) then 
					raise InconsistentSize
				else 
					sizeThen
		| 	SWITCH_EXPR (type_expr, e , ((_,t)::liste),_) -> 
				let sizeCmp = fun (_,e) size -> 
					if(sizeOfExpr listeParam e <> size) then 
						raise  InconsistentSize 
					else 
						size 
				in 
				List.fold_right (sizeCmp) liste (sizeOfExpr listeParam t) 
		| 	CONST(_,STRING_CONST(st))-> String.length st
		|	_ -> failwith ("sizeOfExpr : Constructor "^(name_of_expr e)^" of expr is not yet implemented. ")
	end
and
 sizeOfFormat 
	(listeParam:(string * typ) list) 
	(st:string) 
	(expr_l:(Irg.expr list)) 
	:int = 
	begin 
	let (size, op_l)=parse st in  
		match op_l with 
		[] -> size
		|_ -> size + List.fold_right (fun nop somme -> somme + ( sizeOfExpr listeParam (List.nth  expr_l nop) )) op_l 0
	end
and	
 	sizeOfNodeKey  
	(key:string) 
	:int = 
	begin
		let size = sizeOfSpec (Irg.get_symbol key) in 		
		print_string (key^" size = "^(string_of_int size)^".\n")
		;size
	end
and
	sizeOfSpec 
	(spec:Irg.spec)
	:int = 
	begin
	match spec with 
	| 	AND_MODE (_,listeParam,_,attr_list) | AND_OP(_,listeParam, attr_list) -> sizeOfExpr listeParam (get_expr_of_image attr_list)
	| 	OR_MODE (_,st_list) | OR_OP (_,st_list) -> 
		let h::size_list = (List.map (sizeOfNodeKey) st_list) in
		List.fold_right (fun s size -> if(s <> size) then raise  InconsistentSize else size ) size_list h
	| _ ->	failwith "sizeOfSpec : Node of spec not implemented "
	end
;;
