

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
(**
	Parse an string to extract its size and the positions of expressions in the format.
	@param chaine
		the string that describe the format of expressions.
	@return	
		-the number of expressions
		-the list of positions.
	@see formatlexer.mll
		
*)
let parse (chaine:string):(int * int list)  = 
  let lexbuf = Lexing.from_string  chaine in
   Formatlexer.formatSize 0 0 [] lexbuf


(**
	Extract the expression of an image attribute from the list of attribute.
	@param attr_list
		the list of attributes.
	@return
		the expression of the image attribute.
*)
let rec get_expr_of_image attr_list =
                match attr_list with
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
	Calculate an expression's size. 
	@param e
		the expresions that describes the attribute
	@return
		the size
	Raise InconsistentSize
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
(**
	Calculate the size for an format expression.
	@param listeParam
		the node's list of parameters
	@param st
		the string which describe the format of expressions
	@param expr_l
		the list of expressions that are described.
	@return
		the size
*)
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
(**
	Return the size of a node's expression
	@param key
		the node's name which is the key in the hashtable.
	@return
		 the size
*)
 	sizeOfNodeKey  
	(key:string) 
	:int = 
	begin
		sizeOfSpec (Irg.get_symbol key)
	end
and
(**
	Return the size of a node's expression.
	@param spec
		a spec which reprensent a node in the hashtable.
	@return
		 the size
*)
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
