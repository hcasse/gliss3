(* 
Irg optimization 
OR_node factorization.

07.2009
*)

open Irg
(**Association of an node and its sons *)
type opt_struct= Irg.spec (* current node *)
				*
			  Irg.spec list (* list of sons *)

let lra = ref []
let lrs = ref []
let lrp = ref []
let la = ref []


(** 
	Provide the list of all sons's name for a parent node.  
	@param s
		the name of the parent node
	@return 
		the string list that contains the names
*)
let next (s:string) : (string list) = match (get_symbol s) with
	|AND_MODE(_,pl,_,_) | AND_OP(_,pl,_) 
		-> List.fold_right (fun (_,t) r ->match t with TYPE_ID(s)->s::r | _ -> r) pl []
	|OR_MODE(_,sl) | OR_OP(_,sl) 
		-> sl
	| _ 
		-> []

(**
	Provide the value of a node.
	@param and_node
		the and_node.
	@return 
		the value of the node.
*)


let get_expr_from_value_from_and_mode (and_node:Irg.spec) :Irg.expr = 
	match and_node with 
		| AND_MODE(_,_,e,_) -> e
		| _ -> failwith "get_expr_from_value_from_and_mode: 1st argument must be an AND MODE. "
(**
	Extract switch case number from an And Node spec. 
	We use here the image attribute value as case number. 
	@param  s 
		the node 
	@return 
		the code of the node
*)

let case_code_from_spec (s:Irg.spec) :int = match s with 
	| Irg.AND_MODE (_,_,_,attr_list)|Irg.AND_OP (_,_,attr_list) -> 
		let image_expr = (Image_attr_size.get_expr_of_image attr_list) in 
		let rec int_of_expr expr = 
			match expr with 
			| CONST(STRING,STRING_CONST(image_string)) -> 
				int_of_string ("0b"^image_string)
			| ELINE(_,_,e) -> int_of_expr e
			| _ -> failwith ("code_from_spec: Optimization need constant string as image. ("^(String_of_expr.name_of_expr image_expr)^") ")
		in
			int_of_expr image_expr
	| _ -> failwith "code_from_spec: require an and_node as argument."

(**
	Compute case structure of SWITCH_EXPR from an And Node  and the attribute name. 
	@param size 
		the image size of the attribute
	@param attr_name
		the name of the attribute
	@param and_node 
		the node that contains the attribute
	@return 
		the case for an switch expression
*)

let case_from_attr_expr size (attr_name:string) (and_node:Irg.spec) :(Irg.expr*Irg.expr) = 
	(
		Irg.CONST(Irg.CARD(size),Irg.CARD_CONST(Int32.of_int (case_code_from_spec and_node))), 
		Irg.get_expr_from_attr_from_op_or_mode and_node attr_name
	)

(**
	Compute case structure of SWITCH_STAT from an And Node spec and the attribute name. 
	param size=size of CARD parameter of the new and_node (in bit).
	@param size 
		the image size of the attribute
	@param attr_name
		the name of the attribute
	@param and_node 
		the node that contains the attribute
	@return 
		the case for an switch statement 
*)
let case_from_attr_stat size (attr_name:string) (and_node:Irg.spec) :(Irg.expr*Irg.stat) = 
	(
		Irg.CONST(Irg.CARD(size),Irg.CARD_CONST(Int32.of_int (case_code_from_spec and_node))), 
		Irg.get_stat_from_attr_from_spec and_node attr_name
	)

(**
	Compute case structure of SWITCH_EXPR to return value from an And Node spec. 
	@param and_node 
		the node that contains the attribute
	@return
		the case for an switch expression
*)

let case_from_value_expr size (and_node:Irg.spec) :(Irg.expr*Irg.expr) = 
	(
		Irg.CONST(Irg.CARD(size),Irg.CARD_CONST(Int32.of_int (case_code_from_spec and_node))), 
		get_expr_from_value_from_and_mode and_node
	)
(**
	Return the type of the expression
	@param expr
		An Irg.expr 
	@return 
		An Irg.type_expr
*)

let rec type_of_expr (expr:Irg.expr) : Irg.type_expr = match expr with 
	| 	Irg.NONE -> Irg.NO_TYPE
	| 	Irg.COERCE(type_expr,_) -> type_expr
	| 	Irg.FORMAT(_,_)-> Irg.STRING
	| 	Irg.CANON_EXPR( type_expr,_,_)-> type_expr
	| 	Irg.REF(_)-> Irg.UNKNOW_TYPE
	| 	Irg.FIELDOF(type_expr,_,_) -> type_expr
	| 	Irg.ITEMOF (type_expr,_,_)-> type_expr
	| 	Irg.BITFIELD (type_expr,_,_,_) -> type_expr
	| 	Irg.UNOP (type_expr,_,_)-> type_expr
	| 	Irg.BINOP (type_expr,_,_,_)-> type_expr
	| 	Irg.IF_EXPR (type_expr,_,_,_)-> type_expr
	| 	Irg.SWITCH_EXPR (type_expr,_,_,_)-> type_expr
	| 	Irg.CONST (type_expr,_)-> type_expr
	| 	Irg.ELINE (_,_,e)-> type_of_expr e
	| 	Irg.EINLINE(_)-> Irg.NO_TYPE

(**
	Create an opt_t_struct with the name of the node.
	@param name
		the name of the node
	@return 
		the structure that has been create
*)

let create_opt_struct (name:string) : opt_struct =
	let spec= Irg.get_symbol name and 
	liste_fils = List.fold_right (fun st r ->  (Irg.get_symbol st):: r) (next name) []
	in 
		(spec, liste_fils) 
(**
	Insert an element into a set implemented as a list.
	@param elem
		the element to add
	@param set
		the list where to insert.  
*)
let union_add (elem:'a)  (set:'a list)  :'a list =
	if( List.exists ((=) elem) set) then set 
	else elem :: set 

(*let union = List.fold_right (union_add)*)


(**
	
*)

let get_attr_name = function 
	| 	ATTR_EXPR(st,_) | 	ATTR_STAT(st,_) -> st
	| 	ATTR_USES -> "none"


let get_attr= function 
					| AND_MODE(_,[],_,attr_list) | AND_OP(_,[],attr_list) -> attr_list
					| _ ->failwith "Optirg.get_attr: AND_OP or AND_MODE expected here"


let list_equal l1 l2 = 
	let len1=  (List.length l1) 
	and len2= (List.length l2)
	and aux at1 at2 res= 
		List.exists(fun a -> (get_attr_name a) = (get_attr_name at1)) l2 
		&& List.exists(fun a -> (get_attr_name a) = (get_attr_name at2)) l1  
		&& res 
	in 
		(len1 = len2) && List.fold_right2 (aux) l1 l2 true


let same_attr_list list_of_nodes=
	try  
		let head_list = get_attr(List.hd list_of_nodes) in 
		let aux node res = (list_equal head_list (get_attr node) ) && res in 
		List.fold_right (aux) list_of_nodes true
	with 
		|_ -> false 

(**
	Verify if a node can be optimized.
	@param 
		the node to check 
	@return
		true or false weather the node can be optimized or not
*)
let is_opt (struc:opt_struct) :bool = 
	match struc with 
	|	(OR_MODE(name,_), sons) | (OR_OP(name,_), sons) ->
		begin 

			let param = (List.for_all 
				(
				function 
					| AND_MODE(_,[],_,_) | AND_OP(_,[],_) -> true 
					| AND_MODE(n,_,_,_) | AND_OP(n,_,_) -> false
					| _ -> false
				) 
				sons
			) in
			let attr = (same_attr_list sons) in
			let size = (try let _= Image_attr_size.sizeOfNodeKey name in true with | _ -> false) in 

			if param && (not attr) && (not size) then (lrp:=("\t"^name^"\n")::!lrp) else ();
			if (not param) && attr && (not size) then (lra:=("\t"^name^"\n")::!lra) else ();
			if (not param) && (not attr) && size then (lrs:=("\t"^name^"\n")::!lrs) else ();
			if param && attr && size then (la:=("\t"^name^"\n")::!la) else ();

			param && attr && size

		end
	|	_ -> false


(**
	Insert all nodes in a set of opt_stuct implemanted as a list.
	@param 
		the set  
	@param
		the name of the node
	@return
		the set that has been modified.
		 
*)
let rec set_of_struct 
	(set: opt_struct list ) 
	(pere:string) 
	: opt_struct list 
	=
	 let node_opt= (create_opt_struct pere)	in 
		if (is_opt node_opt) then 
			union_add node_opt set
		else 
			List.fold_right (fun fils res -> set_of_struct res fils) (next pere) set

(**
	Extracte a list of attributes from a list of and nodes.
	@param and_list
		the liste of nodes
	@param size
		an attribute's size.
	@return 
		the list of attribute 
*)

let attr_list_from_and_node 
	(and_list : Irg.spec list)
	(size : int)
	:Irg.attr list = 
	match List.hd and_list with 
	| Irg.AND_OP(_,_,attr_list) 
	| Irg.AND_MODE(_,_,_,attr_list)
		-> List.map 
			(
			function 
			|Irg.ATTR_EXPR(name,e) -> 
				ATTR_EXPR(
					name,
					SWITCH_EXPR(
						(type_of_expr e), 
						REF("code"), 
						(List.map (case_from_attr_expr size name) and_list) , 
						Irg.NONE
					)
				)
			|Irg.ATTR_STAT(name,_) -> 
				ATTR_STAT(
					name,
					SWITCH_STAT(
						REF("code"), 
						List.map (case_from_attr_stat size name) and_list, 
						Irg.NOP
					)
				)
			|Irg.ATTR_USES -> failwith "optirg : attr_list_from_and_node : ATTR_USES not implemented yet."
			) 
			attr_list
	| _ -> failwith "optirg : attr_list_from_and_node : We must have AND Node here. "


(**
	fuse an or_node to create an or mode wich contains all alternative from the or node.
	@param
		or_node: the node that will be optimized
		and_list: the list of and node attached with the or_node
	@return 
		a node which is fused with its sons.
*)

let fusion 
	((or_node,and_list):(opt_struct))
	:Irg.spec =
	let size = Image_attr_size.sizeOfSpec or_node in
	let new_attr_list = attr_list_from_and_node and_list size in
	match or_node with

	(* Case in which we have a MODE *)
	| Irg.OR_MODE(name,_) -> 
		let val_expr = SWITCH_EXPR(STRING, REF("code"), (List.map (case_from_value_expr size) and_list), NONE) in
		Irg.AND_MODE(name,[("code",Irg.TYPE_EXPR(Irg.CARD(size)))], val_expr, new_attr_list)

	(* Case in which we have a MODE *)
	| Irg.OR_OP(name,_) -> 
		Irg.AND_OP(name,[("code",Irg.TYPE_EXPR(Irg.CARD(size)))], new_attr_list)

	(* Case in which we have an other thing : it should not happen here. *)
	| _ -> failwith "fusion : 1st argument must be an or node."

(**
	Remove nodes that can be optimized from the hashtable and replace then by the optimezed ones.
	@param 
		or_node: the node that will be optimized
		and_list: the list of and node attached with the or_node
*)
let imp_fusion ((or_node,and_list):(opt_struct)) :unit = 
	let name = (Irg.name_of or_node) and
		spec = fusion (or_node,and_list) in 
	Irg.rm_symbol name; Irg.add_symbol name spec


(**
	Optimize the tree from the root and print the number of optimization. 
	@param 
		the name of the root
*)
let optimize (name : string) :unit =
	let liste_opt= set_of_struct [] name in 
	let nb_opt = string_of_int (List.length liste_opt) in
	begin
	match liste_opt with 
	[] -> () 
			; (print_string ("##################################\n############# No Optimization ####\n##################################\n")) 
	|_ -> List.iter (imp_fusion) liste_opt 
			; (print_string ("#####################################\n#### Number of optimization =  "^nb_opt^" ####\n#####################################\n")) 
	end ; 
	print_string "\nOptimized nodes : \n" ;
	List.iter (print_string) !la ; 
	print_string "\nNon-Optimizable because of parameters only : \n" ;
	List.iter (print_string) !lrp ;
	print_string "\n" ;
	print_string "\nNon-Optimizable because of size inconsistences only : \n" ;
	List.iter (print_string) !lrs ;
	print_string "\n" ;
	print_string "\nNon-Optimizable because of attributes inconsistences only : \n" ;
	List.iter (print_string) !lra ;
	print_string "\n" 







