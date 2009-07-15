(**

*)

(** index for node name *)
let cpt = ref 0

(** A éjecter d'ici. Copie de Optirg.next pour éviter des dep circulaires. *)
let next (s:string) : (string list) = match (Irg.get_symbol s) with
	|Irg.AND_MODE(_,pl,_,_) | Irg.AND_OP(_,pl,_) 
		-> List.fold_right (fun (_,t) r ->match t with Irg.TYPE_ID(s)->s::r | _ -> r) pl []
	|Irg.OR_MODE(_,sl) | Irg.OR_OP(_,sl) 
		-> sl
	| _ 
		-> []

(** *)
let edge_attr = [Tod.EColor("black");Tod.EArrowSize(2.)]

(** *)
let node_attr_from_spec (s:Irg.spec) :(Tod.node_attribute list) = 
	match s with
	| Irg.AND_MODE(_,_,_,_) -> [Tod.NColor("green");Tod.NShape("box")]
	| Irg.OR_MODE(_,_) -> [Tod.NColor("red");Tod.NShape("box")]
	| Irg.AND_OP(_,_,_) -> [Tod.NColor("green");Tod.NShape("ellipse")]
	| Irg.OR_OP (_,_)-> [Tod.NColor("red");Tod.NShape("ellipse")]
	| _ -> []

let node_of_spec (node_id:string) (s:Irg.spec) :(Tod.stmt) = 
	Tod.Node(node_id, node_attr_from_spec s)

let edge (nodeid_list:string list) :Tod.stmt = 
	Tod.Edge(List.map (fun e -> Tod.NodeID(e)) nodeid_list,edge_attr)

let rec mk_tree_stmt_list (name_father:string) (nodeid_father:string) = 
	let sons = next name_father in
	List.fold_right 
		(fun son_name sons_stmt -> 
			let son_id = son_name^"_"^string_of_int(cpt:=!cpt+1;!cpt) in
			(
				(node_of_spec son_id (Irg.get_symbol son_name)))::
				(edge [nodeid_father ; son_id])::
				(mk_tree_stmt_list son_name son_id)@
				sons_stmt
			) 
		sons []

let rec mk_stmt_list (name_father:string) (nodeid_father:string) = 
	let sons = next name_father in
	List.fold_right 
		(fun son_name sons_stmt -> 
			let son_id = son_name in
			(
				(node_of_spec son_id (Irg.get_symbol son_name)))::
				(edge [nodeid_father ; son_id])::
				(mk_stmt_list son_name son_id)@
				sons_stmt
			) 
		sons []
	
let mk_tree_graph (root_name:string) :Tod.dot = 
	let root_id = root_name^"_"^string_of_int(cpt:=0;!cpt) in
	let root_node = node_of_spec root_id (Irg.get_symbol root_name) in 
	let stmt_list = root_node::(mk_tree_stmt_list root_name root_id) in 
	Tod.Digraph("IRG",false,stmt_list)


let mk_graph (root_name:string) :Tod.dot = 
	let root_id = root_name in
	let root_node = node_of_spec root_id (Irg.get_symbol root_name) in 
	let stmt_list = root_node::(mk_stmt_list root_name root_id) in 
	Tod.Digraph("IRG",false,stmt_list)

let write = Tod.write


