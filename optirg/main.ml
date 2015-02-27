
open Irg
open Optirg

(* argument list *)
let irg = ref ""
let out = ref "" 
let insts = ref false
let options = [ ]

(* argument decoding *)
let free_arg arg =
	if !irg = "" then irg := arg else
	if !out = "" then out := arg else
	raise (Arg.Bad "only IRG and out files required") 
let usage_msg = "SYNTAX: optirg IRG_SRC IRG_DEST\n\tOptimize IRG."

let arg_error msg =
		Printf.fprintf stderr "ERROR: %s\n" msg;
		Arg.usage options usage_msg;
		exit 1

let find_irg_root_node _ =
	let is_defined id =
			match Irg.get_symbol id with
			| UNDEF -> false
			| _ -> true
	in
	if is_defined "multi" then
		"multi"
	else if is_defined "instruction" then
		"instruction"
	else
		raise (Sys_error "you must define a root for your instruction tree\n \"instruction\" for a single ISA\n \"multi\" for a proc with several ISA (like ARM/THUMB)")
	
	

let _ =
	Arg.parse options free_arg usage_msg;
	if !irg = "" then arg_error "one IRG source file must be given !\n";
	if !out = "" then arg_error "one IRG dest file must be given !\n"

let _ =
	try	
		begin
			Irg.load !irg;
			Optirg.optimize (find_irg_root_node ());
			Irg.save !out
		end
	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error"; exit 2
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr); exit 2
