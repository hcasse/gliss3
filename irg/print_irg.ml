(* argument list *)
let nmp: string ref = ref ""
let options = [ ]

(* argument decoding *)
let free_arg arg =
	if !nmp = ""
	then nmp := arg
	else raise (Arg.Bad "only one NML file required") 
let usage_msg = "SYNTAX: gep [options] NML_FILE\n\tGenerate code for a simulator"
let _ =
	Arg.parse options free_arg usage_msg;
	if !nmp = "" then begin
		prerr_string "ERROR: one NML file must be given !\n";
		Arg.usage options usage_msg;
		exit 1
	end


let _ =
	try	
		begin
			let lexbuf = Lexing.from_channel (open_in !nmp) in
			Parser.top Lexer.main lexbuf;
			Irg.StringHashtbl.iter (fun _ s -> Irg.print_spec s) Irg.syms
		end
	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error"; exit 2
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr); exit 2
	| Sem.SemError msg ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg); exit 2
	| Irg.IrgError msg ->
		Lexer.display_error (Printf.sprintf "ERROR: %s" msg); exit 2
	| Sem.SemErrorWithFun (msg, fn) ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg);
		fn (); exit 2;
