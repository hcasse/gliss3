
open Lexing

let _ =
	let lexbuf = Lexing.from_channel stdin in
	try	
		Parser.top Lexer.main lexbuf;
		Irg.StringHashtbl.iter (fun _ s -> Irg.print_spec s) Irg.syms 
	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error";
		Printf.printf "=> %s\n" (Lexing.lexeme lexbuf)
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr)
	| Sem.SemError msg ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg)
	| Sem.SemErrorWithFun (msg, fn) ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg);
		let old_out = Unix.dup Unix.stdout in
		Unix.dup2 Unix.stderr Unix.stdout;
		print_string "yahoo !\n";
		fn ();
		Unix.dup2 old_out Unix.stdout;
		Unix.close old_out

	
