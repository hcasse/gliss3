
open Lexing

let _ =
	let lexbuf = Lexing.from_channel (*stdin*) (open_in "../test/ppc.nml") in
	try	
		Parser.top Lexer.main lexbuf;
		print_string "success !\n";
		
		

		
		print_string "Affichage resultat\n";
(*		Irg.StringHashtbl.iter (fun _ s -> Irg.print_spec s) Irg.syms;*)

		
		print_string "Affichage positions\n";
		Iter.Iter.iter
			(fun _ i ->
				Format.printf "%s: %d\n"
					(Iter.Iter.get_name i)
					(Iter.Iter.get_id i))
			()

	with
	  Parsing.Parse_error ->
		Lexer.display_error "syntax error";
		Printf.printf "=> %s\n" (Lexing.lexeme lexbuf)
	| Lexer.BadChar chr ->
		Lexer.display_error (Printf.sprintf "bad character '%c'" chr)
	| Sem.SemError msg ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg)
	| Irg.IrgError msg ->
		Lexer.display_error (Printf.sprintf "ERROR: %s" msg)
	| Sem.SemErrorWithFun (msg, fn) ->
		Lexer.display_error (Printf.sprintf "semantics error : %s" msg);
		let old_out = Unix.dup Unix.stdout in
		Unix.dup2 Unix.stderr Unix.stdout;
		(**)
		print_string "*****	Debut aff Erreur	*****\n";
		fn ();
		Unix.dup2 old_out Unix.stdout;
		Unix.close old_out
		
	(**)
	|Failure e->Lexer.display_error e
	(**)
	

let _ = Irg.test_instant_spec "instruction"
