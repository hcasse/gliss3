let num= '0' | (['1'-'9'] (['0'-'9']+)?)
let pourcentS= '%' 's'
let bin= ("0"|"1")



rule formatSize size indice liste = parse
	bin							{ formatSize  ( size + 1) indice liste lexbuf }
	| '%'(num as n)('b'|'B') 	{ formatSize ( (int_of_string n) + size) (indice+1) liste lexbuf }
	| pourcentS					{ formatSize  size (indice + 1) (indice::liste) lexbuf }
	| eof						{ (size ,liste) }
	| _ 						{ failwith "optirg : Lexer de format : Un caractère inconnu a été rencontré. " }

and formatClass = parse
	bin						{ "x"^(formatClass lexbuf) }
	| '%'num('b'|'B') as pb	{ pb^(formatClass lexbuf) }
	| pourcentS	as ps		{ ps^(formatClass lexbuf) }
	| eof					{ "" }
	| _ 					{ failwith "optirg : Lexer de format : Un caractère inconnu a été rencontré. " }


and formatCode acc = parse
	bin as n			{ let n=(int_of_char n)-(int_of_char '0') in formatCode (2 * acc + n) lexbuf }
	| '%'num('b'|'B')	{ formatCode acc lexbuf (*let n=(int_of_string n) in formatCode ((2 lsl n) * acc) lexbuf*) }
	| pourcentS			{ formatCode acc lexbuf}
	| eof				{ acc }
	| _ 				{ failwith "optirg : Lexer de format : Un caractère inconnu a été rencontré. " }
