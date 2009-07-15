let num= '0' | (['1'-'9'] (['0'-'9']+)?)
let pourcentB= '%'(num as n)('b'|'B')
let pourcentS= '%' 's'
let bin= ('0'|'1')



rule formatSize size indice liste = parse
	bin 			{ formatSize  ( size + 1) indice liste lexbuf }
	| pourcentB 		{ formatSize ( (int_of_string n) + size) (indice+1) liste lexbuf }
	| pourcentS		{ formatSize  size (indice + 1) (indice::liste) lexbuf }
	| eof			{ (size ,liste) }
	| _ 			{ failwith "optirg : Lexer de format : Un caractère inconnu a été rencontré. " }
