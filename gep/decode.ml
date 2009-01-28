(* return the mask giving the nth param (counting from 0) of an instr of the given spec, the result will be a string
with only '0' or '1' chars representing the bits of the mask *)
let get_string_mask_for_param_from_op sp n =
	let get_str e =
		match e with
		Irg.FORMAT(str, _) ->
			str
		| Irg.CONST(t_e, c) ->
			if t_e=Irg.STRING then
				match c with
				Irg.STRING_CONST(str) ->
					str
				| _ -> ""
			else
				""
		| _ ->
			""
	in
	let get_length_from_format f =
		let l = String.length f in
		let new_f =
			if l<=2 then
			(* shouldn't happen, we should have only formats like %[0-9]*b, not %d or %f *)
				"0"
			else
				String.sub f 1 (l-2)
		in
		Scanf.sscanf new_f "%d" (fun x->x)
	in
	let remove_space s =
		let rec concat_str_list s_l =
			match s_l with
			[] ->
				""
			| h::q ->
				h ^ (concat_str_list q)
		in
		concat_str_list (Str.split (Str.regexp "[ \t]+") s)
	in
	let rec change_i_th_param l i =
		match l with
		[] ->
			""
		| a::b ->
			(match a with
			Str.Delim(d) ->
				if i = 0 then
					(String.make (get_length_from_format d) '1') ^ (change_i_th_param b (i-1))
				else
					(String.make (get_length_from_format d) '0') ^ (change_i_th_param b (i-1))
			| Str.Text(txt) ->
				(String.make (String.length txt) '0') ^ (change_i_th_param b i)
			)
	in
	let get_expr_from_iter_value v  =
		match v with
		Iter.EXPR(e) ->
			e
		| _ -> Irg.NONE
	in
	change_i_th_param (Str.full_split (Str.regexp "%[0-9]*[bdfxs]") (remove_space (get_str (get_expr_from_iter_value (Iter.get_attr sp "image"))))) n



