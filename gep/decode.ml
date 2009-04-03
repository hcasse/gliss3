(* return the mask giving the nth param (counting from 0) of an instr of the given spec, the result will be a string
with only '0' or '1' chars representing the bits of the mask,
the params' order is the one given by the Iter.get_params method *)
let get_string_mask_for_param_from_op sp n =
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
	let rec get_frmt_params e =
		match e with
		Irg.FORMAT(_, params) ->
			params
		| Irg.ELINE(_, _, e) ->
			get_frmt_params e
		| _ ->
			failwith "(Decode) can't find the params of a given (supposed) format expr"
	in
	let image_attr =
		get_expr_from_iter_value (Iter.get_attr sp "image")
	in
	let frmt_params =
		get_frmt_params image_attr
	in
	let str_params =
		remove_space (Fetch.get_str image_attr)
	in
	let rec get_name_of_param e =
		match e with
		Irg.FIELDOF(_, ee, _) ->
			ee
		| Irg.REF(name) ->
			name
		| Irg.ELINE (_, _, ee) ->
			get_name_of_param ee
		| _ ->
			failwith "(Decode) can't find the base name of smtg not a fieldof or ref (for the moment)"
	in
	let get_rank_of_named_param n =
		let rec aux nn i p_l =
			match p_l with
			[] ->
				failwith ("(Decode) can't find rank of param "^nn^" in the format params")
			| a::b ->
				if nn=(get_name_of_param a) then
					i
				else
					aux nn (i+1) b
		in
		aux n 0 frmt_params
	in
	let rec get_i_th_param_name i l =
		match l with
		[] ->
			failwith "(Decode) can't find name of i_th param of a spec"
		| a::b ->
			if i=0 then
				Irg.get_name a
			else
				get_i_th_param_name (i-1) b
	in
	change_i_th_param (Str.full_split (Str.regexp "%[0-9]*[bdfxs]") (str_params)) (get_rank_of_named_param (get_i_th_param_name n (Iter.get_params sp)))



