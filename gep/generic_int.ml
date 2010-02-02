
type gen_int =
{
	(* number of bits *)
	length : int;
	(* list representing the number cut in 32 bit slices, the head is the lowest bits, the tail is the upper bits,
	   only the lowest bits of the tail are significative (the first 'length % 32' bits) *)
	number   : Int32.t list;
}

let length v = v.length

(* add a given Int32 a certain number of times to the left of a given list,
typically used to add zeros when resizing *)
let make_Int32_list num size start =
	let rec aux size_left accu =
		if size_left = 0 then
			accu
		else
			accu @ [num]
	in
	aux size start


let rec power_of_2_i32 n =
	if n = 0 then
		Int32.one
	else
		if n > 31 then
			Int32.zero (* return the 32 lsb of the result *)
		else
			Int32.shift_left (power_of_2_i32 (n-1)) 1

let mask_i32 n =
	Int32.sub (power_of_2_i32 n) Int32.one


(* for these functions dealing with shift, the shift value is always between 0 and 32 *)

(* return the result and the lost bits right justified in a 2-tuple *)
let left_shift_with_lost_bits_i32 i shift_val accu =
	let mask = mask_i32 shift_val
	in
	( Int32.logor (Int32.shift_left i shift_val) (Int32.logand accu mask),
	Int32.shift_right_logical (Int32.logand i (Int32.shift_left mask (32 - shift_val))) (32 - shift_val) )

(* return the bits lost during a left shift *)
let remain_of_left_shift_i32 i shift_val =
	Int32.shift_right_logical (Int32.logand i (Int32.shift_left (mask_i32 shift_val) (32 - shift_val))) (32 - shift_val)

(* return the result and the lost bits right justified in a 2-tuple *)
let right_shift_with_lost_bits_i32 i shift_val accu =
	let mask = mask_i32 shift_val
	in
	( Int32.logor (Int32.shift_right_logical i shift_val) (Int32.shift_left (Int32.logand accu mask) (32 - shift_val)),
	Int32.logand i mask )

(* return the bits lost during a right shift *)
let remain_of_right_shift_i32 i shift_val =
	Int32.logand i (mask_i32 shift_val)
	

let left_shift_int32_list l n =
	let rec aux i32_list shift_val accu =
		match i32_list with
		[] ->
			[]
		| a::b ->
			let (res, lost_bits) = left_shift_with_lost_bits_i32 a shift_val accu
			in
			(match b with
			[] ->
				res :: [lost_bits]
			| _ ->
				res :: (aux b shift_val lost_bits)
			)
	in
	aux l n Int32.zero
	

let right_shift_int32_list l n =
	let rec aux i32_list shift_val accu =
		match i32_list with
		[] ->
			[]
		| a::b ->
			let (res, lost_bits) = right_shift_with_lost_bits_i32 a shift_val accu
			in
			(match b with
			[] ->
				[ res ]
			| _ ->
				res :: (aux b shift_val lost_bits)
			)
	in
	List.rev (aux (List.rev l) n Int32.zero)

(* keep only the first n bits, unused bits get zeroed or dropped *)
let rec clean_i32_list l n =
	match l with
	[] ->
		[]
	| a::b ->
		if n <= 32 then
			[ Int32.logand a (mask_i32 n)]
		else
			a :: (clean_i32_list b (n - 32))


(* add 0s in msb to meet new size *)
let expand_gen_int gi new_size =
	let size = gi.length
	in
	let diff = new_size - size
	in
	(* overestimated but safe and corrected by cleaning afterwards *)
	let msb_to_add = diff / 32 + 1
	in
	let rec add_zero_msb l n =
		if n == 0 then
			l
		else
			(add_zero_msb l (n - 1)) @ [ Int32.zero ]
	in
	{
		length = new_size;
		number = clean_i32_list (add_zero_msb gi.number msb_to_add) new_size
	}

let zero =
	{
		length = 1;
		number = [ Int32.zero ];
	}

let one =
	{
		length = 1;
		number = [ Int32.one ];
	}

let lognot gi =
	{
		length = gi.length;
		number = clean_i32_list (List.map Int32.lognot gi.number) gi.length;
	}

let logand gi1 gi2 =
	let arg1 = if gi1.length < gi2.length then expand_gen_int gi1 gi2.length else gi1
	in
	let arg2 = if gi1.length > gi2.length then expand_gen_int gi2 gi1.length else gi2
	in
	{
		length = arg1.length;
		number = List.map2 Int32.logand arg1.number arg2.number;
	}

let logor gi1 gi2 =
	let arg1 = if gi1.length < gi2.length then expand_gen_int gi1 gi2.length else gi1
	in
	let arg2 = if gi1.length > gi2.length then expand_gen_int gi2 gi1.length else gi2
	in
	{
		length = arg1.length;
		number = List.map2 Int32.logor arg1.number arg2.number;
	}

let is_equals gi1 gi2 =
	if gi1.length != gi2.length then
		false
	else
		List.for_all2 (fun x y -> ((Int32.compare x y) == 0) ) gi1.number gi2.number

let get_lowest_bit gi = 
	if gi.length < 1 then
		failwith "cannot get lowest bit from a too small number"
	else
		Int32.logand (List.nth gi.number 0) Int32.one

let set_lowest_bit gi i32 =
	let set_bit _ =
		Int32.logor (Int32.logand i32 (Int32.lognot Int32.one)) (Int32.logand i32 Int32.one)
	in
	let build_i32_list _ =
		match gi.number with
		[] ->
			[]
		| a::b ->
			(set_bit ())::b
	in
	let res =
		{
			length = gi.length;
			number = build_i32_list ();
		}
	in
	if gi.length < 1 then
		failwith "cannot set lowest bit from a too small number"
	else
		res

let shift_left gi n =
	let lsb_zeros = n / 32
	in
	let rec add_lsb_zeros_to_list l num =
		if num == 0 then
			l
		else
			Int32.zero :: (add_lsb_zeros_to_list l (num - 1))
	in
	let shift_val = n mod 32
	in
	if n < 0 then
		failwith "negative shift (generic_int.ml::gen_shift_left)"
	else
		if n == 0 then
			gi
		else
			{
				length = gi.length + n;
				number = clean_i32_list
					(left_shift_int32_list (add_lsb_zeros_to_list gi.number lsb_zeros) shift_val)
					(gi.length + n);
			}


let shift_right_logical gi n =
	let dropped_lsb = n / 32
	in
	let rec drop_lsb_from_list l num =
		if num == 0 then
			l
		else
			(match l with
			a::b ->
				drop_lsb_from_list b (num - 1)
			| [] ->
				[]
			)
	in
	let shift_val = n mod 32
	in
	if n < 0 then
		failwith "negative shift (generic_int.ml::gen_shift_right)"
	else
		if n == 0 then
			gi
		else
			{
				length = gi.length + n;
				number = clean_i32_list 
					(right_shift_int32_list (drop_lsb_from_list gi.number dropped_lsb) shift_val)
					(gi.length + n);
			}

