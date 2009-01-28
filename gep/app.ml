(*
 * $Id: app.ml,v 1.4 2009/01/28 13:43:49 casse Exp $
 * Copyright (c) 2009, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of OGliss.
 *
 * OGliss is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * OGliss is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OGliss; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

module OrderedType = struct
	type t = Toc.c_type
	let compare s1 s2 = if s1 = s2 then 0 else if s1 < s2 then (-1) else 1
end
module TypeSet = Set.Make(OrderedType)

(** Gather information useful for the generation. *)
type maker_t = {
	mutable get_params: Irg.spec -> int -> string -> Irg.type_expr -> Templater.dict_t -> Templater.dict_t;
}


(* Test if memory or register attributes contains ALIAS.
	@param attrs	Attributes to test.
	@return			True if it contains "alias", false else. *)
let rec contains_alias attrs =
	match attrs with
	  [] -> false
	| (Irg.ALIAS _)::_ -> true
	| _::tl -> contains_alias tl


(** Format date (in seconds) and return a stirng.
	@param date	Date to format.
	@return		Date formatted as a string. *)
let format_date date =
	let tm = Unix.localtime date in
	Printf.sprintf "%0d/%02d/%02d %02d:%02d:%02d"
		tm.Unix.tm_year tm.Unix.tm_mon tm.Unix.tm_mday
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec


let out f = Templater.TEXT (fun out -> output_string out (f ()))

let get_params maker inst f dict =

	let get_type t =
		match t with
		  Irg.TYPE_EXPR t -> t
		| Irg.TYPE_ID n ->
			(match (Irg.get_symbol n) with
			  Irg.TYPE (_, t) -> t
			| _ -> Irg.NO_TYPE) in

	ignore (List.fold_left
		(fun (i: int) (n, t) ->
			let t = get_type t in
			(if t <> Irg.NO_TYPE then
				f (maker.get_params inst i n t (
					("PARAM", out (fun _ -> n)) ::
					("INDEX", out (fun _ -> string_of_int i)) ::
					("TYPE", out (fun _ -> Toc.type_to_string (Toc.convert_type t))) ::
					("PARAM_TYPE", out (fun _ -> String.uppercase (Toc.type_to_field (Toc.convert_type t)))) ::
					dict)));
			i + 1)
		0
		(Iter.get_params inst))

let get_instruction maker f dict _ i = f
	(("IDENT", out (fun _ -> Iter.get_name i)) ::
	("ICODE", Templater.TEXT (fun out -> Printf.fprintf out "%d" (Iter.get_id i))) ::
	("params", Templater.COLL (get_params maker i)) ::
	("has_param", Templater.BOOL (fun _ -> (List.length (Iter.get_params  i)) > 0)) ::
	("num_params", Templater.TEXT (fun out -> Printf.fprintf out "%d" (List.length (Iter.get_params i)))) ::
	dict)

let get_register f dict _ sym =
	match sym with
	  Irg.REG (name, size, t, attrs) -> f (
	  	("type", out (fun _ -> Toc.type_to_string (Toc.convert_type t))) ::
		("name", out (fun _ -> name)) ::
		("NAME", out (fun _ -> String.uppercase name)) ::
		("aliased", Templater.BOOL (fun _ -> contains_alias attrs)) ::
		("array", Templater.BOOL (fun _ -> size > 1)) ::
		("size", out (fun _ -> string_of_int size)) ::
		dict)	(* make_array size*)
	| _ -> ()

let get_value f dict t =
	f (
		("name", out (fun _ -> Toc.type_to_field t)) ::
		("type", out (fun _ -> Toc.type_to_string t)) ::
		dict
	)

let get_param f dict t =
	f (
		("NAME", out (fun _ -> String.uppercase (Toc.type_to_field t))) ::
		dict
	)

let get_memory f dict key sym = 
	match sym with
	  Irg.MEM (name, size, Irg.CARD(8), attrs) ->
	  	f (
			("NAME", out (fun _ -> String.uppercase name)) ::
			("name", out (fun _ -> name)) ::
			("aliased", Templater.BOOL (fun _ -> contains_alias attrs)) ::
			dict
		)
	| _ -> ()
	

let maker _ = {
	get_params = fun _ _ _ _ dict -> dict;
}

let make_env info maker =

	let param_types =
		let collect_field set (name, t) =
			match t with
			  Irg.TYPE_EXPR t -> TypeSet.add (Toc.convert_type t) set
			| Irg.TYPE_ID n ->
				(match (Irg.get_symbol n) with
				  Irg.TYPE (_, t) -> TypeSet.add (Toc.convert_type t) set
				| _ -> set) in
	
		let collect_fields set params =
			List.fold_left collect_field set params in
		Iter.iter (fun set i -> collect_fields set (Iter.get_params i)) TypeSet.empty in

	("instructions", Templater.COLL (fun f dict -> Iter.iter (get_instruction maker f dict) ())) ::
	("registers", Templater.COLL (fun f dict -> Irg.StringHashtbl.iter (get_register f dict ) Irg.syms)) ::
	("values", Templater.COLL (fun f dict -> TypeSet.iter (get_value f dict) param_types)) ::
	("params", Templater.COLL (fun f dict -> TypeSet.iter (get_param f dict) param_types)) ::
	("memories", Templater.COLL (fun f dict -> Irg.StringHashtbl.iter (get_memory f dict) Irg.syms)) ::
	("date", out (fun _ -> format_date (Unix.time ()))) ::
	("proc", out (fun _ -> info.Toc.proc)) ::
	("PROC", out (fun _ -> String.uppercase info.Toc.proc)) ::
	("version", out (fun _ -> "GLISS V2.0 Copyright (c) 2009 IRIT - UPS")) ::
	[]


(**
 * Load a NMP file and launch the given function on it.
 * @param file	File to process.
 * @param f		Function to work with definitions.
 *)
let process file f =
	try	
		begin
			let lexbuf = Lexing.from_channel (open_in file) in
			Parser.top Lexer.main lexbuf;
			let info = Toc.info () in
			f info
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
	| Toc.Error msg -> 
		Printf.fprintf stderr "ERROR: %s\n" msg;
		exit 4
	| Sys_error msg ->
		Printf.eprintf "ERROR: %s\n" msg; exit 1
	| Unix.Unix_error (err, _, path) ->
		Printf.fprintf stderr "ERROR: %s on \"%s\"\n" (Unix.error_message err) path
	| Failure e ->
		Lexer.display_error e; exit 3
