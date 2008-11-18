(*
 * $Id: gep.ml,v 1.2 2008/11/18 15:39:33 casse Exp $
 * Copyright (c) 2008, IRIT - UPS <casse@irit.fr>
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
open Lexing

exception UnsupportedMemory of Irg.spec

module OrderedString = struct
	type t = string
	let compare s1 s2 = String.compare s1 s2
end
module StringSet = Set.Make(OrderedString)


(* options *)
let options = []
let nmp: string ref = ref ""

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


(** Build the given directory.
	@param path			Path of the directory.
	@raise Sys_error	If there is an error. *)
let makedir path =
	if not (Sys.file_exists path) then
		try 
			Unix.mkdir path 0o740
		with Unix.Unix_error (code, _, _) ->
			raise (Sys_error (Printf.sprintf "cannot create \"%s\": %s" path (Unix.error_message code)))
	else
		let stats = Unix.stat path in
		if stats.Unix.st_kind <> Unix.S_DIR
		then raise (Sys_error (Printf.sprintf "cannot create directory \"%s\": file in the middle" path))


(** Get the processor name.
	@return	Processor name.
	@raise	Sys_error	Raised if the proc is not defined. *)
let get_proc _ =
	match Irg.get_symbol "proc" with
	  Irg.LET(_, Irg.STRING_CONST name) -> name
	| _ -> raise (Sys_error "no \"proc\" definition available")


(** Format date (in seconds) and return a stirng.
	@param date	Date to format.
	@return		Date formatted as a string. *)
let format_date date =
	let tm = Unix.localtime date in
	Printf.sprintf "%0d/%02d/%02d %02d:%02d:%02d"
		tm.Unix.tm_year tm.Unix.tm_mon tm.Unix.tm_mday
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
	

(** Build the file "proc/include/id.h"
	@param proc		Name of the processor. *)
let make_id_h out proc =
	let uproc = String.uppercase proc in
	Printf.fprintf out "\n/* instruction codes */\n";
	Iter.iter
		(fun _ i -> Printf.fprintf
			out
			"#define %s_%s %d\n"
			uproc
			(Iter.get_name i)
			(Iter.get_id i))
		()


(** Build the XXX/include/api.h file. *)
let make_api_h out proc =
	let uproc = String.uppercase proc in

	let rec contains_alias attrs =
		match attrs with
		  [] -> false
		| (Irg.ALIAS _)::_ -> true
		| _::tl -> contains_alias tl in

	let make_array size =
		if size = 1 then ""
		else Printf.sprintf "[%d]" size in

	let make_state k s =
		match s with
		  Irg.MEM (name, size, Irg.CARD(8), attrs)
		  when not (contains_alias attrs) ->
			Printf.fprintf out "\tgliss_memory_t *%s;\n" name
		| Irg.MEM _ ->
			raise (UnsupportedMemory s)
		| Irg.REG (name, size, t, attrs)
		when not (contains_alias attrs) ->
			Printf.fprintf out "\t%s %s%s;\n" (Toc.convert_type t) name (make_array size)
		| _ -> () in

	let make_field t =
		match t with
		  "int8_t" -> "int8"
		| "int16_t" -> "int16"
		| "int32_t" -> "int32"
		| "int64_t" -> "int64"
		| "uint8_t" -> "uint8"
		| "uint16_t" -> "uint16"
		| "uint32_t" -> "uint32"
		| "uint64_t" -> "uint64"
		| "float" -> "_float"
		| "double" -> "_double"
		| "long double" -> "_long_double"
		| "char *" -> "string"
		| _ -> failwith "unsupported" in

	let collect_field set (name, t) =
		match t with
		  Irg.TYPE_EXPR t -> StringSet.add (Toc.convert_type t) set
		| Irg.TYPE_ID n ->
			(match (Irg.get_symbol n) with
			  Irg.TYPE (_, t) -> StringSet.add (Toc.convert_type t) set
			| _ -> set) in
	
	let collect_fields set params =
		List.fold_left collect_field set params in
	
	let make_reg_param _ spec =
		match spec with
		  Irg.REG (name, size, t, attrs) ->
			Printf.fprintf out ",\n\t%s_%s_T" uproc (String.uppercase name)
		| _ -> () in		
	
	(* output includes *)
	Printf.fprintf out "\n#include <stdint.h>\n";
	Printf.fprintf out "\n#include <gliss/memory.h>\n";

	(* xxx_state_t typedef generation *)
	Printf.fprintf out "\n/* %s_state_t type */\n" proc;
	Printf.fprintf out "typedef %s_state_t {\n" proc;
	Irg.StringHashtbl.iter make_state Irg.syms;
	Printf.fprintf out "} %s_state_t;\n" proc;

	(* output xxx_value_t *)
	Printf.fprintf out "\n/* %s_value_t type */\n" proc;
	Printf.fprintf out "typedef union %s_value_t {\n" proc;
	let set = 
		Iter.iter (fun set i -> collect_fields set (Iter.get_params i)) StringSet.empty in
	StringSet.iter
		(fun t -> Printf.fprintf out "\t%s %s;\n" t (make_field t))
		set;
	Printf.fprintf out "} %s_value_t;\n" proc;
	
	(* output xxx_param_t *)
	Printf.fprintf out "\n/* %s_param_t type */\n" proc;
	Printf.fprintf out "typedef enum %s_param_t {\n" proc;
	Printf.fprintf out "\tVOID_T = 0";
	StringSet.iter
		(fun t -> Printf.fprintf out ",\n\t%s_PARAM_%s_T"
			uproc (String.uppercase (make_field t)))
		set;
	Irg.StringHashtbl.iter make_reg_param Irg.syms;
	Printf.fprintf out "\n} %s_param_t;\n" proc
	
	(* output xxx_ii_t *)
	
	(* output xxx_inst_t *)


(** Build an include file.
	@param f	Function to generate the content of the file.
	@param proc	Processor name.
	@param file	file to create.
	@param dir	Directory containing the includes.
	@raise Sys_error	If the file cannot be created. *)
let make_include f proc file dir =
	let uproc = String.uppercase proc in
	let out = open_out (dir ^ "/" ^ file ^ ".h") in
	
	(* output header *)
	let def = Printf.sprintf "GLISS_%s_%s_H" uproc (String.uppercase file) in
	Printf.fprintf out "/* Generated by gep (%s) copyright (c) 2008 IRIT - UPS */\n" (format_date (Unix.time ()));
	Printf.fprintf out "#ifndef %s\n" def;
	Printf.fprintf out "#define %s\n" def;
	
	(* output the content *)
	f out proc;
		
	(* output tail *)
	Printf.fprintf out "\n#endif /* %s */\n" def;
	close_out out
	


(* main program *)
let _ =
	try	
		begin
			let lexbuf = Lexing.from_channel (open_in !nmp) in
			Parser.top Lexer.main lexbuf;
			let proc = get_proc () in
			makedir proc;
			let include_path = proc ^ "/include" in
			makedir include_path;
			make_include make_id_h proc "id" include_path;
			make_include make_api_h proc "api" include_path
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
	| Sys_error msg ->
		Printf.eprintf "ERROR: %s\n" msg; exit 1
	| Failure e ->
		Lexer.display_error e; exit 3

