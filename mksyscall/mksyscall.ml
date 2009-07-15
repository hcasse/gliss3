(*
 * $Id: mksyscall.ml,v 1.1 2009/07/15 07:49:42 casse Exp $
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

open Frontc


(* Options *)
let banner =
	"mksyscall V1.0 (01/07/09)\n" ^
	"Copyright (c) 2009, IRIT - UPS <hugues.casse@laposte.net>\n\n" ^
	"SYNTAX:\tmksyscall [options] API\n" ^
	"\tmksyscall [options] --\n"
let args: parsing_arg list ref = ref []
let files: string list ref = ref []
let out_file = ref ""
let from_stdin = ref false
let api = ref ""

(* Options scanning *)
let opts = [
	("-o", Arg.Set_string out_file,
		"Output to the given file.");
	("-nogcc", Arg.Unit (fun _ -> args := (GCC_SUPPORT false) :: !args),
		"Do not use the GCC extensions.");
	("-i", Arg.String (fun file -> args := (INCLUDE file) :: !args),
		"Include the given file.");
	("-I", Arg.String (fun dir -> args := (INCLUDE_DIR dir) :: !args),
		"Include retrieval directory");
	("-D", Arg.String (fun def -> args := (DEF def) :: !args),
		"Pass this definition to the preprocessor.");
	("-U", Arg.String (fun undef -> args := (UNDEF undef) :: !args),
		"Pass this undefinition to the preprocessor.");
	("-s", Arg.String (fun sys -> args := (PREPROC (sys ^ "-gcc -E")) :: !args),
		"Emulated system");
	("-a", Arg.String (fun a ->
			api := a;
			args := USE_CPP :: (FROM_FILE (a ^ ".c")) :: !args),
		"Emulated API")
]


(* Named types *)
module HashString =
struct
	type t = string
	let equal (s1 : t) (s2 : t) = s1 = s2
	let hash (s : t) = Hashtbl.hash s
end
module StringHashtbl = Hashtbl.Make(HashString)
let named : Cabs.base_type StringHashtbl.t = StringHashtbl.create 211

let get_named name =
	StringHashtbl.find named name

let fill_named defs =
	List.iter
		(fun def ->
			match def with
			  Cabs.TYPEDEF ((_, _, names), _) ->
				List.iter (fun (name, typ, _, _) -> StringHashtbl.add named name typ) names
			| _ -> ())
		defs

(** Type of parameters *)
type param =
	  VOID
	| SIMPLE of Cabs.base_type


let rec get_type typ =
	match typ with
	  Cabs.NO_TYPE -> SIMPLE (Cabs.INT (Cabs.NO_SIZE, Cabs.NO_SIGN))
	| Cabs.VOID -> VOID
 	| Cabs.CHAR _ -> SIMPLE typ
 	| Cabs.INT _ -> SIMPLE typ
 	| Cabs.NAMED_TYPE name -> get_type (get_named name)
 	| Cabs.CONST typ -> get_type typ
 	| Cabs.VOLATILE typ -> get_type typ
 	| Cabs.GNU_TYPE (_, typ) -> get_type typ
 	| Cabs.TYPE_LINE (_, _, typ) -> get_type typ
	| _ -> failwith "unsupported type"


(** Find the syscall for the given *)
let rec get_syscall (attrs : Cabs.gnu_attrs) =
	match attrs with
	  [] -> -1
	| (Cabs.GNU_CALL ("syscall", [Cabs.GNU_CST (Cabs.CONST_INT n)]))::_ -> int_of_string n
	| _::tl -> get_syscall tl


(** Get the list of system calls found.
	@param defs		C definition
	@return			List of system calls. *)
let rec get_calls defs =
	match defs with
	  [] -> []
	| (Cabs.FUNDEF ((_, _, (name, Cabs.PROTO (rtype, params, _), attrs, _)), body))::tl ->
		(name, rtype, params, attrs)::(get_calls tl)
	| _::tl -> get_calls tl


(** Test if the attribute unsupported is set.
	@param attrs	Attribute to look in.
	@return			True if unsupported is set, false else. *)
let rec is_unsupported attrs =
	match attrs with
	  [] -> false
	| (Cabs.GNU_ID "unsupported")::_ -> true
	| _::tl -> is_unsupported tl


(** Test if the attribute unsupported is set.
	@param attrs	Attribute to look in.
	@return			True if unsupported is set, false else. *)
let rec is_only_args attrs =
	match attrs with
	  [] -> false
	| (Cabs.GNU_ID "only_args")::_ -> true
	| _::tl -> is_unsupported tl


(** Test if the attribute unsupported is set.
	@param attrs	Attribute to look in.
	@return			True if unsupported is set, false else. *)
let rec is_only_emulate attrs =
	match attrs with
	  [] -> false
	| (Cabs.GNU_ID "only_emulate")::_ -> true
	| _::tl -> is_unsupported tl


(** Perform declaration of a parameter.
	@param out		Out channel.
	@param param	Parameter to declare. *)
let declare_param out param =
	let (_, _, (name, typ, attrs, _)) = param in
	()


(* Main Program *)
let _ =
	let on_error msg =
		Printf.fprintf stderr "ERROR: %s\n" msg;
		exit 1 in

	(* Parse arguments *)
	Arg.parse opts (fun arg -> on_error ("bad argument: " ^ arg)) banner;
	if !api = "" then on_error "select an API";

	(* read the C definitions *)
	let defs = match Frontc.parse !args with
		  PARSING_ERROR ->  on_error "syntax erro"
		| PARSING_OK defs -> defs in

	(* get system calls *)
	fill_named defs;
	let calls = get_calls defs in

	(* open the output *)
	let (out, close) =
		if !out_file = "" then (stdout,false)
		else ((open_out !out_file), true) in

	(* generate syscall identifiers *)
	Printf.fprintf out "\n/* syscall identifiers */\n";
	List.iter
		(fun (name, _, _, attrs) ->
			Printf.fprintf out "#define __SYSCALL_%s %d\n" name (get_syscall attrs))
		calls;

	(* generate emulation functions *)
	List.iter
		(fun (name, rtype, params, attrs) ->
			Printf.fprintf out "\nBOOL gliss_syscall_%s(gliss_state_t *state) {\n" name;
			if is_unsupported attrs
			then Printf.fprintf out "\tRETURN(-1); return FALSE;\n"
			else ();
			Printf.fprintf out "}\n"
		)
		calls;

	(* generate the gliss_syscall function *)
	Printf.fprintf out "\n/* gliss_syscall function */\n";
	Printf.fprintf out "void gliss_syscall(gliss_inst_t *inst, gliss_state_t *state) {\n";
	Printf.fprintf out "	int syscall_num;\n";
	Printf.fprintf out "	BOOL ret = FALSE;;\n\n";
	Printf.fprintf out "	syscall_num = GLISS_SYSCALL_CODE(inst, state);\n";
	Printf.fprintf out "	if(verbose)\n";
	Printf.fprintf out "		fprintf(verbose, \"got a system call (number : %%u; name : %%s)\\n\", syscall_num, ppc_get_syscall_name(syscall_num));\n";
	Printf.fprintf out "	switch(syscall_num) {\n";
	List.iter (fun (name, _, _, _) ->
			Printf.fprintf out "	case __SYSCALL_%s: ref = gliss_%s(state); break;\n" name name)
		calls;
	Printf.fprintf out "	}\n";
	Printf.fprintf out "	if(!ret) {\n";
	Printf.fprintf out "		if(verbose)\n";
	Printf.fprintf out "			fprintf(verbose, \"Warning : system call returns an error (number : %%u, name : %%s)\\n\", syscall_num, ppc_get_syscall_name(syscall_num));\n";
	Printf.fprintf out "		gliss_sysparm_failed(state);\n";
	Printf.fprintf out "	}\n";
	Printf.fprintf out "	else\n";
	Printf.fprintf out "		gliss_sysparm_succeed(state);\n";
	Printf.fprintf out "}\n\n";

	(* close the output if needed *)
	if close then close_out out

