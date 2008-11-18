(*
 * $Id: toc.ml,v 1.1 2008/11/18 15:39:33 casse Exp $
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

exception UnsupportedType of Irg.type_expr

(** Convert a type to C string.
	@param t	Type to convert.
	@return		C type as string.
	@raise UnsupportedType	If the type is not supported. *)
let rec convert_type t =
	match t with
	  Irg.NO_TYPE -> assert false
	| Irg.BOOL -> "uint8_t"
	| Irg.INT n when n <= 8 -> "int8_t"
	| Irg.INT n when n <= 16 -> "int16_t"
	| Irg.INT n when n <= 32 -> "int32_t"
	| Irg.INT n when n <= 64 -> "int64_t"
	| Irg.CARD n when n <= 8 -> "uint8_t"
	| Irg.CARD n when n <= 16 -> "uint16_t"
	| Irg.CARD n when n <= 32 -> "uint32_t"
	| Irg.CARD n when n <= 64 -> "uint64_t"
	| Irg.FLOAT (23, 9) -> "float"
	| Irg.FLOAT (52, 12) -> "double"
	| Irg.FLOAT (64, 16) -> "long double"
	| Irg.STRING -> "char *"
	| Irg.ENUM _ -> "uint32_t"
	| Irg.RANGE (_, m) ->
		convert_type (Irg.INT (int_of_float (ceil ((log (Int32.to_float m)) /. (log 2.)))))
	| _ -> raise (UnsupportedType t)
