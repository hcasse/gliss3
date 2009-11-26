(*
 * $Id$
 * Copyright (c) 2009, IRIT - UPS <casse@irit.fr>
 *
 * This file is part of OGliss.
 *
 * GLISS2 is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * GLISS2 is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GLISS2; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

open Irg

let uniq_ids: int Absint.StatHashtbl.t = Absint.StatHashtbl.create 211
let uniq_id = ref 0
let get_id stat =
	try
		Absint.StatHashtbl.find uniq_ids stat
	with Not_found ->
		let id = !uniq_id in
		Absint.StatHashtbl.add uniq_ids stat id;
		incr uniq_id;
		id

module OrderedStat = struct
	type t = stat
	let compare s1 s2 = (get_id s1) - (get_id s2)
end
module StatSet = Set.Make(OrderedStat)

module DefSet = struct
	type t = StatSet.t
	let undef = StatSet.singleton NOP
	let any = StatSet.empty
	let includes s1 s2 = StatSet.subset s2 s1
	let join s1 s2 = StatSet.union s1 s2
	let output out s =
		output_string out "{ ";
		StatSet.iter (fun e -> output_statement out e; output_string out ", ") s;
		output_string out " }";
end
module State = State.Make(DefSet)

module Problem = struct
	type init = unit
	type ctx = unit
	type t = State.t

	let null _ = State.empty
	let make _ = ()

	let update _ state stat =
		let rec set l s =
			match l with
			| LOC_NONE -> s
			| LOC_CONCAT (_, l1, l2) ->
				set l2 (set l1 s)
			| LOC_REF (_, id, NONE, _, _) ->
				State.set s id 0 (StatSet.singleton stat)
			| LOC_REF (_, id, ix, _, _) ->
				try State.set s id (Sem.to_int (Sem.eval_const ix)) (StatSet.singleton stat)
				with Sem.SemError _ -> State.set_all s id StatSet.empty in
		match stat with
		| SET (l, e)
		| SETSPE(l, e) -> set l state
		| _ -> state

	let includes s1 s2 = State.includes s1 s2
	let observe _ state stat = state
	let disjoin _ stat state = (state, state)
	let join _ stat s1 s2 = State.join s1 s2
end
