(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ChildList.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/11/04 17:37:48 by juloo             #+#    #+#             *)
(*   Updated: 2017/11/04 17:48:02 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open ComponentTmpl_T
open Utils

type ('a, 'n) mounted_child = {
	mutable length : int;
	update : 'a -> unit;
	unmount : unit -> unit;
	unmounted : ('a, 'n) unmounted
}

(* Represent a mounted list of child *)
(* The same record is also used to iter on childs *)
(* `index` is the index of the current child *)
(* `offset` is the sum of the lengths of the childs before `index` *)
type ('a, 'n) mounted = {
	mutable childs : ('a, 'n) mounted_child array;
	mutable index : int;
	mutable offset : int;
	parent : 'n action -> unit
}

let begin_iter t =
	t.offset <- 0;
	t.index <- 0

let iter t = t.index < Array.length t.childs

let inc t =
	let i = t.index in
	t.offset <- t.offset + t.childs.(i).length;
	t.index <- i + 1

let update t data =
	let offset = t.offset
	and child = t.childs.(t.index) in
	child.update data;
	let delta = t.offset - offset in
	child.length <- child.length + delta;
	inc t

let mount_child t (mount, _ as unmounted) =
	let offset = t.offset in
	let update, unmount = mount t.parent in
	let length = t.offset - offset in
	{ length; update; unmount; unmounted }

let append t childs =
	for _ = t.index to Array.length t.childs - 1 do inc t done;
	let childs = Array.map (mount_child t) childs in
	t.childs <- Array.append t.childs childs;
	t.index <- Array.length t.childs

let insert t childs =
	let childs = Array.map (mount_child t) childs in
	t.childs <- array_insert t.childs t.index childs;
	t.index <- t.index + Array.length childs

let remove t i n =
	for i = i to i + n - 1 do
		let offset = t.offset in
		let child = t.childs.(i) in
		child.unmount ();
		assert (offset - t.offset = child.length);
		t.offset <- offset
	done;
	let removed = Array.init n (fun i' ->
		t.childs.(i + i').unmounted
	) in
	t.childs <- array_remove t.childs i n;
	removed

let truncate t = remove t t.index (Array.length t.childs - t.index)

let length t = Array.length t.childs

let create childs =
	let childs = ref childs in
	let mount parent' =

		let rec t = {
			childs = [||];
			index = 0;
			offset = 0;
			parent
		}

		and parent =
			let parent d action =
				parent' action;
				t.offset <- t.offset + d
			in
			function
			| Insert (i, n)		-> parent ~+1 @@ Insert (i + t.offset, n)
			| Replace (i, n)	-> parent ~+0 @@ Replace (i + t.offset, n)
			| Delete i			-> parent ~-1 @@ Delete (i + t.offset)
		in

		let unmount () =
			t.index <- 0;
			t.childs |> Array.iter (fun c ->
				t.offset <- 0;
				c.unmount ()
			);
			childs := Array.map (fun c -> c.unmounted) t.childs
		in

		t.childs <- Array.map (mount_child t) !childs;
		childs := [||];
		t, unmount

	and deinit () =
		Array.iter (fun (_, deinit) -> deinit ()) !childs
	in
	mount, deinit
