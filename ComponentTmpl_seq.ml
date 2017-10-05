(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_seq.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:18:18 by juloo             #+#    #+#             *)
(*   Updated: 2017/10/06 00:07:49 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open ComponentTmpl_T

type 'a seq_child = {
	mutable length	: int;
	update			: 'a -> unit;
	unmount			: unit -> unit;
	deinit			: unit -> unit
}

let seq f (tmpl : ('b, 'e, 'n) t) : ('a, 'e, 'n) t =
	fun data event_push ->
		let mount parent =
			let offset = ref 0 in
			let index = ref 0 in
			let inserted = ref 0 in
			let childs = ref [||] in
			let pushed = ref [] in

			let parent =
				let parent d a = inserted := !inserted + d; parent a in
				function
				| Insert (i, n)		-> parent ~+1 (Insert (i + !offset, n))
				| Delete i			-> parent ~-1 (Delete (i + !offset))
				| Replace (i, n)	-> parent ~+0 (Replace (i + !offset, n))
			in

			let create_child data =
				let mount, deinit = tmpl data event_push in
				let update, unmount = mount parent in
				let c = { length = 0; update; unmount; deinit } in
				c
			in

			let push data =
				let i = !index in
				inserted := 0;
				let child =
					try
						let c = !childs.(!index) in
						c.update data;
						c
					with Invalid_argument _ ->
						let c = create_child data in
						pushed := c :: !pushed;
						c
				in
				let l = child.length + !inserted in
				child.length <- l;
				offset := !offset + l;
				index := i + 1
			in

			let remove_tail from =
				for i = from to Array.length !childs - 1 do
					let c = !childs.(i) in
					c.unmount ();
					c.deinit ()
				done;
				childs := Array.sub !childs 0 from
			in

			let update data =
				offset := 0;
				index := 0;
				f push data;
				childs := Array.(List.rev !pushed |> of_list |> append !childs);
				pushed := [];
				remove_tail !index;
			in

			let unmount () =
				offset := 0;
				remove_tail 0
			in

			update data;
			update, unmount
		in
		mount, (fun () -> ())
