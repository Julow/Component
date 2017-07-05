(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/03/15 17:35:56 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/07/05 22:19:35 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> (
		unit -> ('a -> unit) * (unit -> unit)
	) * (unit -> unit)

let run (t, tasks) view update =
	let stream, push = Lwt_stream.create () in
	let push e = push (Some e) in
	let mount, deinit = view t push in
	let redraw, unmount = mount () in
	let rec loop t tasks =
		List.iter (fun t -> Lwt.on_success t push) tasks;
		redraw t;
		let%lwt event = Lwt_stream.next stream in
		match update t event with
		| `Loop (t, tasks)	-> loop t tasks
		| `Done r			-> Lwt.return r
	in
	let r = loop t tasks in
	unmount ();
	deinit ();
	r
