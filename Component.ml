(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/03/15 17:35:56 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/06/21 22:35:49 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> 'a -> unit

let run (t, tasks) view update =
	let stream, push = Lwt_stream.create () in
	let push e = push (Some e) in
	let redraw = view t push in
	let rec loop t tasks =
		List.iter (fun t -> Lwt.on_success t push) tasks;
		redraw t;
		let%lwt event = Lwt_stream.next stream in
		match update t event with
		| `Loop (t, tasks)	-> loop t tasks
		| `Done r			-> Lwt.return r
	in
	loop t tasks
