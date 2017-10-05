(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/03/15 17:35:56 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/10/06 00:26:55 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) root = 'a -> ('e -> unit) -> ('a -> unit) * (unit -> unit)

type ('a, 'e) task = Task of (unit -> ('a, 'e) controler Lwt.t)
and ('a, 'e) loop = 'e * ('a, 'e) task list
and ('a, 'e) controler = 'a -> ('a, 'e) loop

let run (t, tasks) root update =
	let pull, push =
		let stream, push = Lwt_stream.create () in
		let pull () = Lwt_stream.next stream in
		let push e = push (Some e) in
		pull, push
	in
	let redraw, destroy = root t push in
	let pull_tasks = List.iter (fun (Task t) -> Lwt.on_success (t ()) push) in
	let rec loop t =
		let%lwt event = pull () in
		let t, tasks = update t event in
		pull_tasks tasks;
		redraw t;
		loop t
	in
	try
		pull_tasks tasks;
		loop t
	with e ->
		destroy ();
		raise e
