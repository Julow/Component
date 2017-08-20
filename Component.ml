(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/03/15 17:35:56 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/08/20 20:43:04 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> (
		unit -> ('a -> unit) * (unit -> unit)
	) * (unit -> unit)

let run (t, tasks) view update =
	let pull, push =
		let stream, push = Lwt_stream.create () in
		let pull () = Lwt_stream.next stream in
		let push e = push (Some e) in
		pull, push
	in
	let redraw, destroy =
		let mount, deinit = view t push in
		let redraw, unmount = mount () in
		let destroy () =
			unmount ();
			deinit ()
		in
		redraw, destroy
	in
	let pull_tasks = List.iter (fun t -> Lwt.on_success t push) in
	let rec loop t =
		let%lwt event = pull () in
		match update t event with
		| `Loop (t, tasks)	->
			pull_tasks tasks;
			redraw t;
			loop t
		| `Done r			->
			destroy ();
			Lwt.return r
	in
	pull_tasks tasks;
	loop t
