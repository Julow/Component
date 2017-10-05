(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_T.ml                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:30:54 by juloo             #+#    #+#             *)
(*   Updated: 2017/10/05 23:56:50 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'n action =
	| Insert of int * 'n
	| Replace of int * 'n
	| Delete of int

type ('a, 'e, 'n) t = 'a -> ('e -> unit) -> (
		('n action -> unit) -> ('a -> unit) * (unit -> unit)
	) * (unit -> unit)

let root parent (tmpl : ('a, 'e, 'n) t) : ('a, 'e) Component.root =
	fun data event_push ->
		let mount, deinit = tmpl data event_push in
		let update, unmount = mount parent in
		let destroy () =
			unmount ();
			deinit ()
		in
		update, destroy
