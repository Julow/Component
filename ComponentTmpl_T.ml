(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_T.ml                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:30:54 by juloo             #+#    #+#             *)
(*   Updated: 2017/11/03 00:20:54 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'n action =
	| Insert of int * 'n
	| Replace of int * 'n
	| Delete of int

type 'a mounted = ('a -> unit) * (unit -> unit)
type ('a, 'n) mount = ('n action -> unit) -> 'a mounted
type ('a, 'n) unmounted = ('a, 'n) mount * (unit -> unit)

type ('a, 'e, 'n) t = 'a -> ('e -> unit) -> ('a, 'n) unmounted

let root parent (tmpl : ('a, 'e, 'n) t) : ('a, 'e) Component.root =
	fun data event_push ->
		let mount, deinit = tmpl data event_push in
		let update, unmount = mount parent in
		let destroy () =
			unmount ();
			deinit ()
		in
		update, destroy
