(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:08:52 by juloo             #+#    #+#             *)
(*   Updated: 2017/10/06 00:15:12 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open ComponentTmpl_T

let dummy : ('a, 'e, 'n) t =
	fun _ _ -> (fun _ -> (fun _ -> ()), fun () -> ()), fun () -> ()

let comp (tmpl : ('b, 'f, 'n) t) get set : ('a, 'e, 'n) t =
	fun data event_push ->
		let event_push e = event_push (set e) in
		let mount, deinit = tmpl (get data) event_push in
		let mount parent =
			let update, unmount = mount parent in
			let update data = update (get data) in
			update, unmount
		in
		mount, deinit


let seq = ComponentTmpl_seq.seq

include ComponentTmpl_switch.Case
let switch = ComponentTmpl_switch.switch
