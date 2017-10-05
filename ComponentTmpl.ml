(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:08:52 by juloo             #+#    #+#             *)
(*   Updated: 2017/10/05 22:50:07 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Tmpl (P : sig
	type node
end) =
struct

	type ('a, 'e) tmpl = ('a, 'e, P.node) ComponentTmpl_T.t

	let root parent tmpl =
		fun data event_push ->
			let mount, deinit = tmpl data event_push in
			let update, unmount = mount parent in
			let destroy () =
				unmount ();
				deinit ()
			in
			update, destroy


	let dummy _ _ = (fun _ -> (fun _ -> ()), fun () -> ()), fun () -> ()

	let comp view get set =
		fun data event_push ->
			let event_push e = event_push (set e) in
			let mount, deinit = view (get data) event_push in
			let mount parent =
				let update, unmount = mount parent in
				let update data = update (get data) in
				update, unmount
			in
			mount, deinit


	let seq = ComponentTmpl_seq.seq

	include ComponentTmpl_switch.Case
	let switch = ComponentTmpl_switch.switch

end
