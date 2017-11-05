(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_group.ml                             :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/11/05 19:58:40 by juloo             #+#    #+#             *)
(*   Updated: 2017/11/05 19:58:53 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open ComponentTmpl_T

let group (childs : ('b, 'e, 'n) t list) : ('b, 'e, 'n) t =
	let childs = Array.of_list childs in
	fun data event_push ->
		let childs = Array.map (fun c -> c data event_push) childs in
		let mount_childs, deinit_childs = ChildList.create childs in
		let mount parent =
			let childs, unmount_childs = mount_childs parent in
			let update data =
				ChildList.begin_iter childs;
				while ChildList.iter childs do
					ChildList.update childs data
				done
			in
			update, unmount_childs
		in
		mount, deinit_childs
