(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_seq.ml                               :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:18:18 by juloo             #+#    #+#             *)
(*   Updated: 2017/11/05 21:30:09 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open ComponentTmpl_T
open Utils

let seq f (tmpl : ('b, 'e, 'n) t) : ('a, 'e, 'n) t =
	fun data event_push ->

		let tmpl data = tmpl data event_push in

		let childs = ref [] in
		f (fun data -> childs := tmpl data :: !childs) data;
		let childs = array_rev_of_list !childs in

		let mount_childs, deinit_childs = ChildList.create childs in
		let mount parent =
			let childs, unmount_childs = mount_childs parent in

			let update data =
				ChildList.begin_iter childs;
				let append = ref [] in
				f (fun data ->
					if ChildList.iter childs then
						ChildList.update childs data
					else
						append := tmpl data :: !append
				) data;
				if !append <> [] then
					ChildList.append childs (array_rev_of_list !append)
				else if ChildList.iter childs then
					ChildList.truncate childs
					|> Array.iter (fun (_, deinit) -> deinit ())
				else
					()

			in
			update, unmount_childs

		in
		mount, deinit_childs
