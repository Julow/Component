(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_switch.ml                            :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:18:18 by juloo             #+#    #+#             *)
(*   Updated: 2017/10/05 22:56:41 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

open ComponentTmpl_T

type switch = unit

module Case =
struct

	type ('e, 'n) case = {
		case : 'a. ('a, 'e, 'n) t -> 'a -> switch
	}

end

open Case

let switch f =

	fun data event_push ->

		let current_id = ref 0 in
		let mount_current = ref (fun _ -> assert false) in
		let unmount_current = ref (fun () -> ()) in
		let parent_mounted = ref None in
		let deinit_all =  ref (fun () -> ()) in

		(* Calls `init` with `data`, register the deinit function and returns `mount` *)
		let init_tmpl init data =
			let mount, deinit = init data event_push in
			let deinit_prev = !deinit_all in
			deinit_all := (fun () -> deinit (); deinit_prev ());
			mount
		in

		let mount_tmpl mount parent =
			let update, unmount = mount parent in
			update, unmount, mount
		in

		let case_update state data =
			match !parent_mounted, !state with
			| Some parent, `Tmpl init			->
				state := `Mounted (mount_tmpl (init_tmpl init data) parent)
			| None, `Tmpl init					->
				state := `First_initied (init_tmpl init data)
			| Some parent, `First_initied mount	->
				state := `Mounted (mount_tmpl mount parent)
			| Some parent, `Initied mount		->
				let update, _, _ as mounted = mount_tmpl mount parent in
				state := `Mounted mounted;
				update data
			| Some _, `Mounted (update, _, _)	->
				update data
			| _									->
				failwith "Switch update: unexpected state"
		in

		let case_mount state parent =
			match !state with
			| `First_initied mount
			| `Initied mount	->
				state := `Mounted (mount_tmpl mount parent)
			| _					->
				failwith "Switch mount: unexpected state"
		in

		let case_unmount state () =
			match !state with
			| `Mounted (_, unmount, mount)	->
				unmount ();
				state := `Initied mount
			| _					->
				failwith "Switch unmount: unexpected state"
		in

		let case (type a) (tmpl : (a, 'e, 'n) t) : a -> switch =
			let id = !current_id in
			let state = ref (`Tmpl tmpl) in
			current_id := id + 1;
			fun data ->
				(if id <> !current_id then
					!unmount_current ();
					mount_current := case_mount state;
					unmount_current := case_unmount state
				);
				case_update state data
		in

		let update : 'a -> switch = f { case } in
		update data;

		let mount parent =
			let unmount () =
				parent_mounted := None;
				!unmount_current ()
			in
			parent_mounted := Some parent;
			!mount_current parent;
			update, unmount
		in
		mount, (fun () -> !deinit_all ())
