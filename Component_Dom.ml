(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component_Dom.ml                                   :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/10/08 20:43:31 by juloo             #+#    #+#             *)
(*   Updated: 2017/11/05 20:00:08 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Make (Dom : sig

	(* The concrete node type *)
	type element

	(* The oups part *)
	type element'
	val coerce : element' -> element

	(* `insert parent index node` *)
	(* Inserts `node` into `parent` at `index` *)
	val insert : element' -> int -> element -> unit

	(* `replace parent index node` *)
	(* Replaces the `index` child of `parent` with `node` *)
	val replace : element' -> int -> element -> unit

	(* `delete node index` *)
	(* Removes the `index` child of `node` *)
	val delete : element' -> int -> unit

end) =
struct

	open ComponentTmpl_T

	module T =
	struct
		type ('a, 'e) tmpl = ('a, 'e, Dom.element) t
		type ('a, 'e) attr = 'a -> ('e -> unit) -> Dom.element' -> 'a -> unit
	end

	open T

	let root_element parent = function
		| Insert (i, node)	-> Dom.insert parent i node
		| Replace (i, node)	-> Dom.replace parent i node
		| Delete i			-> Dom.delete parent i

	let root parent tmpl = root (root_element parent) tmpl

	let e create_element (attrs : ('a, 'e) attr list) childs : ('a, 'e) tmpl =
		let childs = ComponentTmpl.group childs in
		let attrs = Array.of_list attrs in
		fun data event_push ->
			let element = create_element () in
			let attrs = Array.map (fun a -> a data event_push element) attrs in
			let mount_childs, deinit_childs = childs data event_push in

			let mount parent =
				let update_childs, _ = mount_childs (root_element element) in

				let update data =
					update_childs data;
					Array.iter (fun attr -> attr data) attrs
				in

				parent (Insert (0, Dom.coerce element));
				update, (fun () -> parent (Delete 0))
			in
			mount, deinit_childs

end
