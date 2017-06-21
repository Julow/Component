(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component_Html.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/21 22:04:10 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/06/21 22:34:55 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type action = Insert of Dom.node Js.t | Replace of Dom.node Js.t | Delete

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> (action -> unit)
		-> (action -> unit) -> 'a -> unit

type ('a, 'e) attr = 'a -> ('e -> unit) -> Dom_html.element Js.t -> 'a -> unit

let root_element element childs =
	let get_child_opt index = element##.childNodes##item index in
	let get_child index =
		Js.Opt.get (get_child_opt index) (fun () -> assert false)
	in
	let index = ref 0 in
	let child_lengths = Array.map (fun _ -> 0) childs in
	let child_update action =
		let i = !index in
		index := i + 1;
		let inc =
			match action with
			| Insert node	->
				Dom.insertBefore element node (get_child_opt i); ~+1
			| Replace node	->
				Dom.replaceChild element node (get_child i); 0
			| Delete		->
				Dom.removeChild element (get_child i); ~-1
		in
		child_lengths.(i) <- child_lengths.(i) + inc
	in
	let childs = Array.map (fun child -> child child_update) childs in
	let update data =
		index := 0;
		Array.iteri (fun i child ->
				let start = !index in
				child child_update data;
				index := start + child_lengths.(i)
			) childs
	in
	update

let root tmpl root =
	fun data event_push ->
		let update = root_element root [| tmpl data event_push |] in
		update

let e _type attrs childs =
	let attrs = Array.of_list attrs in
	let childs = Array.of_list childs in
	fun data event_push root ->
		let element = Dom_html.document##createElement (Js.string _type) in
		let attrs = attrs |> Array.map (fun attr ->
				attr data event_push element) in
		let childs = childs |> Array.map (fun child ->
				child data event_push) in
		let update = root_element element childs in
		root (Insert (element :> Dom.node Js.t));
		fun _ data ->
			Array.iter (fun attr -> attr data) attrs;
			update data

let text f =
	fun data _ root ->
		let text = ref (f data) in
		let element = Dom_html.document##createTextNode (Js.string !text) in
		root (Insert (element :> Dom.node Js.t));
		fun _ data ->
			let text' = f data in
			if text' <> !text then (
				element##.data := Js.string text';
				text := text'
			)

let dummy = fun _ _ _ -> fun _ _ -> ()

let attr name f =
	let name = Js.string name in
	fun data _ element ->
		let value = ref (f data) in
		let set_attr value = element##setAttribute name (Js.string value) in
		let update data =
			let v' = f data in
			if !value <> v' then (
				set_attr v';
				value := v'
			)
		in
		set_attr !value;
		update
