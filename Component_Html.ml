(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component_Html.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/21 22:04:10 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/07/05 23:34:03 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type action =
	| Insert of int * Dom.node Js.t
	| Replace of int * Dom.node Js.t
	| Delete of int

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> (
		(action -> unit) -> ('a -> unit) * (unit -> unit)
	) * (unit -> unit)

type ('a, 'e) attr = 'a -> ('e -> unit) -> Dom_html.element Js.t -> 'a -> unit

let cache_last ?(eq=(=)) f init update =
	let last = ref init in
	fun data ->
		let data = f data in
		if not @@ eq !last data then (
			update data;
			last := data
		)

let dead _ = failwith "dead"

let html_root element offset =
	let get_child_opt i = element##.childNodes##item (i + offset) in
	let get_child i = Js.Opt.get (get_child_opt i) dead in
	function
	| Insert (i, node)	-> Dom.insertBefore element node (get_child_opt i); ~+1
	| Replace (i, node)	-> Dom.replaceChild element node (get_child i); ~+0
	| Delete i			-> Dom.removeChild element (get_child i); ~-1

let root (tmpl : ('a, 'e) tmpl) root_element : ('a, 'e) Component.tmpl =
	let root' action = ignore @@ html_root root_element 0 action in
	fun data event_push ->
		let (mount_tmpl, deinit_tmpl) = tmpl data event_push in
		let mount () =
			let update_tmpl, unmount_tmpl = mount_tmpl root' in
			update_tmpl, unmount_tmpl
		in
		mount, deinit_tmpl

let e _type attrs childs =
	let attrs = Array.of_list attrs in
	let childs = Array.of_list childs in
	fun data event_push ->
		let element = Dom_html.document##createElement (Js.string _type) in
		let attrs = Array.map (fun a -> a data event_push element) attrs in
		let childs = Array.map (fun c -> c data event_push) childs in
		let mount root =
			let child_lengths = Array.map (fun _ -> 0) childs in
			let offset = ref 0 in

			let root' i action =
				let offset' = !offset in
				let inc = html_root element offset' action in
				let length = child_lengths.(i) + inc in
				offset := offset' + length;
				child_lengths.(i) <- length
			in

			let mount_child i (mount, _) = fst @@ mount (root' i) in
			let childs = Array.mapi mount_child childs in
			root (Insert (0, (element :> Dom.node Js.t)));

			let update data =
				offset := 0;
				Array.iter (fun child -> child data) childs;
				Array.iter (fun attr -> attr data) attrs

			in
			let unmount () = () in
			update, unmount
		in
		let deinit () = Array.iter (fun (_, deinit) -> deinit ()) childs in
		mount, deinit

let text f =
	fun data event_push ->
		let text = f data in
		let element = Dom_html.document##createTextNode (Js.string text) in
		let mount root =
			root (Insert (0, (element :> Dom.node Js.t)));
			let update data = element##.data := Js.string data in
			let unmount () = root (Delete 0) in
			cache_last f text update, unmount
		in
		let deinit () = () in
		mount, deinit

let dummy = fun _ _ -> (fun _ -> (fun _ -> ()), (fun () -> ())), (fun () -> ())

let attr name f =
	let name = Js.string name in
	fun data _ element ->
		let set_attr value = element##setAttribute name (Js.string value) in
		let value = f data in
		set_attr value;
		cache_last f value set_attr

(* let lst f event_f tmpl =
	fun data event_push ->
		fun root ->
			fun root data -> *)

let event _type handler =
	fun data event_push (element : Dom_html.element Js.t) ->
		let data = ref data in
		let handler = Dom_html.handler (fun e ->
			event_push (handler !data e);
			Js._false
		) in
		let element = (element :> Dom_html.eventTarget Js.t) in
		Dom_html.addEventListener element _type handler Js._false |> ignore;
		fun d' -> data := d'

open Dom_html.Event

let on_click h = event click h
let on_mousedown h = event mousedown h
let on_mouseup h = event mouseup h
let on_mouseover h = event mouseover h
let on_mousemove h = event mousemove h
let on_mouseout h = event mouseout h
let on_keypress h = event keypress h
let on_keydown h = event keydown h
let on_keyup h = event keyup h

let on_input handler =
	fun data event_push element ->
		let element' = Dom_html.CoerceTo.input element in
		let element' = Js.Opt.case element' dead (fun e -> e) in
		let handler data e = handler data e (Js.to_string element'##.value) in
		event input handler data event_push element
