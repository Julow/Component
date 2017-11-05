(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component_Html.ml                                  :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/21 22:04:10 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/11/05 20:06:44 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

module Component_Dom_html = Component_Dom.Make (struct

	let get_child_opt element i = element##.childNodes##item i
	let get_child element i = Js.Opt.get (get_child_opt element i)
			(fun () -> failwith "child out of bounds")

	type element = Dom.node Js.t
	type element' = Dom_html.element Js.t
	let coerce element = (element :> element)

	let insert parent i node = Dom.insertBefore parent node (get_child_opt parent i)
	let replace parent i node = Dom.replaceChild parent node (get_child parent i)
	let delete parent i = Dom.removeChild parent (get_child parent i)

end)

open ComponentTmpl_T
open Component_Dom_html

include ComponentTmpl
include Component_Dom_html.T

module Internal =
struct

	let cache_last ?(eq=(=)) f init update =
		let last = ref init in
		fun data ->
			let data = f data in
			if not @@ eq !last data then (
				update data;
				last := data
			)

	let e _type =
		let _type = Js.string _type in
		e (fun () -> Dom_html.document##createElement _type)

end

open Internal

let root = root

let text f : ('a, 'e) tmpl =
	fun data event_push ->
		let text = f data in
		let element = Dom_html.document##createTextNode (Js.string text) in
		let mount parent =
			parent (Insert (0, (element :> Dom.node Js.t)));
			let update data = element##.data := Js.string data in
			let unmount () = parent (Delete 0) in
			cache_last f text update, unmount
		in
		let deinit () = () in
		mount, deinit

let div a c = e "div" a c

let attr name f : ('a, 'e) attr =
	let name = Js.string name in
	fun data _ element ->
		let set_attr value = element##setAttribute name (Js.string value) in
		let value = f data in
		set_attr value;
		cache_last f value set_attr

let event _type handler : ('a, 'e) attr =
	fun _ event_push (element : Dom_html.element Js.t) ->
		let handler = Dom_html.handler (fun e ->
			event_push (handler e);
			Js._false
		) in
		let element = (element :> Dom_html.eventTarget Js.t) in
		Dom_html.addEventListener element _type handler Js._false |> ignore;
		fun _ -> ()

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
		let element' = Js.Opt.case element'
			(fun () -> failwith "Not an input") (fun e -> e) in
		let handler e = handler e (Js.to_string element'##.value) in
		event input handler data event_push element
