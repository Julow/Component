(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.mli                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/07 22:57:42 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/06/07 23:28:21 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl
type ('a, 'e) tmpl'

type root

val run : root -> 'a * 'e Lwt.t list
	-> ('a, 'e) tmpl'
	-> ('a -> 'e -> [< `Loop of 'a * 'e Lwt.t list | `Done of 'r ])
	-> 'r Lwt.t

val create_root : #Dom.node Js.t -> #Dom.node Js.t option -> root

module T :
sig

	val e' : string -> ('a, 'e) tmpl list -> ('a, 'e) tmpl'
	val e : string -> ('a, 'e) tmpl list -> ('a, 'e) tmpl

	val text' : ('a -> string) -> ('a, 'e) tmpl'
	val text : ('a -> string) -> ('a, 'e) tmpl

	val comp' : ('a, 'e) tmpl'
		-> ('a -> 'e -> 'r)
		-> ('d -> 'a)
		-> ('d -> 'r -> 'dr)
		-> ('d, 'dr) tmpl'

	val comp : ('a, 'e) tmpl'
		-> ('a -> 'e -> 'r)
		-> ('d -> 'a)
		-> ('d -> 'r -> 'dr)
		-> ('d, 'dr) tmpl

	val t' : ('a -> 'd) -> ('d, 'e) tmpl' -> ('a, 'e) tmpl'
	val t : ('a -> 'd) -> ('d, 'e) tmpl -> ('a, 'e) tmpl

	val dummy' : ('a, 'e) tmpl'

	type 'e switch = { s : 'a. ('a, 'e) tmpl' -> 'a -> unit }

	val switch : ('e switch -> 'a -> unit) -> ('a, 'e) tmpl

	val lst : ('a -> 'd list) -> ('d, 'e) tmpl' -> ('a, 'e) tmpl
	val seq : ('a -> int) -> ('a -> int -> 'd) -> ('d, 'e) tmpl' -> ('a, 'e) tmpl

	val attr : string -> ('a -> string) -> ('a, 'e) tmpl
	val m_attr : string -> ('a -> string) -> ('a, 'e) tmpl
	val _class : string -> ('a -> bool) -> ('a, 'e) tmpl
	val style : string -> ('a -> string) -> ('a, 'e) tmpl

	val event : (#Dom_html.event Js.t as 'v) Dom_html.Event.typ
		-> ('v -> 'e) -> ('a, 'e) tmpl

	val on_click : (Dom_html.mouseEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_mousedown : (Dom_html.mouseEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_mouseup : (Dom_html.mouseEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_mouseover : (Dom_html.mouseEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_mousemove : (Dom_html.mouseEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_mouseout : (Dom_html.mouseEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_keypress : (Dom_html.keyboardEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_keydown : (Dom_html.keyboardEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_keyup : (Dom_html.keyboardEvent Js.t -> 'e) -> ('a, 'e) tmpl
	val on_input : (Dom_html.event Js.t -> string -> 'e) -> ('a, 'e) tmpl

end
