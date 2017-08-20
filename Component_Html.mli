(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component_Html.mli                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/21 22:06:06 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/08/20 21:42:19 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl
type ('a, 'e) attr

val root : ('a, 'e) tmpl -> #Dom.node Js.t -> ('a, 'e) Component.root

val e : string -> ('a, 'e) attr list -> ('a, 'e) tmpl list -> ('a, 'e) tmpl

val text : ('a -> string) -> ('a, 'e) tmpl

(* val switch *)

val dummy : ('a, 'e) tmpl

val comp : ('b, 'f) tmpl -> ('a -> 'b) -> ('f -> 'e) -> ('a, 'e) tmpl

(* val lst : (('b -> unit) -> 'a -> unit)
	-> ('f -> 'e)
	-> ('b, 'f) tmpl
	-> ('a, 'e) tmpl *)

val seq : (('b -> unit) -> 'a -> unit) -> ('b, 'e) tmpl -> ('a, 'e) tmpl

val attr : string -> ('a -> string) -> ('a, 'e) attr
(* val prop : string -> ('a -> string) -> ('a, 'e) tmpl *)
(* val _class : string -> ('a -> bool) -> ('a, 'e) tmpl *)
(* val style : string -> ('a -> string) -> ('a, 'e) tmpl *)

open Dom_html

val on_click : ('a -> mouseEvent Js.t -> 'e) -> ('a, 'e) attr
val on_mousedown : ('a -> mouseEvent Js.t -> 'e) -> ('a, 'e) attr
val on_mouseup : ('a -> mouseEvent Js.t -> 'e) -> ('a, 'e) attr
val on_mouseover : ('a -> mouseEvent Js.t -> 'e) -> ('a, 'e) attr
val on_mousemove : ('a -> mouseEvent Js.t -> 'e) -> ('a, 'e) attr
val on_mouseout : ('a -> mouseEvent Js.t -> 'e) -> ('a, 'e) attr
val on_keypress : ('a -> keyboardEvent Js.t -> 'e) -> ('a, 'e) attr
val on_keydown : ('a -> keyboardEvent Js.t -> 'e) -> ('a, 'e) attr
val on_keyup : ('a -> keyboardEvent Js.t -> 'e) -> ('a, 'e) attr
val on_input : ('a -> event Js.t -> string -> 'e) -> ('a, 'e) attr
