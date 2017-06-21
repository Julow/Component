(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component_Html.mli                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/21 22:06:06 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/06/21 22:33:20 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl
type ('a, 'e) attr

val root : ('a, 'e) tmpl -> #Dom.node Js.t -> ('a, 'e) Component.tmpl

val e : string -> ('a, 'e) attr list -> ('a, 'e) tmpl list -> ('a, 'e) tmpl

val text : ('a -> string) -> ('a, 'e) tmpl

(* val comp *)
(* val t *)
(* val switch *)

val dummy : ('a, 'e) tmpl

(* val lst : (('b -> unit) -> 'a -> unit)
	-> ('f -> 'e)
	-> ('b, 'f) tmpl
	-> ('a, 'e) tmpl *)

val attr : string -> ('a -> string) -> ('a, 'e) attr
(* val prop : string -> ('a -> string) -> ('a, 'e) tmpl *)
(* val _class : string -> ('a -> bool) -> ('a, 'e) tmpl *)
(* val style : string -> ('a -> string) -> ('a, 'e) tmpl *)
(* val event *)
