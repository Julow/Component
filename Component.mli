(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.mli                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/07 22:57:42 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/06/21 22:18:51 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> 'a -> unit

val run : 'a * 'e Lwt.t list
	-> ('a, 'e) tmpl
	-> ('a -> 'e -> [< `Loop of 'a * 'e Lwt.t list | `Done of 'r ])
	-> 'r Lwt.t
