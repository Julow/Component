(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.mli                                      :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/06/07 22:57:42 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/07/05 22:11:35 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> (
		unit -> ('a -> unit) * (unit -> unit)
	) * (unit -> unit)

val run : 'a * 'e Lwt.t list
	-> ('a, 'e) tmpl
	-> ('a -> 'e -> [< `Loop of 'a * 'e Lwt.t list | `Done of 'r ])
	-> 'r Lwt.t
