(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   ComponentTmpl_T.ml                                 :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/08/20 21:30:54 by juloo             #+#    #+#             *)
(*   Updated: 2017/08/20 21:30:57 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type 'n action =
	| Insert of int * 'n
	| Replace of int * 'n
	| Delete of int

type ('a, 'e, 'n) t = 'a -> ('e -> unit) -> (
		('n action -> unit) -> ('a -> unit) * (unit -> unit)
	) * (unit -> unit)
