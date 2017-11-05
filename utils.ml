(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   utils.ml                                           :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: juloo </var/mail/juloo>                    +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/11/04 17:38:40 by juloo             #+#    #+#             *)
(*   Updated: 2017/11/04 17:46:31 by juloo            ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

let array_insert t at ins =
	let n = Array.length ins in
	let r = at + n in
	Array.init (Array.length t + n) (fun i ->
		if i < at		then t.(i)
		else if i >= r	then t.(i - n)
						else ins.(i - at)
	)

let array_remove t at n =
	Array.init (Array.length t - n) (fun i ->
		t.(if i < at then i else i + n)
	)

let array_rev t =
	let len = Array.length t in
	Array.init len (fun i -> t.(len - i - 1))

let array_rev_of_list lst = array_rev (Array.of_list lst)
