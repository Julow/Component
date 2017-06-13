(* ************************************************************************** *)
(*                                                                            *)
(*                                                        :::      ::::::::   *)
(*   Component.ml                                       :+:      :+:    :+:   *)
(*                                                    +:+ +:+         +:+     *)
(*   By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+        *)
(*                                                +#+#+#+#+#+   +#+           *)
(*   Created: 2017/03/15 17:35:56 by jaguillo          #+#    #+#             *)
(*   Updated: 2017/06/07 23:29:04 by jaguillo         ###   ########.fr       *)
(*                                                                            *)
(* ************************************************************************** *)

type ('a, 'e) tmpl = 'a -> ('e -> unit) -> Dom_html.element Js.t -> ('a -> unit)
type ('a, 'e) tmpl' = 'a -> ('e -> unit) -> Dom.node Js.t * ('a -> unit)

type root = Dom.node Js.t -> unit

let run root (t, tasks) view update =
	let stream, push = Lwt_stream.create () in
	let push e = push (Some e) in
	let node, redraw = view t push in
	let rec loop t tasks =
		List.iter (fun t -> Lwt.on_success t push) tasks;
		redraw t;
		let%lwt event = Lwt_stream.next stream in
		match update t event with
		| `Loop (t, tasks)	-> loop t tasks
		| `Done r			-> Lwt.return r
	in
	root node;
	loop t tasks

let create_root parent_element root =
	let root = ref (root :> Dom.node Js.t option) in
	fun node ->
		match !root with
		| Some old when node == old	-> ()
		| Some old					->
			Dom.replaceChild parent_element node old;
			root := Some node
		| None						->
			Dom.appendChild parent_element node;
			root := Some node

let to_child_tmpl f =
	fun data event_push parent_element ->
		let element, update = f data event_push in
		Dom.appendChild parent_element element;
		update

let cached_update neq f update_f data =
	fun d' ->
		let d' = f d' in
		if neq d' !data then (
			update_f d';
			data := d'
		)

module T =
struct

	let e' _type childs =
		fun data event_push ->
			let element = Dom_html.document##createElement (Js.string _type) in
			let childs = List.map (fun c -> c data event_push element) childs in
			(element :> Dom.node Js.t), fun data ->
				List.iter (fun c -> c data) childs

	let e _type childs = to_child_tmpl (e' _type childs)

	let text' f =
		fun data _ ->
			let data = f data in
			let element = Dom_html.document##createTextNode (Js.string data) in
			let update data = element##.data := Js.string data in
			(element :> Dom.node Js.t), cached_update (<>) f update (ref data)

	let text f = to_child_tmpl (text' f)

	let comp' view update f event_f =
		fun data event_push ->
			let data = ref (data, f data) in
			let event_push e =
				let data, comp_data = !data in
				event_push (update comp_data e |> event_f data)
			in
			let node, redraw = view (snd !data) event_push in
			node, fun d' ->
				let d' = f d' in
				if d' != (snd !data) then (
					redraw d';
					data := fst !data, d'
				)

	let comp view update f event_f = to_child_tmpl (comp' view update f event_f)

	let t' f tmpl =
		fun data event_push ->
			let data = ref (data, f data) in
			let node, redraw = tmpl (snd !data) event_push in
			node, fun d' ->
				if d' != fst !data then
					let d'' = f d' in
					if d'' != snd !data then (
						data := d', d'';
						redraw d''
					)

	let t f tmpl =
		fun data event_push parent_element ->
			let data = ref (data, f data) in
			let redraw = tmpl (snd !data) event_push parent_element in
			fun d' ->
				if d' != fst !data then
					let d'' = f d' in
					if d'' != snd !data then (
						data := d', d'';
						redraw d''
					)

	let dummy' =
		fun _ _ ->
			let element = Dom_html.document##createComment (Js.string "") in
			(element :> Dom.node Js.t), (fun _ -> ())

	type 'e switch = { s : 'a. ('a, 'e) tmpl' -> 'a -> unit }

	let switch f =
		fun data event_push parent_element ->
			let root = create_root parent_element None in
			let s view =
				let data (* data, redraw, node *) = ref None in
				fun d' ->
					match !data with
					| Some (d, redraw, node)	->
						if d != d' then (
							redraw d';
							data := Some (d', redraw, node)
						);
						root node;
					| None						->
						let node, redraw = view d' event_push in
						redraw d';
						data := Some (d', redraw, node);
						root node
			in
			let f = f { s } in
			f data;
			cached_update (!=) (fun d -> d) f (ref data)

	let lst f view =
		fun data event_push parent_element ->
			let tail_element = Dom_html.document##createComment (Js.string "") in
			Dom.appendChild parent_element tail_element;
			let list_index f lst =
				let rec list_index i = function
					| e :: tail when f e	-> i
					| e :: tail				-> list_index (i + 1) tail
					| []					-> -1
				in
				list_index 0 lst
			in

			let rec update_list a_list b_list reusable =
				match a_list, b_list with
				| a :: a_tail, (b, _ as b') :: b_tail when a == b	->
					b' :: update_list a_tail b_tail reusable
				| a :: a_tail, (b, b_elem) :: b_tail	->
					let index = list_index (fun (b, _) -> a == b) b_tail in
					if index >= 0 then
						remove_n a_list b_list (index + 1) reusable
					else
						let index = list_index ((==) b) a_tail in
						if index < 0 then (
							(snd b_elem) a;
							(a, b_elem) :: update_list a_tail b_tail reusable
						) else
							insert a_tail b_list reusable a
				| a :: a_tail, []						->
					insert a_tail [] reusable a
				| [], (_, b_elem) :: b_tail				->
					Dom.removeChild parent_element (fst b_elem);
					update_list [] b_tail reusable
				| [], []								->
					List.iter (fun e -> Dom.removeChild parent_element (fst e)) reusable;
					[]

			and remove_n a_list b_list n reusable =
				match b_list with
				| (_, b_elem) :: b_tail when n > 0	->
					remove_n a_list b_tail (n - 1) (b_elem :: reusable)
				| _									->
					update_list a_list b_list reusable

			and insert a_list b_list reusable data =
				let e, reusable =
					match reusable with
					| (_, update as e) :: reusable	->
						update data;
						e, reusable
					| []							->
						view data event_push, []
				in
				let before =
					match b_list with
					| (_, (el, _)) :: _	-> (el :> Dom.node Js.t)
					| []				-> (tail_element :> Dom.node Js.t)
				in
				Dom.insertBefore parent_element (fst e) (Js.Opt.return before);
				(data, e) :: update_list a_list b_list reusable
			in

			let data = f data in
			let data = ref (update_list data [] [], data) in
			fun d' ->
				let d' = f d' in
				let childs, data' = !data in
				if d' != data' then
					data := update_list d' childs [], d'

	let seq n f view =
		fun data event_push parent_element ->
			let tail_element = Dom_html.document##createComment (Js.string "") in
			Dom.appendChild parent_element tail_element;
			let tail_element = Js.Opt.return tail_element in
			let childs = ref [] in
			let update data =
				let n = n data in
				let rec loop i = function
					| (_, (elem, _)) :: tail when i >= n		->
						Dom.removeChild parent_element elem;
						loop i tail
					| (d, (_, update as child) as head) :: tail	->
						let d' = f data i in
						if d != d' then (
							update d';
							(d', child) :: loop (i + 1) tail
						)
						else
							head :: loop (i + 1) tail
					| [] when i < n						->
						let d' = f data i in
						let elem, _ as child = view d' event_push in
						Dom.insertBefore parent_element elem tail_element;
						(d', child) :: loop (i + 1) []
					| []								-> []
				in
				childs := loop 0 !childs
			in
			update data;
			cached_update (!=) (fun x -> x) update (ref data)

	let attr name f =
		fun data _ parent_element ->
			let name = Js.string name in
			let update v = parent_element##setAttribute name (Js.string v) in
			let data = f data in
			update data;
			cached_update (<>) f update (ref data)

	let m_attr name f =
		fun data _ parent_element ->
			let name = Js.string name in
			let update data =
				let data = Js.string (f data) in
				let parent_element = Js.Unsafe.inject parent_element in
				if (Js.Unsafe.get parent_element name) <> data then
					Js.Unsafe.set parent_element name data
			in
			update data;
			update

	let _class name f =
		fun data _ parent_element ->
			let name = Js.string name in
			let update v =
				if v then parent_element##.classList##add name
				else parent_element##.classList##remove name
			in
			let data = f data in
			update data;
			cached_update (<>) f update (ref data)

	let style name f =
		fun data _ parent_element ->
			let name = Js.string name in
			let update data =
				let style = Js.Unsafe.inject parent_element##.style in
				Js.Unsafe.set style name (Js.string data)
			in
			let data = f data in
			update data;
			cached_update (<>) f update (ref data)

	let event _type handler =
		fun _ event_push parent_element ->
			let handler = Dom_html.handler (fun e ->
				event_push (handler e);
				Js._false
			) in
			Dom_html.addEventListener parent_element _type handler Js._false
				|> ignore;
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
	let on_input h = event input (fun e ->
			let value =
				let input = Js.Opt.bind e##.target Dom_html.CoerceTo.input in
				Js.Opt.case input (fun () -> assert false)
					(fun input -> Js.to_string input##.value)
			in
			h e value
		)

end
