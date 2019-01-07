open Graphics
open Geometry
open Object

type button = {
	x : int;
	y : int;
	text : string;
	size : int;
}

let init_button x y text size =
	{x = x; y = y; text = text; size = size}

let draw_button button =
	Graphics.set_color Graphics.black;
	Graphics.moveto button.x button.y;
	Graphics.lineto button.x (button.y + button.size / 2);
	Graphics.lineto (button.x + button.size) (button.y + button.size / 2);
	Graphics.lineto (button.x + button.size) button.y;
	Graphics.lineto button.x button.y;
	Graphics.moveto (button.x + button.size / 2) (button.y + button.size / 4);
	Graphics.draw_string button.text

let is_in_button m_x m_y button =
	if m_x > button.x && m_x < button.x + button.size && m_y > button.y && m_y < button.y + button.size / 2
	then true else false

let display_inf (orig : Geometry.vector) obj1 obj2 =
	let ori_x = int_of_float orig.x in
	let ori_y = int_of_float orig.y in
	Graphics.set_color Graphics.black;
	Graphics.moveto ori_x ori_y;
	Graphics.draw_string "Objet 1 :";
	Graphics.moveto ori_x (ori_y - 15);
	let x1 = int_of_float obj1.position.x in 
	let y1 = int_of_float obj1.position.y in
	Graphics.draw_string ("Posi x : " ^ string_of_int x1 ^ " y : " ^ string_of_int y1);
	Graphics.moveto ori_x (ori_y - 30);
	let vx1 = int_of_float obj1.speed.x in
	let vy1 = int_of_float obj1.speed.y in
	Graphics.draw_string ("Speed x : " ^ string_of_int vx1 ^ " y : " ^ string_of_int vy1);
	Graphics.moveto ori_x (ori_y - 50);
	Graphics.draw_string "Objet 2 :";
	Graphics.moveto ori_x (ori_y - 65);
	let x2 = int_of_float obj2.position.x in 
	let y2 = int_of_float obj2.position.y in
	Graphics.draw_string ("Posi x : " ^ string_of_int x2 ^ " y : " ^ string_of_int y2);
	Graphics.moveto ori_x (ori_y - 80);
	let vx2 = int_of_float obj2.speed.x in
	let vy2 = int_of_float obj2.speed.y in
	Graphics.draw_string ("Speed x : " ^ string_of_int vx2 ^ " y : " ^ string_of_int vy2)

let make_interface () = 
	let b_pause = init_button 400 20 "Pause" 60 in
	draw_button b_pause;
	let b_play = init_button 400 60 "Play" 60 in
	draw_button b_play;
	let b_show_inf = init_button 460 60 "Show" 60 in
	draw_button b_show_inf;
	let b_hide_inf = init_button 460 20 "Hide" 60 in
	draw_button b_hide_inf;
	let b_restart = init_button 520 20 "Restart" 60 in
	draw_button b_restart;
	[| b_pause ; b_play ; b_show_inf ; b_hide_inf ; b_restart|]

let get_clic () =
	let status = Graphics.wait_next_event [Button_down] in
	let o_x = status.mouse_x in
	let o_y = status.mouse_y in
	[| o_x ; o_y |]

let get_clic_2 () =
	Printf.printf "Appel à clic 2 \n";
	let stat = Graphics.wait_next_event [Button_up] in
	let o_x = stat.mouse_x in
	let o_y = stat.mouse_y in
	({Geometry.x = float_of_int o_x ; Geometry.y = float_of_int o_y} : Geometry.vector )

let mv_inf (posi : Geometry.vector) =
	let status = Graphics.wait_next_event [Poll] in
	let s = Graphics.button_down () in
	let fake_button = {x = int_of_float posi.x; y = int_of_float posi.y; text = ""; size = 10} in
	if s && is_in_button status.mouse_x status.mouse_y fake_button then
	get_clic_2 ()
	else posi 

let init_sim () =
	(* Partie de selection du départ et de l'arrivée *)
	(* départ obj 1 *)
	Graphics.set_color Graphics.black;
	Graphics.moveto 400 300;
	Graphics.draw_string "Clic depart obj 1 :";
	let l_o1 = get_clic () in 
	let x_o1 = float_of_int l_o1.(0) in
	let y_o1 = float_of_int l_o1.(1) in
	Graphics.clear_graph ();
	Graphics.set_color Graphics.red;
	Graphics.fill_circle (int_of_float x_o1) (int_of_float y_o1) 10;

	(* départ obj 2 *)
	Graphics.set_color Graphics.black;
	Graphics.moveto 400 300;
	Graphics.draw_string "Clic depart obj 2 :";
	let l_o2 = get_clic () in 
	let x_o2 = float_of_int l_o2.(0) in
	let y_o2 = float_of_int l_o2.(1) in
	Graphics.clear_graph ();
	Graphics.set_color Graphics.red;
	Graphics.fill_circle (int_of_float x_o1) (int_of_float y_o1) 10;
	Graphics.fill_circle (int_of_float x_o2) (int_of_float y_o2) 10;

	(* dest obj 1 *)
	Graphics.set_color Graphics.black;
	Graphics.moveto 400 300;
	Graphics.draw_string "Clic destination obj 1 :";
	let l_d1 = get_clic () in 
	let x_d1 = float_of_int l_d1.(0) in
	let y_d1 = float_of_int l_d1.(1) in
	Graphics.clear_graph ();
	Graphics.set_color Graphics.red;
	Graphics.fill_circle (int_of_float x_o1) (int_of_float y_o1) 10;
	Graphics.fill_circle (int_of_float x_o2) (int_of_float y_o2) 10;
	Graphics.set_color Graphics.blue;
	Graphics.fill_circle (int_of_float x_d1) (int_of_float y_d1) 10;

	(* dest obj 2 *)
	Graphics.set_color Graphics.black;
	Graphics.moveto 400 300;
	Graphics.draw_string "Clic destination obj 2 :";
	let l_d2 = get_clic () in 
	let x_d2 = float_of_int l_d2.(0) in
	let y_d2 = float_of_int l_d2.(1) in
	Graphics.clear_graph ();
	Graphics.set_color Graphics.red;
	Graphics.fill_circle (int_of_float x_o1) (int_of_float y_o1) 10;
	Graphics.fill_circle (int_of_float x_o2) (int_of_float y_o2) 10;
	Graphics.set_color Graphics.blue;
	Graphics.fill_circle (int_of_float x_d1) (int_of_float y_d1) 10;
	Graphics.fill_circle (int_of_float x_d2) (int_of_float y_d2) 10;

	[| x_o1 ; y_o1 ; x_o2 ; y_o2 ; x_d1 ; y_d1 ; x_d2 ; y_d2 |]

let test_clic () = 
	let list_but = make_interface () in
	let b_pause = list_but.(0) in
	let b_play = list_but.(1) in
	let b_show_inf = list_but.(2) in
	let b_hide_inf = list_but.(3) in
	let b_restart = list_but.(4) in 

	let status = Graphics.wait_next_event [Poll] in
	let s = Graphics.button_down () in
	if s && is_in_button status.mouse_x status.mouse_y b_pause then "pause"
	else if s && is_in_button status.mouse_x status.mouse_y b_play then "play"
	else if s && is_in_button status.mouse_x status.mouse_y b_show_inf then "show"
	else if s && is_in_button status.mouse_x status.mouse_y b_hide_inf then "hide"
	else if s && is_in_button status.mouse_x status.mouse_y b_restart then "restart"
	else "none"

