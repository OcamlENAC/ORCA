(* ocamlmktop -o orca graphics.cma unix.cma geometry.ml object.ml orca.ml display.ml 
	----> CA CEST MIEUX
	-----> ocamlopt -o orca graphics.cmxa unix.cmxa geometry.ml object.ml orca.ml interface.ml display.ml
 *)


open Object
open Geometry
open Graphics
open Interface
open Array


let update objects refreshing_time =
	(** Met a jour la liste de objects en appliquant l'ORCA *)
	(* Array.map (fun o -> Orca.update o refreshing_time) objects *)
	Orca.update objects refreshing_time


let render_obj o refreshing_time = 
	(** Afficher un object *)
	Graphics.set_color Graphics.black;
	Graphics.draw_circle (int_of_float (o.position).x) (int_of_float (o.position).y) (int_of_float (o.diameter /. 2.));
	Graphics.set_color Graphics.blue;
	Graphics.moveto (int_of_float (o.position).x) (int_of_float (o.position).y);
	Graphics.lineto (int_of_float (o.position.x +. (o.speed.x))) (int_of_float (o.position.y +. (o.speed.y)));
	o

let render objects refreshing_time =
	(** Affiche tous les objects et les renvois*)
	Array.map (fun t -> render_obj t refreshing_time) objects 


let test_status status play show restart prec restart_new =
	if status = "pause" then
		play :=  false
	else if status = "play" then
		play := true
	else if status = "show" then
		show := true
	else if status = "hide" then
		show := false
	else if status = "restart" then
		restart := true
	else if status = "prec" then
		prec := true
	else if status = "r_new" then
		restart_new := true


let show_inf show objects posi it_count = 
		if !show then 
			Interface.display_inf posi objects.(0) objects.(1) !it_count

let rec animation objects past_objects refreshing_time play show restart it_count prec restart_new=

begin
	if !restart_new then 
		begin
			Graphics.close_graph ();
			Graphics.open_graph " 800x600";
			restart_new := false;
			it_count := 0;
			let l = Interface.init_sim () in
			let x_o1 = l.(0) in
			let y_o1 = l.(1) in
			let x_o2 = l.(2) in
			let y_o2 = l.(3) in
			let x_d1 = l.(4) in
			let y_d1 = l.(5) in
			let x_d2 = l.(6) in
			let y_d2 = l.(7) in

			let p2 = Object.init x_o2 y_o2 25. 25. 76. x_d2 y_d2 50. 
			and p1 = Object.init x_o1 y_o1 (-. 40.) 0. 76. x_d1 y_d1 50. in
			let objects = [| p1 ; p2 |] in
			past_objects := [|p1 ; p2|];
			animation objects past_objects refreshing_time play show restart it_count prec restart_new;
		end
	else if not !restart then
	begin

		Unix.sleepf (1. /. 25.);

		Graphics.set_color Graphics.white;
		Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;

		let status = Interface.test_clic () in
		test_status status play show restart prec restart_new;
		
		let f_x = Graphics.size_x () and f_y = Graphics.size_y ()  in
		show_inf show objects ({Geometry.x = float_of_int f_x -. 150. ; Geometry.y = float_of_int f_y /. 2.}) it_count;

		if not !play then
			begin
				if not !prec then
					let no = objects in
					let new_objects = render no refreshing_time in
					animation new_objects past_objects refreshing_time play show restart it_count prec restart_new;
				else 
					begin
						prec := false;
						it_count := ! it_count - 1;
						let no = [| !past_objects.(!it_count * 2) ; !past_objects.(!it_count * 2 + 1) |] in
						let new_objects = render no refreshing_time in
						animation new_objects past_objects refreshing_time play show restart it_count prec restart_new;					
					end
				end
		else if !play then
			begin
				let no = update objects refreshing_time in
				let new_objects = render no refreshing_time in
				past_objects := Array.append !past_objects no;
				it_count := !it_count + 1;
				animation new_objects past_objects refreshing_time play show restart it_count prec restart_new;
			end
	end
	else if !restart then
		begin
			restart := false;
			let no = [| !past_objects.(0) ; !past_objects.(1) |] in
			let new_objects = render no refreshing_time in
			past_objects := no;
			it_count := 0;
			animation new_objects past_objects refreshing_time play show restart it_count prec restart_new;
		end 
end

let start_anim restart restart_new = 

	(** Boucle d'animation, ne renvoit rien*)

	let play = ref true in
	let show = ref false in
	let it_count = ref 0 in
	let prec = ref false in

	Graphics.open_graph " 800x600";

	let l = Interface.init_sim () in
	let x_o1 = l.(0) in
	let y_o1 = l.(1) in
	let x_o2 = l.(2) in
	let y_o2 = l.(3) in
	let x_d1 = l.(4) in
	let y_d1 = l.(5) in
	let x_d2 = l.(6) in
	let y_d2 = l.(7) in

	let p2 = Object.init x_o2 y_o2 25. 25. 76. x_d2 y_d2 50. 
	and p1 = Object.init x_o1 y_o1 (-. 40.) 0. 76. x_d1 y_d1 50. 
	and refreshing_time = 1. /. 75. in
	let objects = [| p1 ; p2 |] in
	let past_objects = ref [|p1 ; p2|] in 
	animation objects past_objects refreshing_time play show restart it_count prec restart_new

let () = 
	
	let restart = ref false in
	let restart_new = ref false in
	start_anim restart restart_new
