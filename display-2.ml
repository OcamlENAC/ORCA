(* ocamlmktop -o orca graphics.cma unix.cma geometry.ml object.ml orca.ml display.ml 
	----> CA CEST MIEUX
	-----> ocamlopt -o orca graphics.cmxa unix.cmxa geometry.ml object.ml orca.ml interface.ml display.ml
 *)


open Object
open Geometry
open Graphics
open Interface


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


let test_status status pause play show restart =
	if status = "pause" then
		begin
			pause := true;
			play :=  false;
		end
	else if status = "play" then
		begin
			pause := false; 
			play := true;
		end
	else if status = "show" then
		show := true
	else if status = "hide" then
		show := false
	else if status = "restart" then
		restart := true


let show_inf show objects posi = 
		if !show then 
		begin
			posi := Interface.mv_inf !posi;
			Interface.display_inf !posi objects.(0) objects.(1);
		end

let rec animation objects past_objects refreshing_time pause play show posi restart =

	begin
	if not !restart then
	begin

		Unix.sleepf (1. /. 25.);

		Graphics.set_color Graphics.white;
		Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;

		let status = Interface.test_clic () in
		test_status status pause play show restart;
	
		show_inf show objects posi;

		if !pause then
			let no = objects in
			let new_objects = render no refreshing_time in
			animation new_objects past_objects refreshing_time pause play show posi restart;

		else
			begin
				let no = update objects refreshing_time in
				let new_objects = render no refreshing_time in
				animation new_objects past_objects refreshing_time pause play show posi restart;
			end
	end
	else
		begin
			restart := false;
			let no = past_objects in
			let new_objects = render no refreshing_time in
			animation new_objects past_objects refreshing_time pause play show posi restart;
		end 
	end


let start_anim restart = 

	(** Boucle d'animation, ne renvoit rien*)

	let pause = ref false in
	let play = ref true in
	let show = ref false in
	let posi = ref {Geometry.x = 600.; Geometry.y = 385.} in

	Printf.printf("Debut main\n");
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

	let p2 = Object.init x_o2 y_o2 25. 25. 76. x_d2 y_d2 50. (* Il se dirige un peu plus haut que la pos initiale de objB*)
	and p1 = Object.init x_o1 y_o1 (-. 40.) 0. 76. x_d1 y_d1 50. in
	let objects = [|p1 ; p2|]  (* p1 :: p2 :: [] in  init px py sx sy d destx desty ms  
	let sx = 5. and sy = 1. and d = 20. and destx = 0. and desty = 0. and ms = 100. *)
	and refreshing_time = 1. /. 50. in
	animation objects objects refreshing_time pause play show posi restart

let () = 
	
	let restart = ref false in
	start_anim restart
