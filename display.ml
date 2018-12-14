
(* ocamlmktop -o orca graphics.cma unix.cma geometry.ml object.ml orca.ml display.ml 
 *)


(*
let update objects refreshing_time =
	(** Met a jour la liste de objects en appliquant l'ORCA *)
	List.map (fun o -> Orca.update o refreshing_time) objects
*)


let update objects refreshing_time =
	(** Met a jour la liste de objects en appliquant l'ORCA *)
	(* Array.map (fun o -> Orca.update o refreshing_time) objects *)
	Orca.update objects refreshing_time


let render_obj (o : Object.obj) refreshing_time = 
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

let rec start_animation objects refreshing_time =
	(** Boucle d'animation, ne renvoit rien*)
	begin
		Unix.sleepf (refreshing_time);
		Graphics.set_color Graphics.white;
		Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;
		let no = update objects refreshing_time in
		let new_objects = render no refreshing_time in
		start_animation new_objects refreshing_time;
	end


let () = 
	Printf.printf("Debut main\n");
	Graphics.open_graph " 800x600";
	let p2 = Object.init 0. 0. 25. 25. 76. 700. 305. 50. (* Il se dirige un peu plus haut que la pos initiale de objB*)
	and p1 = Object.init 800. 0. (-. 40.) 0. 76. 100. 295. 50. in
	let objects = [|p1 ; p2|]  (* p1 :: p2 :: [] in  init px py sx sy d destx desty ms  
	let sx = 5. and sy = 1. and d = 20. and destx = 0. and desty = 0. and ms = 100. *)
	and refreshing_time = 1. /. 25. in
	start_animation objects refreshing_time

