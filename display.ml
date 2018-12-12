
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


let render_obj (o : Object.obj) = 
	(** Afficher un object *)
	Graphics.set_color Graphics.black;
	Graphics.fill_circle (int_of_float (o.position).x) (int_of_float (o.position).y) (int_of_float o.diameter);
	o

let render objects =
	(** Affiche tous les objects et les renvois*)
	Array.map render_obj objects




let rec start_animation objects refreshing_time =
	(** Boucle d'animation, ne renvoit rien*)
	let no = update objects refreshing_time in
	let new_objects = render no in
	begin
		Unix.sleepf (refreshing_time);
		Graphics.set_color Graphics.white;
		Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ()) ;
		start_animation new_objects refreshing_time;
	end


let () = 
	Printf.printf("Debut main\n");
	Graphics.open_graph " 800x600";
	let p2 = Object.init_from_pos 150. 200.
	and p1 = Object.init_from_pos 500. 10. in
	let objects = [|p1 ; p2|]  (* p1 :: p2 :: [] in *)
	and refreshing_time = 1. /. 25. in
	start_animation objects refreshing_time



