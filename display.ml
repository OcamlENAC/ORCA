let update objects dt =
	(** Met a jour la liste de objects en appliquant l'ORCA *)
	List.map (fun o -> Orca.update o dt) objects

let render_obj (o : Object.obj) = 
	(** Afficher un object *)
	Graphics.set_color Graphics.black;
	Graphics.fill_circle (int_of_float (o.position).x) (int_of_float (o.position).y) (int_of_float o.diameter);
	o

let render objects =
	(** Affiche tous les objects et les renvois*)
	List.map render_obj objects




let rec start_animation objects =
	(** Boucle d'animation, ne renvoit rien*)
	let dt = 1. /. 25. in
	let no = update objects dt in
	let new_objects = render no in
	begin
		Unix.sleepf (dt);
		Graphics.set_color Graphics.white;
		Graphics.fill_rect 0 0 800 600 ;
		start_animation new_objects;
	end


let () = 
	Printf.printf("Debut main\n");
	Graphics.open_graph " 800x600";
	let p2 = Object.init 150. 200. 1. 1. 5. 0. 0.
	and p1 = Object.init 500. 10. 1. 25. 5. 0. 0. in
	let objects = p1 :: p2 :: [] in
	start_animation objects
