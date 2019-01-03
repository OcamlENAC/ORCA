open Object
open Geometry


let is_in_danger_zone v_relat d_cone dt = 
	(****Verifier si la pointe du vrelative est dans le cone****)
	(*On regarde dans l'ordre angle relatif ? -> avant le cercle ? -> après le cercle ? -> dans le cercle ?*)
	(*Angle relatif et autres pour les conditions*) 
	let relat_ang_vrelat_vorient = abs_float (Geometry.relative_angle v_relat d_cone.vect) in
	let relat_ang_v_tangent_v_orient = Geometry.get_angle_cone d_cone in
	(* abs_float (asin ((d_cone.rayon /. 2.)/.(Geometry.norm d_cone.vect))) in(*Voir le tableau, c'est l'angle alpha*)   *)
	let norm_proj_vrobot_vorient = Geometry.norm (Geometry.project v_relat (Geometry.get_unit_vector d_cone.vect)) in
	let vect_origincone_pointevectvitesse = Geometry.add_subst ( -. ) v_relat d_cone.vect  in
	(** Debut des conditions **)
	if relat_ang_vrelat_vorient > relat_ang_v_tangent_v_orient then false 
	else if norm_proj_vrobot_vorient > (Geometry.norm d_cone.vect) then true
	else if Geometry.norm vect_origincone_pointevectvitesse < d_cone.rayon  then true
	else false



let correct objA objB correction =
	let new_v_A = Geometry.add_subst (+.) objA.speed (Geometry.mult_scal 0.5 correction) 
	and new_v_B = Geometry.add_subst (-.) objB.speed (Geometry.mult_scal 0.5 correction) in
	let new_objA = Object.set_obj_speed objA new_v_A
	and new_objB = Object.set_obj_speed objB new_v_B in
	(new_objA, new_objB)

let calc_danger_cone robot obstacle dt =
(**** Calcul des composants du cone ****)
	let vecttry = (Geometry.add_subst ( -. ) obstacle.position robot.position) in
	let vect_orientation = Geometry.mult_scal (1./.dt) vecttry in
	let origin_cone_x = robot.position.x +. (obstacle.position.x -. robot.position.x) /. dt in (* on appelle origine le centre du petit cercle ici*)
	let origin_cone_y = robot.position.y +. (obstacle.position.y -. robot.position.y) /. dt in
	let origin_cone_vect = {Geometry.x = origin_cone_x; Geometry.y = origin_cone_y} in
	(*let danger_rayon = (obstacle.diameter +. robot.diameter) *. Geometry.norm (vect_orientation) /. (2. *. dt) in *)
	let danger_rayon = (obstacle.diameter +. robot.diameter) /. (2. *. dt) in 
	(**** Def du cone de danger ****)
	{ Geometry.origin = origin_cone_vect; Geometry.rayon = danger_rayon; Geometry.vect = vect_orientation;}

let render_cone robot obstacle d_cone in_danger dt =
	(* Affichage vect du cone *)
	Graphics.set_color Graphics.magenta;
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.lineto (int_of_float (d_cone.vect.x *. 100.)) (int_of_float (d_cone.vect.y *. 100.));
	
	(* Affichage des cotes du cone *)
	Graphics.set_color Graphics.green;
	let angle = Geometry.get_angle_cone d_cone in
	let adj_vect = Geometry.get_adj_vect_cone d_cone in
	let adj_vect2 = Geometry.rotate_vect adj_vect ((-. 2.) *. angle) in
	let longueur = 500. in
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.lineto (int_of_float (robot.position.x +. adj_vect.x *. longueur)) (int_of_float (robot.position.y +. adj_vect.y *. longueur));
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.lineto (int_of_float (robot.position.x +. adj_vect2.x *. longueur)) (int_of_float (robot.position.y +. adj_vect2.y *. longueur));
	
	(* Affichage origine du cone *)
	Graphics.draw_circle (int_of_float (d_cone.origin.x)) (int_of_float (d_cone.origin.y)) (int_of_float d_cone.rayon);
	Graphics.draw_circle (int_of_float (d_cone.origin.x *. dt)) (int_of_float (d_cone.origin.y *. dt)) (int_of_float (d_cone.rayon *. dt));
	
	(* Affichage de in_danger*)
	Graphics.moveto 50 400;
	Graphics.set_color Graphics.black;
	Graphics.set_text_size 50;
	if in_danger then Graphics.draw_string "DANS LE cone !" else Graphics.draw_string "np";
	
	(* Affichage v_relat *)
	let v_relat_x = robot.speed.x -. obstacle.speed.x in
	let v_relat_y = robot.speed.y -. obstacle.speed.y in
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.set_color Graphics.red;
	Graphics.lineto (int_of_float (robot.position.x +. v_relat_x)) (int_of_float (robot.position.y +. v_relat_y));

	(* Affichage des destinations *)
	Graphics.set_color Graphics.cyan;
	Graphics.fill_circle (int_of_float robot.dest.x) (int_of_float robot.dest.y) 10;
	Graphics.fill_circle (int_of_float obstacle.dest.x) (int_of_float obstacle.dest.y) 10;

	Printf.printf "cone_rayon = %f ,    norme de cone_vect = %f   \n" d_cone.rayon (Geometry.norm d_cone.vect)



let get_correction vr d_cone = 
	let r_angle = Geometry.relative_angle d_cone.vect vr in
	let alpha = Geometry.get_angle_cone d_cone in
	if r_angle < 0. then 
		let proj_vr_cone = Geometry.rotate_vect vr ((-. alpha) -. r_angle) in
		Geometry.add_subst (-.) proj_vr_cone vr
	else 
		let proj_vr_cone = Geometry.rotate_vect vr (alpha -. r_angle) in
	
let calc_half_plane robot correction =
	let vect = Geometry.add_subst ( +. ) robot.speed correction in
	let orig = Geometry.add_subst ( +. ) robot.position vect in
	{Geometry.origin = orig ; Geometry.vect_normal = correction ;}

let update objects refreshing_time =
	(** Retourne le nouveau point avec les vitesses résultantes de l'algo ORCA*)
	let dt = 3. in
	let objA = objects.(0) and objB = objects.(1) in

	let v_relat = Geometry.add_subst (-.) objA.speed objB.speed in

	let d_cone = calc_danger_cone objA objB dt in

	let v_A_opti = Object.calc_opt_speed objA
	and v_B_opti = Object.calc_opt_speed objB in

	let in_danger = is_in_danger_zone v_relat d_cone dt in

	render_cone objA objB d_cone in_danger dt;

	if in_danger then
		let u = get_correction v_relat d_cone in
		let s_plan_A = calc_half_plane objA (Geometry.mult_scal 0.5 u) 
		and s_plan_B = calc_half_plane objB (Geometry.mult_scal (-. 0.5) u) in
		(***  PROJETER LES V_OPTI SUR LE DEMI PLAN   ***)
		let pointe_vAopti = (Geometry.add_subst (+.) objA.position v_A_opti) 
		and pointe_vBopti = (Geometry.add_subst (+.) objB.position v_B_opti) in
		let point_projected_semiplan_A = Geometry.project_on_plane pointe_vAopti s_plan_A
		and point_projected_semiplan_B = Geometry.project_on_plane pointe_vBopti s_plan_B in
		let new_v_A = Geometry.add_subst (-.) point_projected_semiplan_A objA.position 
		and new_v_B = Geometry.add_subst (-.) point_projected_semiplan_B objB.position in
		let new_objA = Object.set_obj_speed objA new_v_A
		and new_objB = Object.set_obj_speed objB new_v_B in
		[| Object.update_pos new_objA refreshing_time ; Object.update_pos new_objB refreshing_time |]

	else
		let u = get_correction v_relat d_cone in
		let s_plan_A = calc_half_plane objA (Geometry.mult_scal 0.5 u) 
		and s_plan_B = calc_half_plane objB (Geometry.mult_scal (-. 0.5) u) in

		(***  PROJETER LES V_OPTI SUR LE DEMI PLAN   ***)
		let pointe_vAopti = (Geometry.add_subst (+.) objA.position v_A_opti) 
		and pointe_vBopti = (Geometry.add_subst (+.) objB.position v_B_opti) in

		let in_sp = (Geometry.is_in_semi_plan objA.position v_A_opti s_plan_A, Geometry.is_in_semi_plan objB.position v_B_opti s_plan_B) in

		match in_sp with
		 | (false, false) -> 
		 	let new_objA = Object.set_obj_speed objA v_A_opti
			and new_objB = Object.set_obj_speed objB v_B_opti in
			[| Object.update_pos new_objA refreshing_time ; Object.update_pos new_objB refreshing_time |]
		 | (true, false) -> 
		 	let point_projected_semiplan_A = Geometry.project_on_plane pointe_vAopti s_plan_A in
		 	let new_v_A = Geometry.add_subst (-.) point_projected_semiplan_A objA.position  in
		 	let new_objA = Object.set_obj_speed objA new_v_A
			and new_objB = Object.set_obj_speed objB v_B_opti in
			[| Object.update_pos new_objA refreshing_time ; Object.update_pos new_objB refreshing_time |] 
		| (false, true) -> 
		 	let point_projected_semiplan_B = Geometry.project_on_plane pointe_vBopti s_plan_B in
		 	let new_v_B = Geometry.add_subst (-.) point_projected_semiplan_B objB.position  in
		 	let new_objA = Object.set_obj_speed objA v_A_opti
			and new_objB = Object.set_obj_speed objB new_v_B in
			[| Object.update_pos new_objA refreshing_time ; Object.update_pos new_objB refreshing_time |] 
		| (true, true) ->
			let point_projected_semiplan_A = Geometry.project_on_plane pointe_vAopti s_plan_A in
		 	let new_v_A = Geometry.add_subst (-.) point_projected_semiplan_A objA.position  in
		 	let new_objA = Object.set_obj_speed objA new_v_A in

			let point_projected_semiplan_B = Geometry.project_on_plane pointe_vBopti s_plan_B in
		 	let new_v_B = Geometry.add_subst (-.) point_projected_semiplan_B objB.position  in
			let new_objB = Object.set_obj_speed objB new_v_B in

	
