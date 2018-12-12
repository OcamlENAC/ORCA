

let calc_cone objA objB =
	0

let is_in_danger vr cone =
	0

let get_correction vr cone =
	0

let new_speed objA objB in_danger =
	(* S'il n'y a pas de risque de collision alors on calcul leur V_opti
		et on renvoit  A et B avec des vitesses modifiées *)
	if not in_danger then
		let new_v_A = Object.calc_opt_speed objA
		and new_v_B = Object.calc_opt_speed objB in
		let new_objA = Object.set_obj_speed objA new_v_A
		and new_objB = Object.set_obj_speed objB new_v_B in
		(new_objA, new_objB)
	else (objA, objB)


let correct objA objB v_correction =
	0

let update objects refreshing_time =
	(** Retourne le nouveau point avec les vitesses résultantes de l'algo ORCA*)
	(* Object.set_obj_pos obj {Geometry.x = ((obj.position).x +. 2.) ; Geometry.y = ((obj.position).x +. 2.)}  *)
	(*
	calc_cone puis is_in_danger puis new_speed

	doit renvoyer Array avec objA et objB avec vitesses et positions modifiées
	*)
	let objA = objects.(0) and objB = objects.(1) in
	let (new_objA, new_objB) = (new_speed objA objB false) in
	
	[| Object.update_pos new_objA refreshing_time ; Object.update_pos new_objB refreshing_time |]


(*

let orca_deux_objet obj set_of_objects =
	(* Chaque objet a Vitesse Position Rayon de visible et en secret un V optimale *)
	
	(* Determiner le cone de risque *)

	(* is_in_cone *)

	(*if is_in_cone vitesse_optimale cone then project vitesse cote_du_cone_le_plus_proche
	else vitesse_optimale *)

	(* Determiner la vitesse corrigée (la plus proche de vopti) *)

let create_cone dt robot obstacle = 
	let vect_orientation = add_subst ( - ) obstacle.position robot.position in 
	let origin_cone_x = robot.position.x +. (obstacle.position.x -. robot.position.x)/. dt in (* on appelle origine le centre du petit cercle ici*)
	let origin_cone_y = robot.position.y +. (obstacle.position.y -. robot.position.y)/. dt in
	let origin_cone_vect = {x = origin_cone_x; y = origin_cone_y} in
	let danger_diameter = (obstacle.diameter + robot.diameter) *. norm (vect_orientation) /. dt in
	(**** Def du cone de danger ****)
	{ origin = origin_cone_vect; diameter = danger_diameter; orientation = vect_orientation}


let is_in_danger_zone dt robot obstacle = 
	let danger_cone = create_cone dt robot obstacle in 
	(**** Calcul des composants du cone ****)
	let vect_orientation = add_subst ( - ) obstacle.position robot.position in 
	let origin_cone_x = robot.position.x +. (obstacle.position.x -. robot.position.x)/. dt in (* on appelle origine le centre du petit cercle ici*)
	let origin_cone_y = robot.position.y +. (obstacle.position.y -. robot.position.y)/. dt in
	let origin_cone_vect = {x = origin_cone_x; y = origin_cone_y} in
	let danger_diameter = (obstacle.diameter + robot.diameter) *. norm (vect_orientation) /. dt in
	(**** Def du cone de danger ****)
	let danger_cone = { origin = origin_cone_vect; diameter = danger_diameter; orientation = vect_orientation} in
	(****Verifier si la pointe du v Vopti est dans le cone****)
	(*On regarde dans l'ordre angle relatif ? -> avant le cercle ? -> après le cercle ? -> dans le cercle ?*)
	(*Angle relatif et autres pour les conditions*) 
	let relat_ang_vrobot_vorient = relative_angle abs_float obstacle.vitesse abs_float vect_orientation in
	let vnormal = vect_normal_norme vect_orientation *. (diameter/.2) in
	let pt_tangent = translation_vect_point vect_normal danger_cone.origin in
	let vect_tangent = {x = pt_tangent.x - robot.x ; y = pt_tangent.y - robot.y} in 
	let relat_ang_vtangent_v_orient = relative_angle vect_tangent vect_orientation in
	let norm_proj_vrobot_vorient = norm (project robot.speed obstacle.speed) in
	let vect_origincone_pointevectvitesse = add_subst {x = - robot.speed.x ; y = - robot.speed.y} (mult_scal (1 /. dt) vect_orientation) in
	(** Debut des conditions **)
	if relat_ang_vrobot_vorient > relat_ang_vtangent_v_orient then false 
	else if norm_proj_vrobot_vorient > (norm vect_orientation /. dt) then true
	else if norm vect_origincone_pointevectvitesse < danger_cone.diameter  then true
	else false

let get_vect_correction dt robot obstacle danger_cone=0

*)
