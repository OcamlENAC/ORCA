
let is_in_danger_zone (robot : Object.obj) (obstacle : Object.obj) (d_cone : Geometry.d_cone) dt = 
	let v_relat = Geometry.add_subst ( -. ) robot.speed obstacle.speed in 
	let v_orient =  Geometry.add_subst ( -. ) robot.position obstacle.position in 
	(****Verifier si la pointe du v Vopti est dans le cone****)
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


let correct (objA: Object.obj) (objB : Object.obj) correction =
	let new_v_A = Geometry.add_subst (+.) objA.speed (Geometry.mult_scal 0.5 correction) 
	and new_v_B = Geometry.add_subst (-.) objB.speed (Geometry.mult_scal 0.5 correction) in
	let new_objA = Object.set_obj_speed objA new_v_A
	and new_objB = Object.set_obj_speed objB new_v_B in
	(new_objA, new_objB)

let calc_danger_cone  (robot : Object.obj) (obstacle: Object.obj) dt =
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

let render_cone (robot : Object.obj) (obstacle : Object.obj) (d_cone : Geometry.d_cone) in_danger dt =
	Graphics.set_color Graphics.green;
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.lineto (int_of_float (d_cone.vect.x *. 100.)) (int_of_float (d_cone.vect.y *. 100.));
	let angle = Geometry.get_angle_cone d_cone in
	let adj_vect = Geometry.get_adj_vect_cone d_cone in
	let adj_vect2 = Geometry.rotate_vect adj_vect ((-. 2.) *. angle) in
	let longueur = 500. in
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.lineto (int_of_float (robot.position.x +. adj_vect.x *. longueur)) (int_of_float (robot.position.y +. adj_vect.y *. longueur));
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.lineto (int_of_float (robot.position.x +. adj_vect2.x *. longueur)) (int_of_float (robot.position.y +. adj_vect2.y *. longueur));
	Graphics.draw_circle (int_of_float (d_cone.origin.x)) (int_of_float (d_cone.origin.y)) (int_of_float d_cone.rayon);
	Graphics.draw_circle (int_of_float (d_cone.origin.x *. dt)) (int_of_float (d_cone.origin.y *. dt)) (int_of_float (d_cone.rayon *. dt));
	Graphics.moveto 50 400;
	Graphics.set_color Graphics.black;
	Graphics.set_text_size 50;
	if in_danger then Graphics.draw_string "DANS LE cone !" else Graphics.draw_string "np";
	let v_relat_x = robot.speed.x -. obstacle.speed.x in
	let v_relat_y = robot.speed.y -. obstacle.speed.y in
	Graphics.moveto (int_of_float robot.position.x) (int_of_float robot.position.y);
	Graphics.set_color Graphics.red;
	Graphics.lineto (int_of_float (robot.position.x +. v_relat_x)) (int_of_float (robot.position.y +. v_relat_y))



let get_correction vr (d_cone : Geometry.d_cone) = (*Printf.printf "%f %f  " proj_vr_cone.x proj_vr_cone.y;*)
	let r_angle = Geometry.relative_angle d_cone.vect vr in
	let alpha = Geometry.get_angle_cone d_cone in
	if r_angle < 0. then 
	let proj_vr_cone = Geometry.rotate_vect vr ((-. alpha) -. r_angle) in
	Geometry.add_subst (-.) proj_vr_cone vr
	else 
	let proj_vr_cone = Geometry.rotate_vect vr (alpha -. r_angle) in
	Geometry.add_subst (-.) proj_vr_cone vr 

let new_speed objA objB in_danger d_cone =
	(* S'il n'y a pas de risque de collision alors on calcul leur V_opti
		et on renvoit  A et B avec des vitesses modifiées *)
	if not in_danger then
		let new_v_A = Object.calc_opt_speed objA
		and new_v_B = Object.calc_opt_speed objB in
		let new_objA = Object.set_obj_speed objA new_v_A
		and new_objB = Object.set_obj_speed objB new_v_B in
		(new_objA, new_objB)
	else 
		let vr = Geometry.add_subst (-.) objA.speed objB.speed in
		let correction = get_correction vr d_cone in
		correct objA objB correction
		


let update objects refreshing_time =
	(** Retourne le nouveau point avec les vitesses résultantes de l'algo ORCA*)
	(* Object.set_obj_pos obj {Geometry.x = ((obj.position).x +. 2.) ; Geometry.y = ((obj.position).x +. 2.)}  *)
	(*
	calc_cone puis is_in_danger puis new_speed

	doit renvoyer Array avec objA et objB avec vitesses et positions modifiées
	*)
	let dt = 10. in
	let objA = objects.(0) and objB = objects.(1) in
	let d_cone = calc_danger_cone objA objB dt in
	let in_danger = is_in_danger_zone objA objB d_cone dt in
	Printf.printf "%B" in_danger; 
	render_cone objA objB d_cone in_danger dt;
	let (new_objA, new_objB) = (new_speed objA objB in_danger d_cone) in
	
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
	let d_cone = create_cone dt robot obstacle in 
	(**** Calcul des composants du cone ****)
	let vect_orientation = add_subst ( - ) obstacle.position robot.position in 
	let origin_cone_x = robot.position.x +. (obstacle.position.x -. robot.position.x)/. dt in (* on appelle origine le centre du petit cercle ici*)
	let origin_cone_y = robot.position.y +. (obstacle.position.y -. robot.position.y)/. dt in
	let origin_cone_vect = {x = origin_cone_x; y = origin_cone_y} in
	let danger_diameter = (obstacle.diameter + robot.diameter) *. norm (vect_orientation) /. dt in
	(**** Def du cone de danger ****)
	let d_cone = { origin = origin_cone_vect; diameter = danger_diameter; orientation = vect_orientation} in
	(****Verifier si la pointe du v Vopti est dans le cone****)
	(*On regarde dans l'ordre angle relatif ? -> avant le cercle ? -> après le cercle ? -> dans le cercle ?*)
	(*Angle relatif et autres pour les conditions*) 
	let relat_ang_vrobot_vorient = relative_angle abs_float obstacle.vitesse abs_float vect_orientation in
	let vnormal = vect_normal_norme vect_orientation *. (diameter/.2) in
	let pt_tangent = translation_vect_point vect_normal d_cone.origin in
	let vect_tangent = {x = pt_tangent.x - robot.x ; y = pt_tangent.y - robot.y} in 
	let relat_ang_vtangent_v_orient = relative_angle vect_tangent vect_orientation in
	let norm_proj_vrobot_vorient = norm (project robot.speed obstacle.speed) in
	let vect_origincone_pointevectvitesse = add_subst {x = - robot.speed.x ; y = - robot.speed.y} (mult_scal (1 /. dt) vect_orientation) in
	(** Debut des conditions **)
	if relat_ang_vrobot_vorient > relat_ang_vtangent_v_orient then false 
	else if norm_proj_vrobot_vorient > (norm vect_orientation /. dt) then true
	else if norm vect_origincone_pointevectvitesse < d_cone.diameter  then true
	else false

let get_vect_correction dt robot obstacle d_cone=0

*)
