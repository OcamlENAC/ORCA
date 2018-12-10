type vector = {
	x: float;
	y: float;
}

type droite = {
	origin : vector;
	vect_dir : vector;
}

type disque = {
	origin : vector;
	diameter : float;
}

type semi_plan = {
	origin : vector;
	vect_normal : vector;
}

type cone_danger = {
	origin : vector;
	diameter : float; 
	orientation : vector; (*Vecteur AB, AB' = AB/dt*)
}

let scalar_product v1 v2 =
	(** Retourne le produit scalaire entre deux vecteurs *)
	v1.x *. v2.x +. v1.y *. v2.y 

let norm v = 
	(** Retourne la norme du vecteur v *)
	scalar_product v v

let project v1 v2 =
	(** Project vector v1 on v2 and return the projected vector *)
	(* v1' projeté de v1 sur v2 ssi v1' = (v1.v2)*v2/||v2|| *)
	let facteur = (scalar_product v1 v2) /. norm v2 in
	{x = facteur *. v2.x; y = facteur *. v2.y}

let relative_angle v1 v2 =
	(** retourne l'angle relatif entre 2 vecteurs *)
	acos ( (scalar_product v1 v2) /. ( norm v1 *. norm v2) )

let mult_scal dt v =
	{x=v.x *. dt ; y=v.y *. dt}

let add_subst oper v1 v2 =
	{x = oper v1.x v2.x ; y = oper v1.y v2.y} (*modifié pour pouvoir add ou subst*)

let vect_normal_norme v =
	let vnormal = {x = v.y; y = -(v.x)} in
	let norme = norm vnormal in
	{x = v.y /. norme; y = -(v.x) /. norme}

let translation_vect_point vect point = 
	{ x = point.x + vect.x ; y = point.y + vect.y }
	

let is_in_danger_zone dt robot obstacle = 
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
	let relat_ang_vrobot_vorient = relative_angle obstacle.vitesse robot.vitesse in
	let vnormal = vect_normal_norme vect_orientation *. (diameter/.2) in
	let pt_tangent = translation_vect_point vect_normal danger_cone.origin in
	let vect_tangent = {x = pt_tangent.x - robot.x ; y = pt_tangent.y - robot.y} in 
	let relat_ang_vtangent_v_orient = relative_angle vect_tangent vect_orientation in
	let norm_proj_vrobot_vorient = norm (project robot.speed obstacle.speed) in
	let vect_origincone_pointevectvitesse = add_subst {x = - robot.speed.x ; y = - robot.speed.y} (mult_scal (1 /. dt) vect_orientation) in
	(** Debut des conditions **)
	if relat_ang_vrobot_vorient > relat_ang_vtangent_v_orient then false 
	else if norm_proj_vrobot_vorient > (norm vect_orientation /. dt) then true
	else if norm vect_origincone_pointevectvitesse < danger_cone.diameter = then true
	else false



