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

type d_cone = {
	(** 	origin = centre du cercle de rayon r = AB/tau
		rayon = du cercle de taille ab/tau
		vect = ab'
	*)
	origin: vector;
	rayon : float;
	vect: vector;
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

let mult_scal dt v =
	{x=v.x *. dt ; y=v.y *. dt}

let add_subst oper v1 v2 =
	{x = oper v1.x v2.x ; y = oper v1.y v2.y} (*modifié pour pouvoir add ou subst*)

let vect_normal_norme v =
	let vnormal = {x = v.y ; y = -. v.x} in
	let norme = norm vnormal in
	{x = v.y /. norme; y = (-. v.x) /. norme}

let translation_vect_point vect point = 
	{ x = point.x +. vect.x ; y = point.y +. vect.y }

let get_unit_vector v =
	mult_scal (1. /. norm v) v

let determinant u v =
	u.x *. v.y -. u.y *. v.x

let relative_angle u v =
	let pi = 3.141592653589793 in
	let angle = ref 0. in
	angle := (atan2 v.y v.x) -. (atan2 u.y u.x);
	while !angle >= pi do angle := !angle -. (2. *. pi) done;
	while !angle <= (-. pi) do angle := !angle +. (2. *. pi) done;
	!angle
	
