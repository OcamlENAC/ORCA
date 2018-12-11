
type obj = {
	
	position : Geometry.vector;
	speed : Geometry.vector;
	diameter : float;
	dest : Geometry.vector;
	max_speed : float;
}
	
let init px py sx sy d destx desty ms =
	(** Renvoit un object avec les caracteristiques ci dessus *)
	{position = {Geometry.x=px;Geometry.y=py};
	speed = {Geometry.x=sx;Geometry.y=sy};
	diameter = d;
	dest = {Geometry.x = destx ; Geometry.y = desty};
	max_speed = ms;}

let init_from_pos px py =
	(** Initialise un objet à la position px py *)
	let sx = 5. and sy = 1. and d = 20. and destx = 0. and desty = 0. and ms = 100. in
	init px py sx sy d destx desty ms


let set_obj_pos o new_pos = 
	{position=new_pos;speed=o.speed;diameter=o.diameter;dest=o.dest;max_speed=o.max_speed}


let set_obj_speed o new_speed = 
	{position=o.position;speed=new_speed;diameter=o.diameter;dest=o.dest;max_speed=o.max_speed}

let update_pos o refreshin_time =
	let new_pos = Geometry.add o.position (Geometry.mult_scal refreshing_time o.speed) in
	set_obj_pos o new_pos

let calc_opt_speed obj =
	(** Calcul la vitesse optimale sans obstacle d'un objet pour qu'il atteigne sa destination *)
	let vdirection_opt_speed ={Geometry.x = obj.dest.x - obj.position.x ; Geometry.y = obj.dest.y - obj.position.y} in
	let vdirection_norme = Geometry.get_unit_vector vdirection_opt_speed in
	let opt_speed = Geometry.mult_scal obj.max_speed vdirection_norme in
	opt_speed



