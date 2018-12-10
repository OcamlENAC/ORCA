let update (obj : Object.obj) dt =
	(** Retourne le nouveau point avec les vitesses résultantes de l'algo ORCA*)
	(* Object.set_obj_pos obj {Geometry.x = ((obj.position).x +. 2.) ; Geometry.y = ((obj.position).x +. 2.)}  *)
	Object.update_pos obj dt
