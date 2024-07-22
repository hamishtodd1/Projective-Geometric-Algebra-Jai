/*
    TODO operator that takes two Vector3s and multiplies them!
    Heh if they're just translators it just adds them
    But you can implement Vector3 * Quaternion, that's cool
*/

one_dq :: Dq.{
    // elements=.[1.,0.,0.,0., 0.,0.,0.,0.]
    e=1.
    };
e0 :: Plane.{e0=1.}; e1 :: Plane.{e1=1.}; e2 :: Plane.{e2=1.}; e3 :: Plane.{e3=1.};
e021 :: Point.{e021=1.}; e013 :: Point.{e013=1.}; e032 :: Point.{e032=1.}; e123 :: Point.{e123=1.};
// e01 :: #run mul(e0, e1);
// e02 : Dq : #run mul(e0, e2);
// e03 : Dq : #run mul(e0, e3);


// e12
// e21

rotation_from_axis_angle :: ( axis_vec3 : Vector3, angle : float) -> Dq {

    //step 1: turn vector into a line
    vec_length := sqrt(sq(axis_vec3.x) + sq(axis_vec3.y) + sq(axis_vec3.z));
    vec_as_quat_axis : Dq;
    vec_as_quat_axis[4] = axis_vec3.x / vec_length;
    vec_as_quat_axis[5] = axis_vec3.y / vec_length;
    vec_as_quat_axis[6] = axis_vec3.z / vec_length;

    //step 2: get the rotation!
    return rotation_from_axis_angle(vec_as_quat_axis, angle);
}

//assumes axis is normalized
rotation_from_axis_angle :: ( axis_line : Dq, angle : float) -> Dq {

    ret := sin( angle / 2. ) * axis_line;
    ret[0] = cos(angle / 2.);
    return ret;
}

make_translator :: ( x: float = 0., y : float = 0., z : float=0.)->Dq {
    
    ret := one_dq;

    ret[1] = -.5 * x;
    ret[2] = -.5 * y;
    ret[3] = -.5 * z;

    return ret;
}


point :: (x:float,y:float,z:float, w:float = 1.) -> Fl {

    ret : Fl;
    ret[7] = w;
    ret[6] = x;
    ret[5] = y;
    ret[4] = z;
    return ret;
}

point_to_vector :: (myFl:Fl) -> Vector3 {
    ret : Vector3;
    ret.x = myFl[6] / myFl[7];
    ret.y = myFl[5] / myFl[7];
    ret.z = myFl[4] / myFl[7];
    return ret;
}