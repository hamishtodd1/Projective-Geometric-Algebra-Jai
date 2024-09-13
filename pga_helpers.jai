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
e021 :: Point.{e021=1.}; e012 :: Point.{e021=-1.}; e013 :: Point.{e013=1.}; e031 :: Point.{e013=-1.}; e032 :: Point.{e032=1.}; e023 :: Point.{e032=-1.}; e123 :: Point.{e123=1.};
e0123 :: Dq.{e0123=1.};
e12 :: Line.{e12=1.}; e23 :: Line.{e23=1.}; e31 :: Line.{e31=1.}; e13 :: Line.{e31=-1.}; e01 :: Line.{e01=1.}; e02 :: Line.{e02=1.}; e03 :: Line.{e03=1.};

sq :: (x:float) -> float { return x * x; }

distance :: (a: Plane, point: Point) -> float { return e_norm(normalize(a) & normalize(point)); }
distance :: (a: Line,  point: Point) -> float { return e_norm(normalize(a) & normalize(point)); }
distance :: (a: Point, point: Point) -> float { return distance_prenormalized(normalize(a), normalize(point)); }

distance_prenormalized :: (a: Plane, b: Point) -> float { return e_norm(a & b); }
distance_prenormalized :: (a: Line,  b: Point) -> float { return e_norm(a & b); }
distance_prenormalized :: (a: Point, b: Point) -> float { v := a-b; return sqrt(v.x*v.x + v.y*v.y + v.z*v.z); }

point_trajectory_arc_length :: (dq:Dq,startPt:Point, numSamples := 8) -> float {

    ret := 0.;
    old, current : Point;
    for 0..numSamples-1 {
        current = pow(dq,i / (numSamples - 1)) >>> startPt;
        if i > 0
            ret += distance(old, current);
        old = current;
    }
    return ret;
}

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

make_point :: (x:=0.,y:=0.,z:=0., w:= 1.) -> Point {
    ret : Point = ---;
    ret.w = w; ret.x = x; ret.y = y; ret.z = z;
    return ret;
}

// velocityUnder(rotor, target) {
//     let rotorLogarithm = rotor.logarithm(newDq)
//     this.mul(rotorLogarithm, target)
//     for(let i = 0; i <5; ++i) {
//         if(!this.hasGrade(i))
//             target.zeroGrade(i)
//     }
//     return target
// }

to_vector3 :: (a:Point) -> Vector3 {

    ret : Vector3;
    if(a.w != 0.) {
        ret.x = a.x / a.w;
        ret.y = a.y / a.w;
        ret.z = a.z / a.w;
    }
    else {
        ret.x = a.x;
        ret.y = a.y;
        ret.z = a.z;
    }
    return ret;
}

to_point :: (a:Vector3) -> Point {
    return Point.{a.z, a.y, a.x, 1.};
}