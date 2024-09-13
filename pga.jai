/*
    This library was created by Hamish Todd and is released into the public domain

    Here are the data structurs it defines:
        Plane - which is the same thing as a planar reflection
        Point - which is the same thing as a point reflection
        Line  - which is the same thing as a line reflection, which is just a 180
        Dq "Dual Quaternion" - rigid transformation; rotation/translation/screw motion
        Fl "Flector" - handedness-reversing distance-preserving motion; reflection/rotoreflection/transflection

    Note that it is theoretically possible for a "line" to be not exactly a line.
    An example of this is e12 + e03. This is instead an object called a "screw axis"
    These don't show up too often though, so it's not far wrong at all to think of a line as a line

    And some operations
        Geometric product A * B
            The daddy! Does transform composition
            If you want a rotation followed by another rotation, that's their geometric product
        Sandwich A >>> B
            "Application" of A to B
            If A is a rotation and B is a point, it will give you that point rotated by A
        Wedge product ^
            "Meet" of A and B
            If A and B are planes, it is a line where they meet
        Join A & B
            "Incident" or "containing" A and B
            If A and B are both points, A & B is the line on which they both lie
        Inner product |
            "Orthogonal or at least incident" with A and B
            For example, the line which is orthogonal to plane A and passing through point B is A|B
        Addition and subtraction + and -
            "Mixture" of A and B
            If A and B are planes, A+B and A-B are planes directly between them
        Linear interpolation: lerp(A,B,t)
            A and B are mixed with a having "weight" t and B having the rest
        Euclidean norm: e_norm(A)
            The "magnitude" of an ordinary object
            If A and B are planes and A's magnitude is higher than B's, then A+B will be closer to A than B
        Reverse: ~
            Essentially the inverse but ignores normalization (which rarely matters), so is much faster
            Since everything is homogeneous
        Square root: sqrt(A)
            "Halfway" between A and the identity, mostly useful when you want a transformation taking you from A to B
            If A and B are both planes, sqrt(A*~B) is a rotation or translation from one toward the other
        Exponential: exp(L*t)
            For (normalized) line L and scalar t, this is a rotation or translation by t (radians or meters) about L
        Logarithm: logarithm(A)
            Opposite of exponential, gets you a line scaled by angle/distance
        slerp(A,B,t)
            "Spherical linear interpolation" between transforms A and B; looks nicer than lerp
        Derivative: velocity_under()
*/

/*
    TODO
        Maybe rename Dual Quat to a short version of "Handness preserving transfomation"
        and Flector to "Handness reversing transfomation"

    You will find #symmetric in very few places - that's because GA ops are mostly non-symmetric, some anti-symmetric, and some symmetric!
*/

#scope_file
eps :: .0001; //not been tweaked for anything yet, so feel free to change
#scope_export

//Flector: point or plane or rotoreflection (reflection followed by translation), transflection (reflection followed by translation)
Fl :: struct {
    e0, e1, e2, e3 : float;         //plane part
    e021, e013, e032, e123 : float; //point part

    /*
    the layout here is such that:
    planes have d, x, y, z
    points have z, y, x, w
    possibly this should be changed!
    possibly this should be decided by the user and then made good on at compile time
    */
#place e0;
    elements : [8] float = ---;
#place e0;
    plane : Plane = ---;
#place e021;
    point : Point = ---;
}

//Dual Quaternion: line, rotation, translation, or screw motion
Dq :: struct {
    e : float;             // identity part - "how much is this transformation like a 0-degree rotation"

    //maybe want this to be a single "Line" called "linePart"
    e01, e02, e03 : float; // line-at-infinity part
    e12, e31, e23 : float; // line-through-the-origin part
    
    e0123 : float;         // screwiness part - "how much is this transformation like a 180-degree rotation followed by a very large translation"
#place e;
    elements : [8] float = ---;
#place e01;
    line : Line = ---;
}

//TODO test!!!
to_quaternion :: (a:Dq) -> Quaternion {
    return Quaternion.{w = a.e, x = a.e23, y = -a.e31, z = a.e12};
}

//Requires normalization!
to_mat4 :: (a:Dq) -> Matrix4 {

    r44 := a[4] * a[4];
    r55 := a[5] * a[5];
    r66 := a[6] * a[6];

    r45 := a[4] * a[5];
    r46 := a[4] * a[6];
    r56 := a[5] * a[6];

    r04 := a[0] * a[4];
    r05 := a[0] * a[5];
    r06 := a[0] * a[6];

    m : Matrix4;
    m._11 = -r44 - r55; m._12 =  r04 + r56; m._13 =  r46 - r05; m._14 = a[3] * a[5] - a[2] * a[4] - a[0] * a[1] - a[7] * a[6];
    m._21 =  r56 - r04; m._22 = -r44 - r66; m._23 =  r06 + r45; m._24 = a[1] * a[4] - a[3] * a[6] - a[0] * a[2] - a[7] * a[5];
    m._31 =  r05 + r46; m._32 =  r45 - r06; m._33 = -r55 - r66; m._34 = a[2] * a[6] - a[1] * a[5] - a[0] * a[3] - a[7] * a[4];

    return m * 2. + Matrix4_Identity;
}

Line :: struct {
    e01, e02, e03 : float; // line-at-infinity part
    e12, e31, e23 : float; // line-through-the-origin part

#place e01;
    elements : [6] float = ---;
}

Plane :: struct {
    e0, e1, e2, e3 : float;
#place e0;
    elements : [4] float = ---;
}

Point :: struct {
    //z,y,x... awful. Should be xyz, could then address it that way
    //but changing that would require either a lot of changes or a lot of changes or inconsistency with fl
    e021, e013, e032 : float;
    e123 : float = 1.;

//only think of these as actually being x,y,z if you know what "homogeneous" means
//Eg the *actual* x, y, z of the point is x/w, y/w, z/w
#place e021;
    z : float = ---;
#place e013;
    y : float = ---;
#place e032;
    x : float = ---;
#place e123;
    w : float = ---;

#place e021;
    elements : [4] float = ---;
}

operator *[] :: inline(b: *Plane, index: int) -> *float { return *b.elements[index]; }
operator *[] :: inline(b: *Point, index: int) -> *float { return *b.elements[index]; }
operator *[] :: inline(b: *Line,  index: int) -> *float { return *b.elements[index]; }
operator *[] :: inline(b: *Dq,    index: int) -> *float { return *b.elements[index]; }
operator *[] :: inline(b: *Fl,    index: int) -> *float { return *b.elements[index]; }

cst :: (a:Plane) -> Fl { return Fl.{plane=a}; }
cst :: (a:Point) -> Fl { return Fl.{point=a}; }
cst :: (l:Line ) -> Dq { return Dq.{line=l}; }
cst :: (f:float) -> Dq { return Dq.{e=a}; }

is_k_vector :: (a:Type) -> bool {
    ret := false;
    if a == {
        case float; ret = true;
        case Point; ret = true; 
        case Line; ret = true; 
        case Plane; ret = true;
    }
    return ret;
}
is_full_versor :: (a:Type) -> bool {
    ret := false;
    if a == {
        case Dq; ret = true;
        case Fl; ret = true; 
    }
    return ret;
}
is_mv :: (a:Type) -> bool {
    ret := false;
    if a == {
        case Dq; ret = true;
        case Fl; ret = true; 
        case float; ret = true;
        case Point; ret = true; 
        case Line; ret = true; 
        case Plane; ret = true;
    }
    return ret;
}

//note that a|a is not the same as a*a, but rather the squared norm of a
e_norm_sq :: (a:$T) -> float 
#modify{ return is_k_vector(T); }
{ return a|a; }

e_norm    :: (a:$T) -> float 
#modify{ return is_k_vector(T); } //sorta false in the "line"-line case!
{ return sqrt(abs(a|a)); }

e_norm    :: (a:float) -> float
{ return abs(a); }

normalize :: (a:Point) -> Point { return a / a.e123; }
normalize :: (a:Plane) -> Plane { return a / e_norm(a); }
normalize :: (a:Line ) -> Line  { return a / e_norm(a); }
normalize :: (r:Dq) -> Dq {
    s := 1. / sqrt(r[0]*r[0] + r[4]*r[4] + r[5]*r[5] + r[6]*r[6]);
    d := (r[7]*r[0] - (r[1]*r[6] + r[2]*r[5] + r[3]*r[4]))*s*s;
    ret := r*s;
    ret[1] += ret[6]*d; ret[2] += ret[5]*d; ret[3] += ret[4]*d;
    ret[7] -= ret[0]*d;
    return ret;
}

sqrt :: (r:Dq) -> Dq {
    en := sqrt(abs((r * ~r)[0]));
    scalar_would_be_zero := r[0] == en;
    return ifx !scalar_would_be_zero then r+en else r-en;
}

exp :: (b:Line) -> Dq {
    l := b[3]*b[3] + b[4]*b[4] + b[5]*b[5];
    if l==0.
        return Dq.{elements=.[1., b[0], b[1], b[2], 0., 0., 0., 0.]};
    m := (b[0]*b[5] + b[1]*b[4] + b[2]*b[3]);
    a := sqrt(l); c := cos(a); s := sin(a)/a; t := m/l*(c-s);
    return Dq.{elements=.[c, s*b[0] + t*b[5], s*b[1] + t*b[4], s*b[2] + t*b[3], s*b[3], s*b[4], s*b[5], m*s]};
}

logarithm_prenormalized :: (r:Dq) -> Line {
    if n[4] == 0. && n[5] == 0. && n[6] == 0.
            return Line.{elements=.[0., n[1], n[2], n[3], 0., 0., 0., 0.]};

    a := 1. / (1. - n[0] * n[0]);
    b := acos(n[0]) * sqrt(a);
    c := a * n[7] * (1. - n[0] * b);
    return Line.{elements=.[c * n[6] + b * n[1], c * n[5] + b * n[2], c * n[4] + b * n[3], b * n[4], b * n[5], b * n[6]]};
}

logarithm :: (r:Dq) -> Line { return logarithm_prenormalized(normalize(r)); }

pow :: (a:Dq, b:float) -> Dq { return exp(b*logarithm(a)); }
pow_prenormalized :: (a:Dq, b:float) -> Dq { return exp(b*logarithm_prenormalized(a)); }

slerp :: (start:Dq, end:Dq, t:float) -> Dq { return pow(end * ~start, t) * start; }
lerp  :: (start:$T, end:T,  t:float) -> T  { return start + (end - start) * t; }

// casting in the opposite direction is more like select_grade, you need to think about this

operator * :: (a:Fl, b:Dq) -> Fl
{
    ret:Fl;

    ret[0] = + b[0] * a[0] - b[1] * a[1] - b[2] * a[2] - b[3] * a[3] + b[4] * a[4] + b[5] * a[5] + b[6] * a[6] + b[7] * a[7];
    ret[1] = + b[0] * a[1] - b[4] * a[2] + b[5] * a[3] - b[6] * a[7];
    ret[2] = + b[4] * a[1] + b[0] * a[2] - b[6] * a[3] - b[5] * a[7];
    ret[3] = - b[5] * a[1] + b[6] * a[2] + b[0] * a[3] - b[4] * a[7];

    ret[4] = - b[4] * a[0] + b[2] * a[1] - b[1] * a[2] + b[7] * a[3] + b[0] * a[4] + b[6] * a[5] - b[5] * a[6] + b[3] * a[7];
    ret[5] = - b[5] * a[0] - b[3] * a[1] + b[7] * a[2] + b[1] * a[3] - b[6] * a[4] + b[0] * a[5] + b[4] * a[6] + b[2] * a[7];
    ret[6] = - b[6] * a[0] + b[7] * a[1] + b[3] * a[2] - b[2] * a[3] + b[5] * a[4] - b[4] * a[5] + b[0] * a[6] + b[1] * a[7];
    ret[7] = + b[6] * a[1] + b[5] * a[2] + b[4] * a[3] + b[0] * a[7];

    return ret;
}

operator * :: (a:Fl, b:Fl) -> Dq
{
    ret:Dq;
    ret[0] = + b[1] * a[1] + b[2] * a[2] + b[3] * a[3] - b[7] * a[7];

    ret[1] = + b[1] * a[0] - b[0] * a[1] - b[4] * a[2] + b[5] * a[3] - b[2] * a[4] + b[3] * a[5] + b[7] * a[6] - b[6] * a[7];
    ret[2] = + b[2] * a[0] + b[4] * a[1] - b[0] * a[2] - b[6] * a[3] + b[1] * a[4] + b[7] * a[5] - b[3] * a[6] - b[5] * a[7];
    ret[3] = + b[3] * a[0] - b[5] * a[1] + b[6] * a[2] - b[0] * a[3] + b[7] * a[4] - b[1] * a[5] + b[2] * a[6] - b[4] * a[7];
    ret[4] = + b[2] * a[1] - b[1] * a[2] + b[7] * a[3] + b[3] * a[7];
    ret[5] = - b[3] * a[1] + b[7] * a[2] + b[1] * a[3] + b[2] * a[7];
    ret[6] = + b[7] * a[1] + b[3] * a[2] - b[2] * a[3] + b[1] * a[7];

    ret[7] = + b[7] * a[0] + b[6] * a[1] + b[5] * a[2] + b[4] * a[3] - b[3] * a[4] - b[2] * a[5] - b[1] * a[6] - b[0] * a[7];

    return ret;
}

operator * :: (a:Dq, b:Dq) -> Dq {

    ret:Dq;
	ret[0] = + b[0] * a[0] - b[4] * a[4] - b[5] * a[5] - b[6] * a[6];

    ret[1] = + b[1] * a[0] + b[0] * a[1] - b[4] * a[2] + b[5] * a[3] + b[2] * a[4] - b[3] * a[5] - b[7] * a[6] - b[6] * a[7];
    ret[2] = + b[2] * a[0] + b[4] * a[1] + b[0] * a[2] - b[6] * a[3] - b[1] * a[4] - b[7] * a[5] + b[3] * a[6] - b[5] * a[7];
    ret[3] = + b[3] * a[0] - b[5] * a[1] + b[6] * a[2] + b[0] * a[3] - b[7] * a[4] + b[1] * a[5] - b[2] * a[6] - b[4] * a[7];
    ret[4] = + b[4] * a[0] + b[0] * a[4] + b[6] * a[5] - b[5] * a[6];
    ret[5] = + b[5] * a[0] - b[6] * a[4] + b[0] * a[5] + b[4] * a[6];
    ret[6] = + b[6] * a[0] + b[5] * a[4] - b[4] * a[5] + b[0] * a[6];

    ret[7] = + b[7] * a[0] + b[6] * a[1] + b[5] * a[2] + b[4] * a[3] + b[3] * a[4] + b[2] * a[5] + b[1] * a[6] + b[0] * a[7];

    return ret;
}

operator * :: (a:Dq, b:Fl) -> Fl {

    ret : Fl;
	ret[0] = + b[0] * a[0] + b[1] * a[1] + b[2] * a[2] + b[3] * a[3] + b[4] * a[4] + b[5] * a[5] + b[6] * a[6] - b[7] * a[7];
    ret[1] = + b[1] * a[0] + b[2] * a[4] - b[3] * a[5] - b[7] * a[6];
    ret[2] = + b[2] * a[0] - b[1] * a[4] - b[7] * a[5] + b[3] * a[6];
    ret[3] = + b[3] * a[0] - b[7] * a[4] + b[1] * a[5] - b[2] * a[6];

    ret[4] = + b[4] * a[0] - b[2] * a[1] + b[1] * a[2] - b[7] * a[3] - b[0] * a[4] + b[6] * a[5] - b[5] * a[6] - b[3] * a[7];
    ret[5] = + b[5] * a[0] + b[3] * a[1] - b[7] * a[2] - b[1] * a[3] - b[6] * a[4] - b[0] * a[5] + b[4] * a[6] - b[2] * a[7];
    ret[6] = + b[6] * a[0] - b[7] * a[1] - b[3] * a[2] + b[2] * a[3] + b[5] * a[4] - b[4] * a[5] - b[0] * a[6] - b[1] * a[7];
    ret[7] = + b[7] * a[0] + b[3] * a[4] + b[2] * a[5] + b[1] * a[6];

    return ret;
}

operator ^ :: (a:Fl, b:Dq) -> Fl
{
    ret:Fl;

    ret[0] = + b[0] * a[0];
    ret[1] = + b[0] * a[1];
    ret[2] = + b[0] * a[2];
    ret[3] = + b[0] * a[3];

    ret[4] = - b[4] * a[0] + b[2] * a[1] - b[1] * a[2] + b[0] * a[4];
    ret[5] = - b[5] * a[0] - b[3] * a[1] + b[1] * a[3] + b[0] * a[5];
    ret[6] = - b[6] * a[0] + b[3] * a[2] - b[2] * a[3] + b[0] * a[6];
    ret[7] = + b[6] * a[1] + b[5] * a[2] + b[4] * a[3] + b[0] * a[7];

    return ret;
}

operator ^ :: (a:Fl, b:Fl) -> Dq {

    ret:Dq;
    ret[0] = 0.;

    ret[1] = + b[1] * a[0] - b[0] * a[1];
    ret[2] = + b[2] * a[0] - b[0] * a[2];
    ret[3] = + b[3] * a[0] - b[0] * a[3];
    ret[4] = + b[2] * a[1] - b[1] * a[2];
    ret[5] = - b[3] * a[1] + b[1] * a[3];
    ret[6] = + b[3] * a[2] - b[2] * a[3];

    ret[7] = + b[7] * a[0] + b[6] * a[1] + b[5] * a[2] + b[4] * a[3] - b[3] * a[4] - b[2] * a[5] - b[1] * a[6] - b[0] * a[7];

    return ret;
}

operator ^ :: (a:Dq, b:Dq) -> Dq{

    ret:Dq;
	ret[0] = + b[0] * a[0];

    ret[1] = + b[1] * a[0] + b[0] * a[1];
    ret[2] = + b[2] * a[0] + b[0] * a[2];
    ret[3] = + b[3] * a[0] + b[0] * a[3];
    ret[4] = + b[4] * a[0] + b[0] * a[4];
    ret[5] = + b[5] * a[0] + b[0] * a[5];
    ret[6] = + b[6] * a[0] + b[0] * a[6];

    ret[7] = + b[7] * a[0] + b[6] * a[1] + b[5] * a[2] + b[4] * a[3] + b[3] * a[4] + b[2] * a[5] + b[1] * a[6] + b[0] * a[7];

    return ret;
}

operator ^ :: (a:Dq, b:Fl) -> Fl {

    ret:Fl;
	ret[0] = + b[0] * a[0];
    ret[1] = + b[1] * a[0];
    ret[2] = + b[2] * a[0];
    ret[3] = + b[3] * a[0];

    ret[4] = + b[4] * a[0] - b[2] * a[1] + b[1] * a[2] - b[0] * a[4];
    ret[5] = + b[5] * a[0] + b[3] * a[1] - b[1] * a[3] - b[0] * a[5];
    ret[6] = + b[6] * a[0] - b[3] * a[2] + b[2] * a[3] - b[0] * a[6];
    ret[7] = + b[7] * a[0] + b[3] * a[4] + b[2] * a[5] + b[1] * a[6];

    return ret;
}

operator | :: (a:Fl, b:Dq) -> Fl
{
    ret:Fl;
    ret[0] = + b[0] * a[0] - b[1] * a[1] - b[2] * a[2] - b[3] * a[3] + b[4] * a[4] + b[5] * a[5] + b[6] * a[6] + b[7] * a[7];
    ret[1] = + b[0] * a[1] - b[4] * a[2] + b[5] * a[3] - b[6] * a[7];
    ret[2] = + b[4] * a[1] + b[0] * a[2] - b[6] * a[3] - b[5] * a[7];
    ret[3] = - b[5] * a[1] + b[6] * a[2] + b[0] * a[3] - b[4] * a[7];

    ret[4] = + b[7] * a[3] + b[0] * a[4];
    ret[5] = + b[7] * a[2] + b[0] * a[5];
    ret[6] = + b[7] * a[1] + b[0] * a[6];
    ret[7] = + b[0] * a[7];

    return ret;
}

operator | :: (a:Fl, b:Fl) -> Dq {

    ret:Dq;
    ret[0] = + b[1] * a[1] + b[2] * a[2] + b[3] * a[3] - b[7] * a[7];

    ret[1] = - b[4] * a[2] + b[5] * a[3] - b[2] * a[4] + b[3] * a[5];
    ret[2] = + b[4] * a[1] - b[6] * a[3] + b[1] * a[4] - b[3] * a[6];
    ret[3] = - b[5] * a[1] + b[6] * a[2] - b[1] * a[5] + b[2] * a[6];
    ret[4] = + b[7] * a[3] + b[3] * a[7];
    ret[5] = + b[7] * a[2] + b[2] * a[7];
    ret[6] = + b[7] * a[1] + b[1] * a[7];

    ret[7] = 0.;

    return ret;
}

operator | :: (a:Dq, b:Dq) -> Dq {

    ret:Dq;
	ret[0] = + b[0] * a[0] - b[4] * a[4] - b[5] * a[5] - b[6] * a[6];

    ret[1] = + b[1] * a[0] + b[0] * a[1] - b[7] * a[6] - b[6] * a[7];
    ret[2] = + b[2] * a[0] + b[0] * a[2] - b[7] * a[5] - b[5] * a[7];
    ret[3] = + b[3] * a[0] + b[0] * a[3] - b[7] * a[4] - b[4] * a[7];
    ret[4] = + b[4] * a[0] + b[0] * a[4];
    ret[5] = + b[5] * a[0] + b[0] * a[5];
    ret[6] = + b[6] * a[0] + b[0] * a[6];

    ret[7] = + b[7] * a[0] + b[0] * a[7];

    return ret;
}

operator | :: (a:Dq, b:Fl) -> Fl{

    ret:Fl;
	ret[0] = + b[0] * a[0] + b[1] * a[1] + b[2] * a[2] + b[3] * a[3] + b[4] * a[4] + b[5] * a[5] + b[6] * a[6] - b[7] * a[7];
    ret[1] = + b[1] * a[0] + b[2] * a[4] - b[3] * a[5] - b[7] * a[6];
    ret[2] = + b[2] * a[0] - b[1] * a[4] - b[7] * a[5] + b[3] * a[6];
    ret[3] = + b[3] * a[0] - b[7] * a[4] + b[1] * a[5] - b[2] * a[6];
    
    ret[4] = + b[4] * a[0] - b[3] * a[7];
    ret[5] = + b[5] * a[0] - b[2] * a[7];
    ret[6] = + b[6] * a[0] - b[1] * a[7];
    ret[7] = + b[7] * a[0];

    return ret;
}

operator ~ :: (a: Dq) -> Dq { return Dq.{ e = a.e, line = -a.line, e0123 = a.e0123}; }
operator ~ :: (a: Fl) -> Fl { return Fl.{ plane = a.plane, point = -a.point}; }
operator ~ :: (a: Line ) -> Line  { return -a; }
operator ~ :: (a: Plane) -> Plane { return  a; }
operator ~ :: (a: Point) -> Point { return -a; }

// probably only useful if you can't be bothered to convert b to point
operator & :: (a:Fl, b:Fl) -> Dq {

    ret:Dq;
	ret[7] = 0.;
    
    ret[6] = + a[6] * b[3] - a[7] * b[2];
    ret[5] = + a[5] * b[3] - a[7] * b[1];
    ret[4] = + a[4] * b[3] - a[7] * b[0];
    ret[3] = + a[5] * b[2] - a[6] * b[1];
    ret[2] = - a[4] * b[2] + a[6] * b[0];
    ret[1] = + a[4] * b[1] - a[5] * b[0];
    
    ret[0] = + a[0] * b[3] + a[1] * b[2] + a[2] * b[1] - a[3] * b[0];

    return ret;
}

operator & :: (a:Dq, b:Point) -> Fl {

    ret:Fl;
    ret[0] = + a[1] * b[2] + a[2] * b[1] + a[3] * b[0];
    ret[1] = - a[1] * b[3] + a[4] * b[1] - a[5] * b[0];
    ret[2] = - a[2] * b[3] - a[4] * b[2] + a[6] * b[0];
	ret[3] = - a[3] * b[3] + a[5] * b[2] - a[6] * b[1];
    
    //needed for align, e0123 is the identity!
    ret[4] = - a[7] * b[0];
    ret[5] = - a[7] * b[1];
    ret[6] = - a[7] * b[2];
	ret[7] = - a[7] * b[3];

    return ret;
}

operator & :: (a:Line, b:Point) -> Plane {

    ret:Plane;
    ret[0] = + a[0] * b[2] + a[1] * b[1] + a[2] * b[0];
    ret[1] = - a[0] * b[3] + a[3] * b[1] - a[4] * b[0];
    ret[2] = - a[1] * b[3] - a[3] * b[2] + a[5] * b[0];
	ret[3] = - a[2] * b[3] + a[4] * b[2] - a[5] * b[1];

    return ret;
}

operator & :: (a:Point, b:Point) -> Line {

    ret:Line;
    
    ret[ 5] = + a[2] * b[3] - a[3] * b[2];
    ret[ 4] = + a[1] * b[3] - a[3] * b[1];
    ret[ 3] = + a[0] * b[3] - a[3] * b[0];
    ret[ 2] = + a[1] * b[2] - a[2] * b[1];
    ret[ 1] = - a[0] * b[2] + a[2] * b[0];
    ret[ 0] = + a[0] * b[1] - a[1] * b[0];

    return ret;
}

operator & :: (a:Plane, b:Point) -> float {
    return a[0] * b[3] + a[1] * b[2] + a[2] * b[1] - a[3] * b[0];
}

operator / :: (a:$T, b:float) -> T
#modify { return is_mv(T); }
{ return a * (1./b); }

operator / :: (a:Plane, b:Plane) -> Dq    { return a * (~b) /(e_norm_sq(b)); }
operator / :: (a:Plane, b:Point) -> Dq    { return a * (~b) /(e_norm_sq(b)); }
operator / :: (a:Point, b:Plane) -> Dq    { return a * (~b) /(e_norm_sq(b)); }
operator / :: (a:Point, b:Point) -> Dq    { return a * (~b) /(e_norm_sq(b)); }
//dividing by fls, dqs, and lines has the "study number" problem, so we don't do it (for now?)

operator + :: (a:Dq, b:Dq) -> Dq {
    ret:Dq;
    ret[0] = a[0] + b[0]; ret[1] = a[1] + b[1]; ret[2] = a[2] + b[2]; ret[3] = a[3] + b[3]; ret[4] = a[4] + b[4]; ret[5] = a[5] + b[5]; ret[6] = a[6] + b[6]; ret[7] = a[7] + b[7];
    return ret;
}
operator + :: (a:Fl, b:Fl) -> Fl {
    ret:Fl;
    ret[0] = a[0] + b[0]; ret[1] = a[1] + b[1]; ret[2] = a[2] + b[2]; ret[3] = a[3] + b[3]; ret[4] = a[4] + b[4]; ret[5] = a[5] + b[5]; ret[6] = a[6] + b[6]; ret[7] = a[7] + b[7];
    return ret;
}
operator + :: (a:Line, b:Line) -> Line {
    ret:Line;
    ret[0] = a[0] + b[0]; ret[1] = a[1] + b[1]; ret[2] = a[2] + b[2]; ret[3] = a[3] + b[3]; ret[4] = a[4] + b[4]; ret[5] = a[5] + b[5];
    return ret;
}
operator + :: (a:Plane, b:Plane) -> Plane {
    ret:Plane;
    ret[0] = a[0] + b[0]; ret[1] = a[1] + b[1]; ret[2] = a[2] + b[2]; ret[3] = a[3] + b[3];
    return ret;
}
operator + :: (a:Point, b:Point) -> Point {
    ret:Point;
    ret[0] = a[0] + b[0]; ret[1] = a[1] + b[1]; ret[2] = a[2] + b[2]; ret[3] = a[3] + b[3];
    return ret;
}
operator + :: (a:Point, b:Plane) -> Fl #symmetric{
    ret:Fl;
    ret.point = a;
    ret.plane = b;
    return ret;
}
operator + :: (a:Dq, b:float) -> Dq #symmetric{
    ret := a;
    ret[0] += b;
    return ret;
}
operator + :: (a:Line, b:float) -> Dq #symmetric{
    ret:Dq;
    ret.line = a;
    ret[0] = b;
    return ret;
}
operator + :: (a:Line, b:Dq) -> Dq #symmetric{
    ret := b;
    ret.line = ret.line + a;
    return ret;
}

operator - :: (a:Dq, b:Dq) -> Dq {
    ret:Dq;
    ret[0] = a[0] - b[0]; ret[1] = a[1] - b[1]; ret[2] = a[2] - b[2]; ret[3] = a[3] - b[3]; ret[4] = a[4] - b[4]; ret[5] = a[5] - b[5]; ret[6] = a[6] - b[6]; ret[7] = a[7] - b[7];
    return ret;
}
operator - :: (a:float, b:Dq) -> Dq {
    ret := -b;
    ret[0] += a;
    return ret;
}
operator - :: (a:Dq, b:float) -> Dq {
    ret := a;
    ret[0] -= b;
    return ret;
}
operator - :: (a:float, b:Line) -> Dq {
    ret : Dq;
    ret.line = -b;
    ret[0] += a;
    return ret;
}
operator - :: (a:Fl, b:Fl) -> Fl {
    ret:Fl;
    ret[0] = a[0] - b[0]; ret[1] = a[1] - b[1]; ret[2] = a[2] - b[2]; ret[3] = a[3] - b[3]; ret[4] = a[4] - b[4]; ret[5] = a[5] - b[5]; ret[6] = a[6] - b[6]; ret[7] = a[7] - b[7];
    return ret;
}
operator - :: (a:Line, b:Line) -> Line {
    ret:Line;
    ret[0] = a[0] - b[0]; ret[1] = a[1] - b[1]; ret[2] = a[2] - b[2]; ret[3] = a[3] - b[3]; ret[4] = a[4] - b[4]; ret[5] = a[5] - b[5];
    return ret;
}
operator - :: (a:Plane, b:Plane) -> Plane {
    ret:Plane;
    ret[0] = a[0] - b[0]; ret[1] = a[1] - b[1]; ret[2] = a[2] - b[2]; ret[3] = a[3] - b[3];
    return ret;
}
operator - :: (a:Point, b:Point) -> Point {
    ret:Point;
    ret[0] = a[0] - b[0]; ret[1] = a[1] - b[1]; ret[2] = a[2] - b[2]; ret[3] = a[3] - b[3];
    return ret;
}

//negating
operator - :: (a:Dq) -> Dq {
    ret:Dq;
    ret[0] = -a[0];
    ret[1] = -a[1]; ret[2] = -a[2]; ret[3] = -a[3]; ret[4] = -a[4]; ret[5] = -a[5]; ret[6] = -a[6];
    ret[7] = -a[7];
    return ret;
}
operator - :: (a:Fl) -> Fl {
    ret:Fl;
    ret[0] = -a[0];
    ret[1] = -a[1]; ret[2] = -a[2]; ret[3] = -a[3]; ret[4] = -a[4]; ret[5] = -a[5]; ret[6] = -a[6];
    ret[7] = -a[7];
    return ret;
}
operator - :: (a:Point) -> Point {
    ret:Point;
    ret[0] = -a[0]; ret[1] = -a[1]; ret[2] = -a[2]; ret[3] = -a[3];
    return ret;
}
operator - :: (a:Plane) -> Plane {
    ret:Plane;
    ret[0] = -a[0]; ret[1] = -a[1]; ret[2] = -a[2]; ret[3] = -a[3];
    return ret;
}
operator - :: (a:Line) -> Line {
    ret:Line;
    ret[0] = -a[0]; ret[1] = -a[1]; ret[2] = -a[2]; ret[3] = -a[3]; ret[4] = -a[4]; ret[5] = -a[5];
    return ret;
}

operator * :: (a:Dq,    b:float) -> Dq    #symmetric { ret:Dq;    for i : 0..7 ret[i] = a[i] * b; return ret; }
operator * :: (a:Fl,    b:float) -> Fl    #symmetric { ret:Fl;    for i : 0..7 ret[i] = a[i] * b; return ret; }
operator * :: (a:Point, b:float) -> Point #symmetric { ret:Point; for i : 0..3 ret[i] = a[i] * b; return ret; }
operator * :: (a:Plane, b:float) -> Plane #symmetric { ret:Plane; for i : 0..3 ret[i] = a[i] * b; return ret; }
operator * :: (a:Line,  b:float) -> Line  #symmetric { ret:Line;  for i : 0..5 ret[i] = a[i] * b; return ret; }

operator & :: (a:Point, b:Dq    ) -> Fl    { return -b & a; }
operator & :: (a:Point, b:Line  ) -> Plane { return -b & a; }
operator & :: (a:Point, b:Plane ) -> float { return -b & a; }

//possibly should be % or just > but there are a few things one might reasonably expect > to be
operator >>> :: ( r:Dq, b:Dq ) -> Dq { return  r * b * ~r; }
operator >>> :: ( r:Dq, b:Fl ) -> Fl { return  r * b * ~r; }
operator >>> :: ( r:Fl, b:Dq ) -> Dq { return  r * b * ~r; }
operator >>> :: ( r:Fl, b:Fl ) -> Fl { return -r * b * ~r; }

operator >>> :: ( r:Point, b:Dq    ) -> Dq    { return cst(r) >>> b; }
operator >>> :: ( r:Plane, b:Dq    ) -> Dq    { return cst(r) >>> b; }
operator >>> :: ( r:Line,  b:Dq    ) -> Dq    { return cst(r) >>> b; }
operator >>> :: ( r:Point, b:Fl    ) -> Fl    { return cst(r) >>> b; }
operator >>> :: ( r:Plane, b:Fl    ) -> Fl    { return cst(r) >>> b; }
operator >>> :: ( r:Line,  b:Fl    ) -> Fl    { return cst(r) >>> b; }

operator >>> :: ( r:Point, b:Point ) -> Point { return (cst(r) >>> cst(b)).point; }
operator >>> :: ( r:Plane, b:Point ) -> Point { return (cst(r) >>> cst(b)).point; }
operator >>> :: ( r:Line,  b:Point ) -> Point { return (cst(r) >>> cst(b)).point; }
operator >>> :: ( r:Dq,    b:Point ) -> Point { return (    r  >>> cst(b)).point; } //worth optimizing!
operator >>> :: ( r:Fl,    b:Point ) -> Point { return (    r  >>> cst(b)).point; }

operator >>> :: ( r:Point, b:Plane ) -> Plane { return (cst(r) >>> cst(b)).plane; }
operator >>> :: ( r:Plane, b:Plane ) -> Plane { return (cst(r) >>> cst(b)).plane; }
operator >>> :: ( r:Line,  b:Plane ) -> Plane { return (cst(r) >>> cst(b)).plane; }
operator >>> :: ( r:Dq,    b:Plane ) -> Plane { return (    r  >>> cst(b)).plane; }
operator >>> :: ( r:Fl,    b:Plane ) -> Plane { return (    r  >>> cst(b)).plane; }

operator >>> :: ( r:Point, b:Line  ) -> Line  { return (cst(r) >>> cst(b)).line;  }
operator >>> :: ( r:Plane, b:Line  ) -> Line  { return (cst(r) >>> cst(b)).line;  }
operator >>> :: ( r:Line,  b:Line  ) -> Line  { return (cst(r) >>> cst(b)).line;  }
operator >>> :: ( r:Dq,    b:Line  ) -> Line  { return (    r  >>> cst(b)).line;  }
operator >>> :: ( r:Fl,    b:Line  ) -> Line  { return (    r  >>> cst(b)).line;  }

operator * :: ( r:Plane, b:Plane ) -> Dq { return cst(r) * cst(b); }
operator * :: ( r:Plane, b:Point ) -> Dq { return cst(r) * cst(b); }
operator * :: ( r:Plane, b:Line  ) -> Fl { return cst(r) * cst(b); }
operator * :: ( r:Plane, b:Dq    ) -> Fl { return cst(r) * b;      }
operator * :: ( r:Plane, b:Fl    ) -> Dq { return cst(r) * b;      }

operator * :: ( r:Point, b:Plane ) -> Dq { return cst(r) * cst(b); }
operator * :: ( r:Point, b:Point ) -> Dq { return cst(r) * cst(b); }
operator * :: ( r:Point, b:Line  ) -> Fl { return cst(r) * cst(b); }
operator * :: ( r:Point, b:Dq    ) -> Fl { return cst(r) * b;      }
operator * :: ( r:Point, b:Fl    ) -> Dq { return cst(r) * b;      }

operator * :: ( r:Line,  b:Plane ) -> Fl { return cst(r) * cst(b); }
operator * :: ( r:Line,  b:Point ) -> Fl { return cst(r) * cst(b); }
operator * :: ( r:Line,  b:Line  ) -> Dq { return cst(r) * cst(b); }
operator * :: ( r:Line,  b:Dq    ) -> Dq { return cst(r) * b;      }
operator * :: ( r:Line,  b:Fl    ) -> Fl { return cst(r) * b;      }

operator * :: ( r:Fl,    b:Plane ) -> Dq { return r * cst(b);      }
operator * :: ( r:Fl,    b:Point ) -> Dq { return r * cst(b);      }
operator * :: ( r:Fl,    b:Line  ) -> Fl { return r * cst(b);      }
operator * :: ( r:Dq,    b:Plane ) -> Fl { return r * cst(b);      }
operator * :: ( r:Dq,    b:Point ) -> Fl { return r * cst(b);      }
operator * :: ( r:Dq,    b:Line  ) -> Dq { return r * cst(b);      }

operator ^ :: ( a:Plane, b:Plane ) -> Line  { return (cst(a) ^ cst(b)).line;  }
operator ^ :: ( a:Plane, b:Line  ) -> Point { return (cst(a) ^ cst(b)).point; }
operator ^ :: ( a:Line,  b:Plane ) -> Point { return (cst(a) ^ cst(b)).point; }

operator | :: ( a:Plane, b:Point ) -> Line  { return (cst(a) | cst(b)).line;  }
operator | :: ( a:Point, b:Plane ) -> Line  { return (cst(a) | cst(b)).line;  }
operator | :: ( a:Plane, b:Line  ) -> Plane { return (cst(a) | cst(b)).plane; }
operator | :: ( a:Line,  b:Plane ) -> Plane { return (cst(a) | cst(b)).plane; }
operator | :: ( a:Line,  b:Point ) -> Plane { return (cst(a) | cst(b)).plane; }
operator | :: ( a:Point, b:Line  ) -> Plane { return (cst(a) | cst(b)).plane; }

operator | :: ( a:Plane, b:Plane ) -> float { return + b[1] * a[1] + b[2] * a[2] + b[3] * a[3];  }
operator | :: ( a:Point, b:Point ) -> float { return -a.e123 * b.e123; }
operator | :: ( a:Line,  b:Line  ) -> float { return - b[3] * a[3] - b[4] * a[4] - b[5] * a[5];  }
operator | :: ( a:float, b:float ) -> float { return a*b; }

operator | :: ( a:Dq,    b:Point ) -> Fl    { return (    a  | cst(b)); }
operator | :: ( a:Point, b:Dq    ) -> Fl    { return (cst(a) |     b ); }
operator | :: ( a:Fl,    b:Point ) -> Dq    { return (    a  | cst(b)); }
operator | :: ( a:Point, b:Fl    ) -> Dq    { return (cst(a) |     b ); }
operator | :: ( a:Dq,    b:Plane ) -> Fl    { return (    a  | cst(b)); }
operator | :: ( a:Plane, b:Dq    ) -> Fl    { return (cst(a) |     b ); }
operator | :: ( a:Fl,    b:Plane ) -> Dq    { return (    a  | cst(b)); }
operator | :: ( a:Plane, b:Fl    ) -> Dq    { return (cst(a) |     b ); }
operator | :: ( a:Dq,    b:Line  ) -> Dq    { return (    a  | cst(b)); }
operator | :: ( a:Line,  b:Dq    ) -> Dq    { return (cst(a) |     b ); }
operator | :: ( a:Fl,    b:Line  ) -> Fl    { return (    a  | cst(b)); }
operator | :: ( a:Line,  b:Fl    ) -> Fl    { return (cst(a) |     b ); }

#scope_file;
//The shame! The terrible shame of projective geometric algebra! Avert your eyes children!
dual :: (a:Dq   ) -> Dq    { return    Dq.{ elements = .[ a[7], a[6], a[5], a[4], a[3], a[2], a[1], a[0] ]}; }
dual :: (a:Fl   ) -> Fl    { return    Fl.{ elements = .[ a[7], a[6], a[5], a[4],-a[3],-a[2],-a[1],-a[0] ]}; }
dual :: (a:Line ) -> Line  { return  Line.{ elements = .[ a[5], a[4], a[3], a[2], a[1], a[0] ]}; }
dual :: (a:Plane) -> Point { return Point.{ w = -a[0], x = -a[1], y = -a[2], z = -a[3]}; }
dual :: (a:Point) -> Plane { return Plane.{ elements = .[ a.w, a.x, a.y, a.z]}; }

i_norm_sq :: (a:$T) -> float 
#modify{ return is_k_vector(T); }
{ return e_norm_sq(dual(a)); }
i_norm    :: (a:$T) -> float 
#modify{ return is_k_vector(T); }
{ return e_norm(dual(a)); }

#scope_export;

// e1, e2. 
angle_to :: (a:Plane, b:Plane) -> float {
    return atan2( e_norm( a ^ b ), a | b );
}
angle_to :: (a:Point, b:Point) -> float {
    if (a.e123 == 0. && b.e123 == 0.)
        return angle_to( dual(a), dual(b) );
    else
        return 0.; //meaningless really
}

origin_rotation_from_vector3s :: ( a:Vector3, b:Vector3 ) -> Dq {
    a_plane := Plane.{elements=.[0.,a[0],a[1],a[2]]};
    b_plane := Plane.{elements=.[0.,a[0],a[1],a[2]]};
    return sqrt( b_plane * a_plane );
}

select_grade :: (a:Dq, grade:int) -> Dq {
    if grade == {
        case 0;
            return Dq.{ e = a.e };
        case 2;
            return Dq.{ line = a.line };
        case 4;
            return Dq.{ e0123 = a.e0123 };
        case;
            return Dq.{};
    }
}
select_grade :: (a:Fl, grade:int) -> Fl {
    if grade == {
        case 1;
            return Fl.{ plane = a.plane };
        case 3;
            return Fl.{ point = a.point };
        case;
            return Fl.{};
    }
}

// usually this is not necessary, it is sufficient to use the reverse (much cheaper)
// a situation where this works while the reverse does not is worth publishing a paper on!
rigorous_inverse :: (d:Dq) -> Dq {

    d_d_reverse := d * ~d; //potentially study
    d_d_reverseInverse : Dq;
    d_d_reverseInverse[0] = 1. / d_d_reverse[0];
    d_d_reverseInverse[7] = -d_d_reverse[7] / (d_d_reverse[0] * d_d_reverse[0]);

    return (~d) * d_d_reverseInverse;
}

invariant_decomposition :: (d:Dq) -> Dq, Dq {

    normalized := normalize(d);

    trnsTarget := one_dq;
    rotnTarget := one_dq;

    if (abs(normalized[7]) < eps) {
        if (abs(normalized[4]) < eps && abs(normalized[5]) < eps && abs(normalized[6]) < eps) {
            //pure translation
            trnsTarget = normalized;
        }
        else {
            //pure rotation
            rotnTarget = normalized;
            rotnTarget[7] = 0.;
        }
    }
    else {
        numerator := select_grade( normalized, 4 );
        trnsTarget = numerator * rigorous_inverse( select_grade(normalized,2) );
        trnsTarget[0] += 1.;

        rotnTarget = normalized * ~trnsTarget;
    }

    return rotnTarget, trnsTarget;
}

approx_equals :: (a:$T, b:T) -> bool 
#modify{ return T == Dq || T == Fl; }
{
    for 0..7 {
        if abs(a[it] - b[it]) > eps {
            return false;
        }
    }
    return true;
}