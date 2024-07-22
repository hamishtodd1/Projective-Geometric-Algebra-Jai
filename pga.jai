/*
    Maybe rename Dual Quat to a short version of "Handness preserving transfomation"
    and Flector to "Handness reversing transfomation"

    have point and plane classes, or auto-cast them
*/

//Flector: point, plane, rotoreflection (reflection followed by translation), transflection(reflection followed by translation)
// Fl :: struct {
//     elements : [8]float;
// }


// Dq :: struct {
//     elements : [8]float;
// }
// operator *[] :: (b: *Dq, index: int) -> *float {
//     return *b.elements[index];
// }

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
    e021, e013, e032, e123 : float; //point part
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

operator *[] :: inline(b: *Plane, index: int) -> *float {
    return *b.elements[index];
}
operator *[] :: inline(b: *Point, index: int) -> *float {
    return *b.elements[index];
}
operator *[] :: inline(b: *Line, index: int) -> *float {
    return *b.elements[index];
}
operator *[] :: inline(b: *Dq, index: int) -> *float {
    return *b.elements[index];
}
operator *[] :: inline(b: *Fl, index: int) -> *float {
    return *b.elements[index];
}

cst :: (pl:Plane) -> Fl {
    ret:Fl;
    ret.e0 = pl.e0; ret.e1 = pl.e1; ret.e2 = pl.e2; ret.e3 = pl.e3;
    return ret;
}
cst :: (po:Point) -> Fl {
    ret:Fl;
    ret.e021 = po.e021; ret.e013 = po.e013; ret.e032 = po.e032; ret.e123 = po.e123;
    return ret;
}
cst :: (l : Line) -> Dq {
    ret:Dq;
    ret.e01 = l.e01; ret.e02 = l.e02; ret.e03 = l.e03;
    ret.e12 = l.e12; ret.e31 = l.e31; ret.e23 = l.e23;
    return ret;
}
cst :: (f:float) -> Dq {
    ret:Dq;
    ret.e = f;
    return ret;
}

// casting in the opposite direction is more like select_grade, you need to think about this

//fl functions
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

operator ~ :: (a: Dq) -> Dq {
    
    ret:Dq;
	ret[0] =  a[0];
    ret[1] = -a[1]; ret[2] = -a[2]; ret[3] = -a[3]; ret[4] = -a[4]; ret[5] = -a[5]; ret[6] = -a[6];
    ret[7] = a[7];

    return ret;
}
operator ~ :: (a: Fl) -> Fl {

    ret:Fl;
    ret[0] = a[0]; ret[1] = a[1]; ret[2] = a[2]; ret[3] = a[3];
    ret[4] = -a[4]; ret[5] = -a[5]; ret[6] = -a[6]; ret[7] = -a[7];

    return ret;
}

operator ~ :: (a: Line ) -> Line  { return -1.*a; }
operator ~ :: (a: Plane) -> Plane { return a; }
operator ~ :: (a: Point) -> Point { return -1.*a; }

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
operator - :: (a:Dq, b:Dq) -> Dq {
    ret:Dq;
    ret[0] = a[0] - b[0]; ret[1] = a[1] - b[1]; ret[2] = a[2] - b[2]; ret[3] = a[3] - b[3]; ret[4] = a[4] - b[4]; ret[5] = a[5] - b[5]; ret[6] = a[6] - b[6]; ret[7] = a[7] - b[7];
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

//possibly should be % or just >
operator >>> :: ( r:Dq, b:Dq ) -> Dq { return  r * (b * (~r)); }
operator >>> :: ( r:Dq, b:Fl ) -> Fl { return  r * (b * (~r)); }
operator >>> :: ( r:Fl, b:Dq ) -> Dq { return  r * (b * (~r)); }
operator >>> :: ( r:Fl, b:Fl ) -> Fl { return -r * (b * (~r)); }

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
operator * :: ( r:Point, b:Point ) -> Dq { return cst(r) * cst(b); }
operator * :: ( r:Line,  b:Line  ) -> Dq { return cst(r) * cst(b); }

operator * :: ( r:Plane, b:Fl    ) -> Dq #symmetric { return cst(r) * b;      }
operator * :: ( r:Plane, b:Point ) -> Dq #symmetric { return cst(r) * cst(b); }
operator * :: ( r:Plane, b:Dq    ) -> Fl #symmetric { return cst(r) * b;      }
operator * :: ( r:Plane, b:Line  ) -> Fl #symmetric { return cst(r) * cst(b); }
operator * :: ( r:Point, b:Fl    ) -> Dq #symmetric { return cst(r) * b;      }
operator * :: ( r:Point, b:Dq    ) -> Fl #symmetric { return cst(r) * b;      }
operator * :: ( r:Point, b:Line  ) -> Fl #symmetric { return cst(r) * cst(b); }
operator * :: ( r:Line,  b:Dq    ) -> Dq #symmetric { return cst(r) * b;      }
operator * :: ( r:Line,  b:Fl    ) -> Fl #symmetric { return cst(r) * b;      }

operator ^ :: ( a:Plane, b:Plane ) -> Line             { return (cst(a) ^ cst(b)).line;  }
operator ^ :: ( a:Plane, b:Line  ) -> Point #symmetric { return (cst(a) ^ cst(b)).point; }
operator | :: ( a:Plane, b:Point ) -> Line  #symmetric { return (cst(a) | cst(b)).line;  }
operator | :: ( a:Plane, b:Line  ) -> Plane #symmetric { return (cst(a) | cst(b)).plane; }
operator | :: ( a:Line,  b:Point ) -> Plane #symmetric { return (cst(a) | cst(b)).plane; }

operator ! :: (a:Dq   ) -> Dq    { return    Dq.{elements=.[a[7],a[6],a[5], a[4], a[3], a[2], a[1], a[0] ]}; }
operator ! :: (a:Fl   ) -> Fl    { return    Fl.{elements=.[a[7],a[6],a[5], a[4],-a[3],-a[2],-a[1],-a[0] ]}; }
operator ! :: (a:Line ) -> Line  { return  Line.{elements=.[a[5], a[4], a[3], a[2], a[1], a[0] ]}; }
operator ! :: (a:Plane) -> Point { return Point.{elements=.[-a[3],-a[2],-a[1],-a[0]]}; }
operator ! :: (a:Point) -> Plane { return Plane.{elements=.[ a[3], a[2], a[1], a[0]]}; }

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
operator & :: (a:Point, b:Dq) -> Fl { return -b & a; }

operator & :: (a:Line, b:Point) -> Plane {

    ret:Plane;
    ret[0] = + a[0] * b[2] + a[1] * b[1] + a[2] * b[0];
    ret[1] = - a[0] * b[3] + a[3] * b[1] - a[4] * b[0];
    ret[2] = - a[1] * b[3] - a[3] * b[2] + a[5] * b[0];
	ret[3] = - a[2] * b[3] + a[4] * b[2] - a[5] * b[1];

    return ret;
}
operator & :: (a:Point, b:Line) -> Plane { return -b & a; }

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
    return + a[0] * b[3] + a[1] * b[2] + a[2] * b[1] - a[3] * b[0];
}
operator & :: (a:Point, b:Plane) -> float { return -b & a; }