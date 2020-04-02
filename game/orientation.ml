open! Base

type t = {
    f0 : float;
    f1 : float;
    f2 : float;
    f3 : float;
    b0 : float;
    b1 : float;
    b2 : float;
    b3 : float;
    start_angle : float;
}

let make ~f0 ~f1 ~f2 ~f3 ~b0 ~b1 ~b2 ~b3 ~start_angle = {
    f0; f1; f2; f3; b0; b1; b2; b3; start_angle;
}
