open! Base

type t = {
    x : float;
    y : float;
}

let zero = {
    x = 0.; y = 0.;
}

let add (a : t) (b : t) = {
    x = a.x +. b.x;
    y = a.y +. b.y;
}

let sub (a : t ) (b : t) = {
    x = a.x -. b.x;
    y = a.y -. b.y;
}

let scale (a : t) (s : float) = {
    x = a.x *. s;
    y = a.y *. s;
}

let scale_flip = Fn.flip scale

let floor (a : t) = {
    x = Float.round_down a.x;
    y = Float.round_down a.y;
}

let dot (a : t) (b : t) =
    a.x *. b.x +. a.y *. b.y

let length (a : t) = 
    Float.sqrt (dot a a)

let normalize (a : t) = 
    let len = length a in
    { x = a.x /. len;
      y = a.y /. len; }

let (+) = add
let (-) = sub
let ( * ) = scale

let make x y = {
    x; y;
}
