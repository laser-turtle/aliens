open! Base

module T = struct
    type t = 
        | Cube of {
            q : int;
            r : int;
            s : int;
        }
        | Axial of {
            q : int;
            r : int;
        }
        [@@deriving compare, sexp, show, yojson]

    let make q r s =
        Cube {q;r;s}

    let make_axis q r = 
        Axial {q;r}

    let q : t -> int = function
        | Cube {q; _} -> q
        | Axial {q; _} -> q

    let r : t -> int = function
        | Cube {r; _} -> r
        | Axial {r; _} -> r

    let s : t -> int = function
        | Cube {s; _} -> s
        | Axial {q; r} -> -q - r

    let qf (t : t) : float =
        q t |> Float.of_int

    let rf (t : t) : float =
        r t |> Float.of_int

    let sf (t : t) : float =
        s t |> Float.of_int

    let equal (t1 : t) (t2 : t) : bool =
        Int.(q t1 = q t2 &&
             r t1 = r t2 &&
             s t1 = s t2)

    let add (a : t) (b : t) : t =
        Cube {q = q a + q b;
              r = r a + r b;
              s = s a + s b; }

    let sub (a : t) (b : t) : t =
        Cube {q = q a - q b;
              r = r a - r b;
              s = s a - s b; }

    let mul (a : t) (k : int) : t =
        match a with
        | Cube {q;r;s} -> Cube {q=q*k; r=r*k; s=s*k}
        | Axial {q;r} -> Axial {q=q*k; r=r*k}

    let length (t : t) : int =
        (abs (q t) + abs (r t) + abs (s t)) / 2

    let distance (a : t) (b : t) : int =
        length (sub a b)

    type dir = 
        | BR
        | B
        | BL
        | TL
        | TR
        | T

    let direction_coord = function
        | BR -> make 1 0 ~-1
        | B -> make 1 ~-1 0
        | BL -> make 0 ~-1 1
        | TL -> make ~-1 0 1
        | TR -> make ~-1 1 0
        | T -> make 0 1 ~-1

    let neighbor (t : t) (d : dir) : t =
        add t (direction_coord d)

    let neighbors (t : t) : t array = [|
            neighbor t BR;
            neighbor t B;
            neighbor t BL;
            neighbor t TL;
            neighbor t TR;
            neighbor t T;
    |]

    let (+) = add
    let (-) = sub
    let ( * ) = mul
    let (=) = equal
    let (<>) a b = not (a = b)

    let to_string (t : t) =
        Printf.sprintf "<q:%d, r:%d, s%d>"
            (q t) (r t) (s t)
end

include T

include Comparator.Make(T)
