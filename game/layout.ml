open! Base

let sqrt_3 = Float.sqrt 3.

let pointy = Float.(Orientation.make
    ~f0:sqrt_3 ~f1:(sqrt_3 / 2.) ~f2:0. ~f3:(3. / 2.)
    ~b0:(sqrt_3 / 3.) ~b1:(-1. / 3.) ~b2:0. ~b3:(2. / 3.)
    ~start_angle:0.5
)

let flat = Float.(Orientation.make
    ~f0:(3.0 / 2.0) ~f1:0.0 ~f2:(sqrt_3 / 2.0) ~f3:sqrt_3
    ~b0:(2.0 / 3.0) ~b1:0.0 ~b2:(-1.0 / 3.0) ~b3:(sqrt_3 / 3.0)
    ~start_angle:0.
)

type t = {
    orientation : Orientation.t;
    size : Point.t;
    origin : Point.t;
}

let make orientation size origin = {
    orientation; size; origin;
}

let hex_to_pixel (t : t) (h : HexCoord.t) : Point.t =
    let module H = HexCoord in
    let open Float in
    let m = t.orientation in
    let x = (m.f0 * H.qf h + m.f1 * H.rf h) * t.size.x in
    let y = (m.f2 * H.qf h + m.f3 * H.rf h) * t.size.y in
    Point.make (x + t.origin.x) (y + t.origin.y)
;;

let pixel_to_hex (t : t) (p : Point.t) : FractHex.t =
    let module H = HexCoord in
    let open Float in
    let m = t.orientation in
    let x = (p.x - t.origin.x) / t.size.x in
    let y = (p.y - t.origin.y) / t.size.y in
    let q = m.b0 * x + m.b1 * y in
    let r = m.b2 * x + m.b3 * y in
    FractHex.make q r (-q - r)
;;

let hex_corner_offset (t : t) (corner : int) : Point.t =
    let open Float in
    let corner = Float.of_int corner in
    let angle = 2. * Float.pi * (t.orientation.start_angle + corner) / 6. in
    Point.make (t.size.x * cos angle) (t.size.y * sin angle)
;;

let polygon_corners (t : t) (h : HexCoord.t) : Point.t array =
    let center = hex_to_pixel t h in
    let result = [|
        Point.zero; Point.zero; Point.zero;
        Point.zero; Point.zero; Point.zero;
    |] in
    let open Float in
    for i=0 to 5 do
        let offset = hex_corner_offset t i in
        result.(i) <- Point.make (center.x + offset.x) (center.y + offset.y)
    done;
    result
;;
