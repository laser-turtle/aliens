open! Base

let lerp (a : float) (b : float) (t : float) =
    a *. (1. -. t) +. b *. t
;;

let hex_lerp (a : HexCoord.t) (b : HexCoord.t) (t : float) : FractHex.t =
    let af = FractHex.of_hex_coord a
    and bf = FractHex.of_hex_coord b in
    FractHex.make (lerp af.q bf.q t)
                  (lerp af.r bf.r t)
                  (lerp af.s bf.s t)
;;

let hex_line (a : HexCoord.t) (b : HexCoord.t) : HexCoord.t array =
    let n = HexCoord.distance a b in
    let nf = Float.of_int n in
    let step = 1. /. Float.max nf 1. in
    Array.init n ~f:(fun idx ->
        hex_lerp a b (step *. Float.of_int idx) |> FractHex.round
    )
;;
