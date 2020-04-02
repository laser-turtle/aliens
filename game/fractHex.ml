open! Base

type t = {
    q : float;
    r : float;
    s : float;
}

let make q r s = {
    q; r; s;
}

let of_hex_coord (h : HexCoord.t) : t = {
    q = HexCoord.qf h;
    r = HexCoord.rf h;
    s = HexCoord.sf h;
}

let round (t : t) : HexCoord.t =
    let q = Float.round t.q
    and r = Float.round t.r
    and s = Float.round t.s in
    let q_diff = Float.abs q -. t.q
    and r_diff = Float.abs r -. t.r
    and s_diff = Float.abs s -. t.s in
    let q = Float.to_int q
    and r = Float.to_int r
    and s = Float.to_int s in
    if Float.(q_diff > r_diff && q_diff > s_diff) then
        HexCoord.make (-r - s) r s
    else if Float.(r_diff > s_diff) then
        HexCoord.make q (-q - s) r
    else
        HexCoord.make q r (-q - r)
;;
