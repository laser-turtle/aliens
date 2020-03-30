open! Base

let column_letters = [|
    "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K";
    "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W";
|]

let hex_coord_to_sector_location (hcoord : HexCoord.t) : string =
    let col = HexCoord.q hcoord in
    let row = HexCoord.r hcoord + ((HexCoord.q hcoord + -1 * (HexCoord.q hcoord land 1)) / 2) in
    let letter = column_letters.(col) in
    let row = Printf.sprintf "%02d" (row + 1) in
    letter ^ row
;;

