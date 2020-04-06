open! Base

let column_letters = [|
    "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K";
    "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W";
|]

let hex_coord_to_sector_offsets (hcoord : HexCoord.t) : int * int =
    let col = HexCoord.q hcoord in
    let row = HexCoord.r hcoord + ((HexCoord.q hcoord + -1 * (HexCoord.q hcoord land 1)) / 2) in
    (col, row+1)
;;

let hex_coord_to_sector_location (hcoord : HexCoord.t) : string =
    let col = HexCoord.q hcoord in
    let row = HexCoord.r hcoord + ((HexCoord.q hcoord + -1 * (HexCoord.q hcoord land 1)) / 2) in
    let letter = column_letters.(col) in
    let row = Printf.sprintf "%02d" (row + 1) in
    letter ^ row
;;

let sector_location_to_hex_coord (str : string) =
    let letter = String.get str 0 |> Char.to_string in
    let row = Int.of_string String.(sub ~pos:1 ~len:2 str) in
    let col = 
        Option.value_exn
        (Array.findi column_letters ~f:(fun _ l -> String.(l = letter)))
        |> fst
    in
    let q = col in
    let r = row - (col - 1 * (col land 1) / 2) in
    let s = -q - r in
    HexCoord.make q r s
;;
