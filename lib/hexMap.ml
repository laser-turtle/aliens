open! Base

(* TODO - keep track of important locations like 
 *        spawns, and escape hatches
 *)

type 'a t = (HexCoord.t, 'a, HexCoord.comparator_witness) Map.t

let mem = Map.mem

module Set = struct
    include Set

    type t = (HexCoord.t, HexCoord.comparator_witness) Set.t

    let empty = Set.empty (module HexCoord)
end

let empty : 'a t = Map.empty (module HexCoord)

let create_grid w h ~f =
    let map = ref empty in
    for s=0 to w-1 do
        let s_offset = Float.(round_down ((of_int s) / 2.)) |> Int.of_float in
        for r = -s_offset to (h - s_offset - 1) do
            let h = HexCoord.make s r (-r - s) in
            match f h with
            | Some data -> map := Map.set !map ~key:h ~data
            | None -> ()
        done
    done;
    !map
;;

let get_neighbors (t : 'a t) (h : HexCoord.t) : Set.t =
    let neighbors = HexCoord.neighbors h in 
    Array.fold neighbors ~init:Set.empty ~f:(fun set coord ->
        if Map.mem t coord then Set.add set h
        else set
    )
;;
