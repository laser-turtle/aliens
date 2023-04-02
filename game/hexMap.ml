open! Base

(* TODO - keep track of important locations like 
 *        spawns, and escape hatches
 *)

type 'a t = (HexCoord.t, 'a, HexCoord.comparator_witness) Map.t

let to_yojson _t : Yojson.Safe.t =
    failwith "Unimplemented"

let of_yojson _json =
    failwith "Unimplemented"

let pp _fmt _t = 
    ()

let mem = Map.mem

module Set = struct
    type t = (HexCoord.t, HexCoord.comparator_witness) Set.t

    let pp _fmt _t =
        ()

    let empty = Set.empty (module HexCoord)
    let add = Set.add
    let length = Set.length
    let of_list = Set.of_list (module HexCoord)
    let fold = Set.fold
    let to_list = Set.to_list
    let union = Set.union
    let filter = Set.filter
    let remove = Set.remove
    let mem = Set.mem
    let iter = Set.iter

    let to_yojson (t : t) : Yojson.Safe.t =
        `List (Set.fold t ~init:[] ~f:(fun lst item ->
            HexCoord.to_yojson item :: lst
        ))

    let force_ok = function
        | Ok ok -> ok
        | Error err -> failwith err

    let of_yojson json =
        match json with
        | `List lst ->
            (try
                List.map lst ~f:(fun hex ->
                    HexCoord.of_yojson hex |> force_ok
                )
                |> of_list
                |> fun set -> Ok set
            with _ -> Error "failed to parse set")
        | _ -> Error "expected list for set" 
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
    let set = ref Set.empty in
    Array.iter neighbors ~f:(fun coord ->
        if Map.mem t coord then (
            set := Set.add !set coord
        )
    );
    !set
;;
