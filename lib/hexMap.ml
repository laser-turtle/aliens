open! Base

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

let random_map w h : Sector.t t =
    let map =
        create_grid w h ~f:(fun _ ->
            if Random.int 100 < 75 then (
                let r = Random.int 100 in
                let sector = 
                    if r < 60 then Sector.Dangerous
                    else Sector.Safe
                in
                Some sector
            ) else None
        )
    in
    (* Pick 4 random spots to be escape hatches *)
    let count = Map.length map in
    let count_4 = count / 4 in
    let rnd_range min max = 
        Random.int (max - min) + min
    in
    let escape1, _ = Map.nth_exn map (rnd_range 0 count_4) in
    let escape2, _ = Map.nth_exn map (rnd_range count_4 (count_4*2)) in
    let escape3, _ = Map.nth_exn map (rnd_range (count_4*2) (count_4*3)) in
    let escape4, _ = Map.nth_exn map (rnd_range (count_4*3) count) in
    let alienSpawn, _ = Map.nth_exn map (rnd_range 0 (count/2)) in
    let humanSpawn, _ = Map.nth_exn map (rnd_range (count/2) count) in

    let map = Map.set map ~key:escape1 ~data:Sector.(EscapeHatch 1) in
    let map = Map.set map ~key:escape2 ~data:Sector.(EscapeHatch 2) in
    let map = Map.set map ~key:escape3 ~data:Sector.(EscapeHatch 3) in
    let map = Map.set map ~key:escape4 ~data:Sector.(EscapeHatch 4) in
    let map = Map.set map ~key:alienSpawn ~data:Sector.AlienSpawn  in
    let map = Map.set map ~key:humanSpawn ~data:Sector.HumanSpawn in

    map
;;
