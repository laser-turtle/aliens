open! Base

type t = {
    map : Sector.t HexMap.t;
    alien_spawn : HexCoord.t;
    human_spawn : HexCoord.t;
    escape_hatches : HexCoord.t list;
}

let empty = {
    map = HexMap.empty;
    alien_spawn = HexCoord.make 0 0 0;
    human_spawn = HexCoord.make 0 0 0;
    escape_hatches = [];
}

let pp _fmt _t =
    ()

let is_escape_hatch (t : t) (coord : HexCoord.t) : bool =
    List.exists t.escape_hatches ~f:(fun c ->
        HexCoord.(coord = c)
    )
;;

let find_exn (t : t) =
    Map.find_exn t.map

let mem (t : t) = Map.mem t.map

let get_neighbors (t : t) c = 
    HexMap.get_neighbors t.map c

let random_map w h : t =
    let map =
        HexMap.create_grid w h ~f:(fun _ ->
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

    {
        map;
        alien_spawn = alienSpawn;
        human_spawn = humanSpawn;
        escape_hatches = [
            escape1;
            escape2;
            escape3;
            escape4;
        ];
    }
;;
