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

let map_to_yojson (map : Sector.t HexMap.t) : Yojson.Safe.t =
    let items = Map.fold map ~init:[] ~f:(fun ~key ~data lst ->
        `List [HexCoord.to_yojson key; Sector.to_yojson data] :: lst
    ) in
    `List items
;;

let to_yojson (t : t) : Yojson.Safe.t =
    `Assoc [
        "map", map_to_yojson t.map;
        "alien_spawn", HexCoord.to_yojson t.alien_spawn;
        "human_spawn", HexCoord.to_yojson t.human_spawn;
        "escape_hatches", `List (List.map ~f:HexCoord.to_yojson t.escape_hatches);
    ]
;;

let force_ok = function
    | Ok o -> o
    | Error err -> failwith err

let map_of_yojson (json : Yojson.Safe.t) =
    let open Yojson.Safe.Util in
    match json with
    | `List lst -> 
        (try
            List.map lst ~f:(fun item ->
                match item with
                | `List [coord; sector] ->
                    let coord = HexCoord.of_yojson coord |> force_ok in
                    let sector = Sector.of_yojson sector |> force_ok in
                    (coord, sector)
                | _ -> failwith "expected two coords"
            )
            |> Map.of_alist (module HexCoord) 
            |> (function
                | `Ok map -> map
                | `Duplicate_key _ -> failwith "duplicate key"
            )
            |> fun map -> Ok map
        with _ ->
            Error "failed to convert map"
        )
    | _ -> Error "expected tuple"
;;

let of_yojson json =
    let open Yojson.Safe.Util in
    let open Result.Monad_infix in
    member "map" json |> map_of_yojson >>= fun map ->
    member "alien_spawn" json |> HexCoord.of_yojson >>= fun alien_spawn ->
    member "human_spawn" json |> HexCoord.of_yojson >>= fun human_spawn ->
    (try
        json
        |> member "escape_hatches"
        |> to_list
        |> List.map ~f:(fun h -> HexCoord.of_yojson h |> force_ok)
        |> fun lst -> Ok lst
    with _ ->
        Error "escape hatch failure"
    )
    >>= fun escape_hatches ->
    Ok {
        map;
        alien_spawn;
        human_spawn;
        escape_hatches;
    }
;;

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
