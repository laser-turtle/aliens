open! Base

let board_size_w = 23
let board_size_h = 14
let max_rounds = 39

module Player = struct
    type team = Alien
              | Human
    [@@deriving show{with_path=false}, yojson]

    type id = int
    [@@deriving show{with_path=false}, yojson]

    type alive = Alive
               | Killed
               | Escaped
    [@@deriving show{with_path=false}, yojson]

    type t = {
        id : id;
        current_pos : HexCoord.t;
        sector_history : HexCoord.t list;
        team : team;
        alive : alive; (* Maybe more than bool *)
        name : string;
    }
    [@@deriving show{with_path=false}, yojson]

    let filter_alive (tlst : t list) (alive : alive) : t list =
        List.filter tlst ~f:(fun p -> Poly.equal p.alive alive)
    ;;
end

module EndState = struct
    type condition = AllHumansEscaped
                   | AllHumansKilled
                   | AllHumansEscapedOrKilled
                   | RoundLimit
    [@@deriving show{with_path=false}, yojson]

    type t = {
        humans_escaped : Player.t list;
        humans_killed : Player.t list;
        aliens_killed : Player.t list;
        condition : condition;
    }
    [@@deriving show{with_path=false}, yojson]
end

module NextAction = struct
    type t = CurrentPlayerPickMove of HexMap.Set.t
           | DecideToAttack of HexCoord.t
           | ConfirmNoiseInYourSector
           | PickNoiseInAnySector
           | ConfirmSafeSector
           | ConfirmSilenceInAllSectors
           | WaitingForPlayer (* Only used in multiplayer *)
           | GameOver of EndState.t
           [@@deriving show{with_path=false}, yojson]
end

module Move = struct
    type t = ChangeName of Player.id * string
           | PlayerMove of HexCoord.t
           | NoiseInAnySector of HexCoord.t
           | AcceptAttack
           | DeclineAttack
           | AcceptSafeSector
           | AcceptSilenceInAllSectors
           | AcceptNoiseInYourSector
           [@@deriving show{with_path=false}, yojson]

    type invalid = unit
end


module Event = struct
    type t = Noise of Player.id * HexCoord.t
           | Silence of Player.id
           | Attack of Player.id * HexCoord.t
           | Escape of Player.id * HexCoord.t * int (* pod *)
           | PlayerKilled of Player.id * HexCoord.t * Player.id (* who * where * by *)
           (* Win conditions ? *)
           [@@deriving show{with_path=false}, yojson]
end

type state = {
    players : Player.t list;
    round : int;
    max_rounds : int;
    current_player : int;
    next_players : Player.id list;
    map : SectorMap.t;
    next : NextAction.t;
    events : Event.t list;
}
[@@deriving show{with_path=false}, yojson]

let get_player (state : state) (pid : Player.id) =
    List.find_exn state.players ~f:(fun p ->
        p.id = pid
    )
;;

let player_name (state : state) (id : Player.id) : string =
    (get_player state id).name
;;

let player_team (state : state) (id : Player.id) : Player.team =
    (get_player state id).team
;;

let event_to_string (state : state) (event : Event.t) : string =
    let sector_loc = GameUtil.hex_coord_to_sector_location in
    match event with
    | Noise (id, coord) ->
        Printf.sprintf "%s: Noise in sector %s"
            (player_name state id)
            (sector_loc coord)
    | Silence id -> Printf.sprintf "%s: Silence in all sectors" (player_name state id)
    | Attack (id, coord) -> 
        Printf.sprintf "%s: Attacks sector %s" 
            (player_name state id)
            (sector_loc coord)
    | Escape (id, _, n) -> Printf.sprintf "%s: ESCAPED IN POD %d!" (player_name state id) n
    | PlayerKilled (who, where, by) ->
            Printf.sprintf "%s %s killed by %s %s in %s"
                (Player.show_team (player_team state who))
                (player_name state who)
                (sector_loc where)
                (Player.show_team (player_team state by))
                (player_name state by)
;;

let random_state = Base.Random.State.make_self_init()

let gen_players names alien_spawn human_spawn : Player.t list =
    let num_players = List.length names in
    let num_aliens =
        if Int.rem num_players 2 = 0 then
            num_players / 2
        else
            num_players / 2 + 1
    in
    let players = 
        num_players
        |> List.init ~f:(fun i ->
            if i < num_aliens then Player.Alien else Human
        ) 
        |> List.permute ~random_state 
        |> List.zip names 
        |> function
           | List.Or_unequal_lengths.Ok lst -> lst
           | _ -> failwith "Impossible case 1"
    in
    let players =
        List.mapi players ~f:(fun i (name, team) ->
            Player.{
                id = i;
                current_pos = 
                    (match team with
                    | Alien -> alien_spawn
                    | Human -> human_spawn);
                sector_history = [];
                team = team;
                alive = Alive;
                name;
            }
        )
        |> List.permute ~random_state
    in
    players
;;

let hcs = GameUtil.hex_coord_to_sector_location

let print_set set =
    set
    |> HexMap.Set.to_list 
    |> List.map ~f:GameUtil.hex_coord_to_sector_location
    |> String.concat ~sep:" "
    |> Printf.sprintf "{ %s }"
    |> Caml.print_endline
;;

let get_player_moves (map : SectorMap.t) (player : Player.t) : HexMap.Set.t =
    let set = 
        match player.team with
        | Human -> SectorMap.get_neighbors map player.current_pos
        | Alien ->
            (* Aliens can move 1 or 2 spaces *)
            let set = SectorMap.get_neighbors map player.current_pos in
            HexMap.Set.fold set ~init:set ~f:(fun set coord ->
                let new_set = SectorMap.get_neighbors map coord in
                HexMap.Set.union set new_set
            )
            (* Remove any escape hatches *)
            |> HexMap.Set.filter ~f:(fun coord ->
                not SectorMap.(is_escape_hatch map coord)
            )
    in
    let remove = Fn.flip HexMap.Set.remove in
    set
    |> remove player.current_pos
    |> remove map.alien_spawn
    |> remove map.human_spawn
;;

let generate_next_players (current_player : Player.id) (players : Player.t list) : Player.id list =
    (* Need to drop all players until (and including) current player *)
    let lst =
        List.drop_while players ~f:(fun p ->
            p.id <> current_player
        )
    in
    lst
    |> List.tl_exn
    |> List.map ~f:(fun p -> p.Player.id)
;;

let find_first_alive_player (players : Player.t list) : Player.id =
    let result =
        List.find_exn ~f:(fun p ->
            Poly.equal p.Player.alive Player.Alive
        ) players
    in
    result.id
;;

let next_player_action (map : SectorMap.t) (player : Player.t)  =
    NextAction.CurrentPlayerPickMove (get_player_moves map player)
;;

let empty_game = {
    players = [];
    round = 0;
    max_rounds = 39;
    current_player = 0;
    events = [];
    map = SectorMap.empty;
    next_players = [];
    next = NextAction.CurrentPlayerPickMove HexMap.Set.empty;
}

let new_game (players : string list) (map : SectorMap.t) = 
    let players = gen_players players map.alien_spawn map.human_spawn in
    let first_player = List.hd_exn players in
    {
    players;
    round = 0;
    max_rounds;
    map;
    current_player = first_player.id;
    next_players = generate_next_players first_player.id players;
    next = next_player_action map first_player;
    events = [];
}

let generate_map () =
    SectorMap.random_map board_size_w board_size_h

module DangerousAction = struct
    type t = NoiseInYourSector (* 10/25 *)
           | NoiseInAnySector (* 10/25 *)
           | Silence (* 5/25 *)

    let pick () =
        let r = Random.State.int random_state 25 in
        if r < 10 then NoiseInYourSector
        else if r < 20 then NoiseInAnySector
        else Silence
    ;;
end

let update_player_list (players : Player.t list) (pid : Player.id) ~f =
    List.map players ~f:(fun p -> 
        if p.id = pid then f p else p
    )
;;

let update_player (state : state) (pid : Player.id) ~f = { 
    state with players = update_player_list state.players pid ~f 
}

let change_name (state : state) (pid : Player.id) (name : string) : state =
    if pid >= 0 && pid < List.length state.players then (
        update_player state pid ~f:(fun (p : Player.t) -> {
            p with name
        })
    ) else (
        state
    )
;;

let update_next (state : state) (next : NextAction.t) : state = {
    state with next
}

type apply_result = (state, Move.invalid) Result.t

let pick_danger_action (state : state) : state =
    match DangerousAction.pick() with
    | NoiseInYourSector -> update_next state NextAction.ConfirmNoiseInYourSector
    | NoiseInAnySector -> update_next state PickNoiseInAnySector
    | Silence -> update_next state ConfirmSilenceInAllSectors
;;

let get_players_from_team (players : Player.t list) (team : Player.team) : Player.t list =
    List.filter players ~f:(fun p ->
        Poly.equal p.team team
    )
;;

let generate_end_stats (state : state) (type_ : EndState.condition) : EndState.t = 
    let humans = get_players_from_team state.players Player.Human in
    let aliens = get_players_from_team state.players Player.Alien in
    let filter = Player.filter_alive humans in
    {
      humans_escaped = filter Player.Escaped;
      humans_killed = filter Player.Killed;
      aliens_killed = Player.filter_alive aliens Player.Killed;
      condition = type_;
    }
;;

let check_for_end_game (state : state) : state =
    let game_over result =
        { state with next = NextAction.GameOver (
            generate_end_stats state result
        )}
    in

    if state.round = state.max_rounds then (
        game_over EndState.RoundLimit
    ) else (
        let humans = get_players_from_team state.players Player.Human in
        let len = List.length humans in

        let escaped = Player.filter_alive humans Player.Escaped in
        let killed = Player.filter_alive humans Player.Killed in

        let num_escaped = List.length escaped in
        let num_killed = List.length killed in

        if num_escaped = len then (
            game_over EndState.AllHumansEscaped
        ) else if num_killed = len then (
            game_over EndState.AllHumansKilled
        ) else if num_escaped + num_killed = len then (
            game_over EndState.AllHumansEscapedOrKilled
        ) else (
            state
        )
    )
;;

let current_player (state : state) : Player.t =
    get_player state state.current_player
;;

let change_player (state : state) =
    let next_state =
        match state.next_players with
        | [] -> 
            let first_player = List.hd_exn state.players in
            { state with 
                round = state.round + 1;
                current_player = first_player.id;
                next_players = generate_next_players first_player.id state.players;
                next = next_player_action state.map first_player;
            }
            (* TODO - check win conditions, huamns could all be dead? *)
        | hd :: tl -> { 
            state with 
                current_player = hd; next_players = tl;
                next = next_player_action state.map (get_player state hd);
        }
    in
    check_for_end_game next_state
;;

let check_player_move (state : state) (coord : HexCoord.t) : apply_result =
    let player = current_player state in
    let pid = player.id in
    let moves = get_player_moves state.map player in
    if HexMap.Set.mem moves coord then (
        let state = update_player state pid ~f:(fun p ->
            { p with
              current_pos = coord;
              sector_history = coord :: p.sector_history;
            }
        ) in
        let sector = SectorMap.find_exn state.map coord in
        let state = 
            match sector with
            | Safe -> update_next state NextAction.ConfirmSafeSector
            | Dangerous -> 
                begin match player.team with
                | Alien -> update_next state NextAction.(DecideToAttack coord)
                | Human -> pick_danger_action state
                end
            | AlienSpawn | HumanSpawn -> failwith "Shouldn't be able to move into a spawn point"
            | EscapeHatch n ->
                begin match player.team with
                | Human -> 
                    (* They win! *) 
                    { state with
                      events = Event.Escape (player.id, coord, n) :: state.events;
                      players = update_player_list state.players player.id ~f:(fun p -> {
                          p with alive = Escaped
                      })
                    }
                    |> change_player
                | Alien -> failwith "Aliens shouldn't be able to move into escape hatches"
                end
        in
        Ok state
    ) else (
        Error ()
    )
;;

let add_event (event : Event.t) (state : state) : state = {
    state with events = event :: state.events
}

let add_events (new_events : Event.t list) (state : state) : state = {
    state with events = List.append new_events state.events
}

let do_alien_attack (state : state) : apply_result =
    let attacker = current_player state in
    let players_in_sector =
        state.players
        |> List.filter ~f:(fun (p : Player.t) ->
                p.id <> attacker.id &&
                HexCoord.(p.current_pos = attacker.current_pos)
           )
    in
    (* Kill these players *)
    let state =
        List.fold players_in_sector ~init:state ~f:(fun state p -> 
            update_player state p.id ~f:(fun p ->
                { p with alive = Player.Killed }
            )
        )
    in
    let killed = List.map players_in_sector ~f:(fun p ->
        Event.PlayerKilled (p.id, p.current_pos, attacker.id)
    ) in
    let event = Event.Attack (attacker.id, attacker.current_pos) in
    Ok (state |> add_events (killed @ [event]) |> change_player)
;;

let do_safe_sector (state : state) : apply_result =
    Ok (change_player state)
;;

let do_accept (state : state) (event : Event.t) : apply_result =
    state
    |> add_event event
    |> change_player
    |> (fun s -> Ok s)
;;

let do_noise_in_any_sector (state : state) (coord : HexCoord.t) : apply_result =
    if SectorMap.mem state.map coord then (
        do_accept state (Event.Noise (state.current_player, coord))
    ) else (
        Error ()
    )
;;

let do_noise_in_your_sector (state : state) : apply_result =
    let p = current_player state in
    do_accept state (Event.Noise (p.id, p.current_pos))
;;

let do_silence_in_all_sectors (state : state) : apply_result =
    do_accept state (Event.Silence state.current_player)
;;

let verify_transition (_state : state) (_move : Move.t) : bool =
    true
;;

let apply (state : state) (move : Move.t) : apply_result =
    assert (verify_transition state move);
    match move with
    | ChangeName (pid, name) -> Ok (change_name state pid name)
    | PlayerMove coord -> check_player_move state coord
    | AcceptAttack -> do_alien_attack state
    | DeclineAttack -> Ok (pick_danger_action state)
    | NoiseInAnySector coord -> do_noise_in_any_sector state coord
    | AcceptNoiseInYourSector -> do_noise_in_your_sector state
    | AcceptSilenceInAllSectors -> do_silence_in_all_sectors state
    | AcceptSafeSector -> do_safe_sector state
;;
