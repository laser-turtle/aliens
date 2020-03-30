open! Base

let board_size_w = 23
let board_size_h = 14
let max_rounds = 39

module Player = struct
    type team = Alien
              | Human

    type id = int

    type alive = Alive
               | Killed
               | Escaped

    type t = {
        id : id;
        current_pos : HexCoord.t;
        sector_history : HexCoord.t list;
        team : team;
        alive : alive; (* Maybe more than bool *)
        name : string;
    }

    let filter_alive (tlst : t list) (alive : alive) : t list =
        List.filter tlst ~f:(fun p -> Poly.equal p.alive alive)
    ;;
end

module EndState = struct
    type condition = AllHumansEscaped
                   | AllHumansKilled
                   | RoundLimit

    type t = {
        humans_escaped : Player.t list;
        humans_killed : Player.t list;
        condition : condition;
    }
end

module NextAction = struct
    type t = CurrentPlayerPickMove of HexMap.Set.t
           | DecideToAttack of HexCoord.t
           | ConfirmNoiseInYourSector
           | PickNoiseInAnySector
           | ConfirmSafeSector
           | ConfirmSilenceInAllSectors
           | GameOver of EndState.t
end

module Move = struct
    type t = ChangeName of Player.id * string
           | PlayerMove of Player.id * HexCoord.t
           | NoiseInAnySector of HexCoord.t
           | AcceptAttack
           | DeclineAttack
           | AcceptSafeSector
           | AcceptSilenceInAllSectors
           | AcceptNoiseInYourSector

    type invalid = unit
end


module Event = struct
    type t = Noise of Player.id * HexCoord.t
           | Silence of Player.id
           | Attack of Player.id * HexCoord.t
           | Escape of Player.id * HexCoord.t * int (* pod *)
           (* Win conditions ? *)
end

type state = {
    players : Player.t array;
    round : int;
    max_rounds : int;
    current_player : int;
    next_players : Player.id list;
    map : SectorMap.t;
    next : NextAction.t;
    events : Event.t list;
}

let random_state = Base.Random.State.make_self_init()

let gen_players num_players alien_spawn human_spawn =
    let num_aliens = 
        if Int.rem num_players 2 = 0 then
            num_players / 2
        else
            num_players / 2 + 1
    in
    let players =
        Array.init num_players ~f:(fun i ->
            let is_alien = i < num_aliens in
            Player.{
                id = i;
                current_pos = if is_alien then alien_spawn else human_spawn;
                sector_history = [];
                team = if is_alien then Alien else Human;
                alive = Alive;
                name = "";
            }
        )
    in
    Array.permute ~random_state players;
    players
;;

let get_player_moves (map : SectorMap.t) (player : Player.t) : HexMap.Set.t =
    let set = 
        match player.team with
        | Human -> SectorMap.get_neighbors map player.current_pos
        | Alien ->
            (* Aliens can move 1 or 2 spaces *)
            let set = SectorMap.get_neighbors map player.current_pos in
            HexMap.Set.fold set ~init:HexMap.Set.empty ~f:(fun set coord ->
                let new_set = SectorMap.get_neighbors map coord in
                HexMap.Set.union set new_set
            )
            (* Remove any escape hatches *)
            |> HexMap.Set.filter ~f:(fun coord ->
                SectorMap.is_escape_hatch map coord
            )
    in
    let remove = Fn.flip HexMap.Set.remove in
    set
    |> remove player.current_pos
    |> remove map.alien_spawn
    |> remove map.human_spawn
;;

let generate_next_players (current_player : int) (players : Player.t array) : Player.id list =
    players
    |> Array.to_list
    |> List.filter ~f:(fun p ->
        p.Player.id > current_player &&
        match p.Player.alive with
        | Alive -> true
        | Killed | Escaped -> false
    )
    |> List.map ~f:(fun p -> p.Player.id)
;;

let find_first_alive_player (players : Player.t array) : Player.id =
    let result =
        Array.find_exn ~f:(fun p ->
            Poly.equal p.Player.alive Player.Alive
        ) players
    in
    result.id
;;

let new_game (num_players : int) (map : SectorMap.t) = 
    let players = gen_players num_players map.alien_spawn map.human_spawn in
    {
    players;
    round = 0;
    max_rounds;
    map;
    current_player = 0;
    next_players = generate_next_players 0 players;
    next = NextAction.CurrentPlayerPickMove (get_player_moves map players.(0));
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

let update_player_list (players : Player.t array) (pid : Player.id) ~f =
    Array.mapi players ~f:(fun i p -> 
        if i = pid then f p else p
    )
;;

let update_player (state : state) (pid : Player.id) ~f = { 
    state with players = update_player_list state.players pid ~f 
}

let change_name (state : state) (pid : Player.id) (name : string) : state =
    if pid >= 0 && pid < Array.length state.players then (
        update_player state pid ~f:(fun p -> {
            p with name
        })
    ) else (
        state
    )
;;

let get_player (state : state) (pid : Player.id) =
    state.players.(pid)
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

let get_players_from_team (players : Player.t array) (team : Player.team) : Player.t list =
    Array.filter players ~f:(fun p ->
        Poly.equal p.team team
    ) |> Array.to_list
;;

let generate_end_stats (state : state) (type_ : EndState.condition) : EndState.t = 
    let humans = get_players_from_team state.players Player.Human in
    let filter = Player.filter_alive humans in
    {
      humans_escaped = filter Player.Escaped;
      humans_killed = filter Player.Killed;
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

        if List.length escaped = len then (
            game_over EndState.AllHumansEscaped
        ) else if List.length killed = len then (
            game_over EndState.AllHumansKilled
        ) else (
            state
        )
    )
;;

let change_player (state : state) =
    let current_player = find_first_alive_player state.players in
    let next_players = generate_next_players current_player state.players in
    let next_state =
        match state.next_players with
        | [] -> 
            { state with 
                round = state.round + 1;
                current_player;
                next_players;
            }
            (* TODO - check win conditions, huamns could all be dead? *)
        | _ :: tl -> { state with current_player; next_players = tl }
    in
    check_for_end_game next_state
;;

let check_player_move (state : state) (pid : Player.id) (coord : HexCoord.t) : apply_result =
    let player = get_player state pid in
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

let current_player (state : state) : Player.t =
    state.players.(state.current_player)
;;

let add_event (event : Event.t) (state : state) : state = {
    state with events = event :: state.events
}

let do_alien_attack (state : state) : apply_result =
    let attacker = current_player state in
    let players_in_sector =
        state.players
        |> Array.filter ~f:(fun (p : Player.t) ->
                p.id <> attacker.id &&
                HexCoord.(p.current_pos = attacker.current_pos)
           )
    in
    (* Kill these players *)
    let state =
        Array.fold players_in_sector ~init:state ~f:(fun state p -> 
            update_player state p.id ~f:(fun p ->
                { p with alive = Player.Killed }
            )
        )
    in
    let event = Event.Attack (attacker.id, attacker.current_pos) in
    Ok (state |> add_event event |> change_player)
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
    let p = state.players.(state.current_player) in
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
    | PlayerMove (pid, coord) -> check_player_move state pid coord
    | AcceptAttack -> do_alien_attack state
    | DeclineAttack -> Ok (pick_danger_action state)
    | NoiseInAnySector coord -> do_noise_in_any_sector state coord
    | AcceptNoiseInYourSector -> do_noise_in_your_sector state
    | AcceptSilenceInAllSectors -> do_silence_in_all_sectors state
    | AcceptSafeSector -> do_safe_sector state
;;
