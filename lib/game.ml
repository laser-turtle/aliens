open! Base

let board_size_w = 23
let board_size_h = 14
let max_rounds = 39

module Player = struct
    type team = Alien
              | Human

    type id = int

    type t = {
        id : id;
        current_pos : HexCoord.t;
        sector_history : HexCoord.t list;
        team : team;
        alive : bool; (* Maybe more than bool *)
        name : string;
    }
end

module Action = struct
    type next = PlayerNeedsToMove of int
end

type state = {
    players : Player.t array;
    round : int;
    current_player : int;
    map : Sector.t HexMap.t;
    next : Action.next;
}

let random_state = Base.Random.State.make_self_init()

let gen_players num_players =
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
                current_pos = HexCoord.make 0 0 0; (*TODO*)
                sector_history = [];
                team = if is_alien then Alien else Human;
                alive = true;
                name = "";
            }
        )
    in
    Array.permute ~random_state players;
    players
;;

let new_game num_players map = {
    players = gen_players num_players;
    round = 0;
    map;
    current_player = 0;
    next = Action.PlayerNeedsToMove 0;
}

let get_player_moves (state : state) (n : int) : HexMap.Set.t =
    let player = state.players.(n) in
    match player.team with
    | Human -> HexMap.get_neighbors state.map player.current_pos
    | Alien ->
        (* Aliens can move 1 or 2 spaces *)
        let set = HexMap.get_neighbors state.map player.current_pos in
        let set = 
            HexMap.Set.fold set ~init:HexMap.Set.empty ~f:(fun set coord ->
                let new_set = HexMap.get_neighbors state.map coord in
                HexMap.Set.union set new_set
            )
        in
        HexMap.Set.remove set player.current_pos
;;

let generate_map () =
    HexMap.random_map board_size_w board_size_h

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

module Move = struct
    type t = unit

    type invalid = unit
end

let apply (state : state) (_move : Move.t) : (state, Move.invalid) Result.t =
    Result.Ok state
;;
