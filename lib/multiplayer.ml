open! Base
open! Js_of_ocaml
open Ocaml_aliens_game

module U = Js.Unsafe

(* We can be the host or not 
 *
 * Host has the canonical game state
 * and sends it back to each player
 * after a move is made.
 *
 * After host creates game, we get an id
 * and start waiting for connections.
 *)

let make_msg (type_ : string) (data : Yojson.Safe.t) =
    let json =
        data
        |> (fun data ->
            `Assoc ["type", `String type_; 
                    "data", data]
        )
        |> Yojson.Safe.to_string
        |> Js.string
    in
    Js._JSON##stringify (Js._JSON##parse json)
;;

let create_game_update (game : Game.state) (players : Game.Player.id list) =
    let json =
        Game.(state_to_yojson game)
        |> (fun data ->
            `Assoc ["type", `String "game-update"; 
                    "exclude", `List (List.map ~f:(fun i -> `Int i) players);
                    "data", data]
        )
        |> Yojson.Safe.to_string
        |> Js.string
    in
    Js._JSON##stringify (Js._JSON##parse json)
;;

let create_player_update (move : Game.Move.t) =
    make_msg "player-update" Game.Move.(to_yojson move)
;;

(* TODO - show some kind of error message *)
let parse_game_update json =
    Js._JSON##stringify json
    |> Js.to_string
    |> Yojson.Safe.from_string
    |> Game.state_of_yojson
    |> function
        | Ok game -> game
        | Error err -> failwith err
;;

let set_waiting (game : Game.state) =
    { game with next = WaitingForPlayer }
;;

let send_to_server_host json : unit =
    U.fun_call U.global##.send_server_host_ [|
        U.inject json
    |]
;;

let send_to_server_join json : unit =
    U.fun_call U.global##.send_server_join_ [|
        U.inject json
    |]
;;

module Host = struct
    type t = {
        mutable master_game : Game.state;
        host_game : Game.state ref;
        update_ui : Game.Event.t list -> Game.state -> unit; 
    }

    let create_waiting_state (game : Game.state) =
        match game.next with
        | Game.NextAction.GameOver _ -> game
        | _ -> { game with next = WaitingForPlayer }
    ;;

    let apply_move (state : t) (move : Game.Move.t) =
        match Game.apply state.master_game move with
        | Ok update -> 
                let diff = Game.event_diff ~new_:update ~old:state.master_game in
                state.master_game <- update;
                diff
        | Error _ -> failwith "GAME APPLY FAILURE"
    ;;

    let send_state_update (diff : Game.Event.t list) (state : t) : unit =
        let game = state.master_game in
        let waiting_state = create_waiting_state game in
        if game.current_player = 0 then (
            (* This is us *)
            state.update_ui diff game;
            state.host_game := game;
            send_to_server_host (create_game_update waiting_state [0]);
        ) else (
            (* Not us, so render waiting state *)
            let excluded = List.filter_map game.players ~f:(fun p ->
                if p.id = game.current_player then None
                else Some p.id
            ) in
            assert Poly.(game.next <> Game.NextAction.WaitingForPlayer);
            state.host_game := waiting_state;
            state.update_ui diff waiting_state;
            send_to_server_host (create_game_update waiting_state [0; game.current_player]);
            send_to_server_host (create_game_update game excluded);
        )
    ;;

    let do_move (state : t) (move : Game.Move.t) : unit =
        let diff = apply_move state move in
        send_state_update diff state;
    ;;

    let create (game : Game.state ref) (update_ui : Game.Event.t list -> Game.state -> unit) = 
        {
            master_game = !game;
            host_game = game;
            update_ui;
        }
    ;;
end
