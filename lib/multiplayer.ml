open Ocaml_aliens_game

(* We can be the host or not 
 *
 * Host has the canonical game state
 * and sends it back to each player
 * after a move is made.
 *
 * After host creates game, we get an id
 * and start waiting for connections.
 *)

let step_host (_game : Game.state) =
    ()
;;
