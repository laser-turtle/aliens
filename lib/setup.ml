open! Base
open! Js_of_ocaml

let map_canvas = "map-canvas"
let gui_canvas = "gui-canvas"
let canvas_parent = "canvas-container"

let get_canvas = Canvas.get_canvas
let reset_canvas_size = Canvas.reset_canvas_size canvas_parent 

let game = 
    Game.new_game 4 (Game.generate_map())

let set_round_counter (game : Game.state) : unit =
    let round = Dom_html.getElementById_exn "round-text" in
    round##.innerHTML := Js.string (Printf.sprintf "Round %d/%d" game.round game.max_rounds);
;;

let set_player_turn (state : Game.state) : unit =
    set_round_counter state;
    let player_content = Dom_html.getElementById_exn "player-turns" in
    player_content##.innerHTML := Js.string "";
    Array.iteri state.players ~f:(fun idx player ->
        let span = Dom_html.createSpan Dom_html.document in
        span##.className := Js.string ("player " ^ if idx = state.current_player then "active" else "");
        span##.innerHTML := Js.string player.name;
        Dom.appendChild player_content span;
    )
;;

let set_sector_history (state : Game.state) : unit =
    let history = Dom_html.getElementById_exn "sector-history" in
    history##.innerHTML := Js.string "";
    let append_element idx sector =
        let s1 = Dom_html.createStrong Dom_html.document in
        let s2 = Dom_html.createStrong Dom_html.document in
        let text = Dom_html.document##createTextNode Js.(string sector) in
        let idx = Printf.sprintf "%02d[" idx in
        s1##.innerHTML := Js.string idx;
        s2##.innerHTML := Js.string "]";
        Dom.appendChild history s1;
        Dom.appendChild history text;
        Dom.appendChild history s2;
    in
    let player = Game.current_player state in
    List.iteri player.sector_history ~f:(fun idx coord ->
        let sector = GameUtil.hex_coord_to_sector_location coord in
        append_element idx sector
    )
;;

let set_event_list (state : Game.state) : unit =
    let content = Dom_html.getElementById_exn "event-list-content" in
    content##.innerHTML := Js.string "";
    List.iter state.events ~f:(fun event ->
        let event_str = Game.event_to_string state event in
        let text = Dom_html.document##createTextNode Js.(string event_str) in
        Dom.appendChild content text
    )
;;

let update_ui_for_state (state : Game.state) : unit =
    set_round_counter state;
    set_player_turn state;
    set_event_list state;
    set_sector_history state;
;;

let draw_map (game : Game.state) =
    let canvas = get_canvas map_canvas in
    let _gui = get_canvas gui_canvas in
    let info = Grid.draw canvas game.map.map Game.board_size_w Game.board_size_h in

    (* Update the canvas dimensions to be only as much as we need *)
   (* let x = info.origin.x +. info.grid_size.x +. info.hex_size in
    let y = info.origin.y +. info.grid_size.y in
    let dpi = Canvas.get_dpi () in
    Canvas.resize canvas info.context dpi x y;
    Canvas.resize gui Canvas.(context gui) dpi x y;
    *)

    info
;;

let draw_moves (info : Grid.context_info ref) (moves : HexMap.Set.t) : unit =
    let gui_canvas = get_canvas gui_canvas in
    let context = Canvas.context gui_canvas in
    let layout = !info.layout in
    context##save;
    context##.globalAlpha := 0.5;
    context##.fillStyle := Js.string "#FFFF99";
    HexMap.Set.iter moves ~f:(fun coord ->
        let poly = Layout.polygon_corners layout coord in
        Grid.polygon_path context poly;
        context##fill;
    );
    context##restore;
;;

let advance_game (info : Grid.context_info ref) (game : Game.state) : Game.state =
    let open Game in
    match game.next with
    | NextAction.CurrentPlayerPickMove moves ->
        draw_moves info moves; game
    | ConfirmSilenceInAllSectors -> game
    | ConfirmNoiseInYourSector -> game
    | ConfirmSafeSector -> game
    | DecideToAttack _coord -> game
    | PickNoiseInAnySector -> game
    | GameOver _ -> game
;;

let attach () =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->
        let map_canvas = get_canvas map_canvas in
        let gui_canvas = get_canvas gui_canvas in
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi gui_canvas;
    
        let info = ref (draw_map game) in

        update_ui_for_state game;
        advance_game info game |> ignore;

        (* Setup canvas listener *)
        (*
        gui_canvas##.onmousemove := Dom_html.handler (fun evt ->
            let clientRect = gui_canvas##getBoundingClientRect in
            let x = Caml.float evt##.clientX -. clientRect##.left in
            let y = Caml.float evt##.clientY -. clientRect##.top in

            (* TODO - selection is defintiely wrong in some way *)
            (*Printf.sprintf "%d %d" evt##.clientX evt##.clientY |> Caml.print_endline;*)

            let context = Canvas.context gui_canvas in

            let f = Layout.pixel_to_hex !info.layout Point.(make x y) in
            let h = FractHex.round f in
            context##clearRect 0. 0. 
                Caml.(float gui_canvas##.width) Caml.(float gui_canvas##.height);
            if HexMap.mem game.map.map h then (
                let poly = Layout.polygon_corners !info.layout h in
                Grid.polygon_path context poly;
                context##save;
                context##.globalAlpha := 0.5;
                context##.fillStyle := Js.string "#FFFF99";
                context##fill;
                context##restore;
            );

            Js._false
        );
        *)

        Dom_html.window##.onresize := Dom_html.handler (fun _ ->
            reset_canvas_size gui_canvas;
            reset_canvas_size map_canvas;
            Canvas.fix_canvas_dpi gui_canvas;
            Canvas.fix_canvas_dpi map_canvas;

            info := draw_map game;

            Js._false
        );

        Js._false
    );
