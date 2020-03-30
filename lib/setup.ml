open! Base
open! Js_of_ocaml

let map_canvas = "map-canvas"
let gui_canvas = "gui-canvas"
let canvas_parent = "canvas-container"

let get_canvas = Canvas.get_canvas
let reset_canvas_size = Canvas.reset_canvas_size canvas_parent 

let game = 
    Game.new_game 4 (Game.generate_map())

let set_round_counter (n : int) : unit =
    let round = Dom_html.getElementById_exn "round-text" in
    round##.innerHTML := Js.string ("Round " ^ Int.(to_string n));
;;

let set_player_turn (state : Game.state) : unit =
    set_round_counter state.round;
    let player_content = Dom_html.getElementById_exn "player-turns" in
    player_content##.innerHTML := Js.string "";
    Array.iteri state.players ~f:(fun idx _player ->
        let span = Dom_html.createSpan Dom_html.document in
        span##.className := Js.string ("player" ^ if idx = state.current_player then "active" else "");
        Dom.appendChild player_content span;
    )
;;

let draw_map (game : Game.state) =
    let canvas = get_canvas map_canvas in
    Grid.draw canvas game.map Game.board_size_w Game.board_size_h |> ignore
;;

let attach () =

    Dom_html.window##.onload := Dom_html.handler (fun _ ->
        let map_canvas = get_canvas map_canvas in
        let gui_canvas = get_canvas gui_canvas in
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi gui_canvas;
    
        let info = ref (Grid.draw map_canvas game.map Game.board_size_w Game.board_size_h) in

        (* Setup canvas listener *)
        gui_canvas##.onmousemove := Dom_html.handler (fun evt ->
            let clientRect = gui_canvas##getBoundingClientRect in
            let x = Caml.float evt##.clientX -. clientRect##.left in
            let y = Caml.float evt##.clientY -. clientRect##.top in

            let context = gui_canvas##getContext Dom_html._2d_ in

            let f = Layout.pixel_to_hex !info.layout Point.(make x y) in
            let h = FractHex.round f in
            context##clearRect 0. 0. 
                Caml.(float gui_canvas##.width) Caml.(float gui_canvas##.height);
            if HexMap.mem game.map h then (
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

        Dom_html.window##.onresize := Dom_html.handler (fun _ ->
            reset_canvas_size gui_canvas;
            reset_canvas_size map_canvas;
            Canvas.fix_canvas_dpi gui_canvas;
            Canvas.fix_canvas_dpi map_canvas;

            info := Grid.draw map_canvas game.map Game.board_size_w Game.board_size_h;

            Js._false
        );

        Js._false
    );

