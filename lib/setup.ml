open! Base
open! Js_of_ocaml

let canvas_id = "map-canvas"
let canvas_parent = "canvas-parent"

let get_map_canvas () = Canvas.get_canvas canvas_id
let reset_canvas_size () = Canvas.reset_canvas_size canvas_parent canvas_id

let attach () =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->
        let canvas = get_map_canvas() in
        Canvas.fix_canvas_dpi canvas;
        Grid.draw canvas Game.board_size_w Game.board_size_h;
        Js._false
    );

    Dom_html.window##.onresize := Dom_html.handler (fun _ ->
        reset_canvas_size();
        let canvas = get_map_canvas() in
        Canvas.fix_canvas_dpi canvas;
        Grid.draw canvas Game.board_size_w Game.board_size_h;
        Js._false
    );
