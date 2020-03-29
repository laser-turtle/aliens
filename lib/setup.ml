open! Base
open! Js_of_ocaml

let get_map_canvas () =
    "map-canvas"
    |> Dom_html.getElementById_exn 
    |> Dom_html.tagged
    |> function
        | Dom_html.Canvas c -> c
        | _ -> failwith "Expected canvas element"
;;

let attach () =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->
        let canvas = get_map_canvas() in
        Grid.draw canvas;
        Js._false
    )
