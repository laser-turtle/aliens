open! Base
open! Js_of_ocaml

let get_canvas id =
    id
    |> Dom_html.getElementById_exn 
    |> Dom_html.tagged
    |> function
        | Dom_html.Canvas c -> c
        | _ -> failwith "Expected canvas element"
;;

let reset_canvas_size parent canvas =
    parent
    |> Dom_html.getElementById_exn
    |> fun elt ->
        let w = elt##.clientWidth
        and h = elt##.clientHeight in
        canvas##.width := w;
        canvas##.height := h;
        canvas##.style##.width := Js.string "100%";
        canvas##.style##.height := Js.string "100%";
;;

type canvas = Dom_html.canvasElement Js.t
type context_2d = Dom_html.canvasRenderingContext2D Js.t

let resize canvas context dpi w h =
    canvas##.width := Float.(w * dpi |> to_int);
    canvas##.height := Float.(h * dpi |> to_int);
    let wi = Float.to_int w
    and hi = Float.to_int h in
    canvas##.style##.width := Js.string (Int.to_string wi ^ "px");
    canvas##.style##.height := Js.string (Int.to_string hi ^ "px");
    let scale = dpi in
    context##scale scale scale;
;;

let fix_canvas_dpi (canvas : canvas) =
    let context = canvas##getContext Dom_html._2d_ in
    let client_wf = canvas##.clientWidth |> Float.of_int in
    let client_hf = canvas##.clientHeight |> Float.of_int in
    resize canvas context Dom_html.window##.devicePixelRatio 
        client_wf client_hf;
;;
