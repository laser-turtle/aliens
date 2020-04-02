open! Base
open! Js_of_ocaml
open Ocaml_aliens_game

let float = Caml.float

type canvas = Dom_html.canvasElement Js.t
type context_2d = Dom_html.canvasRenderingContext2D Js.t

type context_info = {
    canvas : canvas;
    context : context_2d;
    layout : Layout.t;
    map_w : int;
    map_h : int;
    hex_size : float;
    client_size : Point.t;
    origin : Point.t;
    grid_size : Point.t;
}

let polygon_path (context : context_2d) (points : Point.t array) : unit =
    context##beginPath;
    (*
    context##moveTo (points.(0).x+.0.5) (points.(0).y+.0.5);
    for i=0 to Array.length points - 1 do
        context##lineTo (points.(i).x+.0.5) (points.(i).y+.0.5)
    done;
    *)
    let len = Array.length points - 1 in
    for i=0 to len do
        let pt = points.(i) in
        points.(i) <- Point.make Float.(round_down pt.x) Float.(round_down pt.y)
    done;

    context##moveTo points.(0).x points.(0).y;
    for i=0 to len do
        context##lineTo points.(i).x points.(i).y
    done;
    context##closePath
;;

module Colors = struct
    let background = "#FFFFFF"
    let hex_border = "#797B7C"
    let dangerous_fill = "#CCCCCC"
    let stripes = "#A19FA0"
    let black = "#100E0C"
    let sector_text = "#646362"
end

let deg_to_rad (degree : float) =
    degree *. Float.pi /. 180.

let draw_background (context : context_2d) (origin : Point.t) (hex_size : float) (grid_dim : Point.t) = 
    context##save;
    
    let ox = origin.x -. hex_size
    and oy = origin.y -. hex_size in
    
    let w = grid_dim.x +. 2.*.hex_size
    and h = grid_dim.y +. hex_size in

    context##translate ox oy;
    context##rect 0. 0. w h;
    context##clip;

    context##beginPath;

    context##.lineWidth := 0.5;
    context##.strokeStyle := Js.(string Colors.stripes);
    (* 4 * 23 = 92 lines ? *)
    (* 28 * 6 = 168 *)
    let hyp = Float.sqrt (w*.w +. h*.h) in
    let spacing = Float.round_down (hyp /. 108.) in
    context##translate (~-.w *. 0.38) (h *. 0.5);
    context##rotate (deg_to_rad ~-.45.);
    let x = ref 0. in
    while Float.(!x < hyp) do
        let xc = Float.round_down !x +. 0.5 in
        context##moveTo xc 0.;
        context##lineTo xc hyp;
        context##stroke;
        x := !x +. spacing;
    done;
    context##restore;
;;

module Offset = struct
    type t = {
        col : int;
        row : int;
    }

    let make col row = {
        col; row
    }
end

(* Make a DSL for drawing shapes on hexagons
 * maybe corner + offset list for line shapes 
 *)

let draw_hex_base (info : context_info) (hcoord : HexCoord.t) (bg_color : string) =
    let context = info.context in
    let layout = info.layout in
    context##.fillStyle := Js.(string bg_color);
    context##.strokeStyle := Js.(string Colors.hex_border);
    let poly = Layout.polygon_corners layout hcoord in
    polygon_path context poly;
    context##fill;
    polygon_path context poly;
    context##stroke;
;;

let draw_sector_coord (info : context_info) (hcoord : HexCoord.t) =
    (* Draw some text in the middle *)
    let layout = info.layout in
    let context = info.context in
    let size = info.hex_size in
    let font_size = size *. 0.65 in
    context##save;
    let center = Layout.hex_to_pixel layout hcoord in
    let text = Js.string (GameUtil.hex_coord_to_sector_location hcoord) in
    context##.font := Js.string ((font_size |> Float.to_string) ^ "px monospace");
    context##.fillStyle := Js.string Colors.sector_text;
    let text_width = (context##measureText text)##.width in
    let x = center.x -. text_width*.0.5 in
    let y = center.y +. font_size*.0.3 in
    context##fillText text x y;
    context##restore;
;;

let draw_safe_sector info hcoord =
    draw_hex_base info hcoord "white";
    draw_sector_coord info hcoord;
;;

let draw_dangerous_sector info hcoord =
    let context = info.context in
    let layout = info.layout in
    draw_hex_base info hcoord Colors.dangerous_fill;

    let center = Layout.hex_to_pixel layout hcoord |> Point.floor in
    let corners = Layout.polygon_corners layout hcoord in
    context##.fillStyle := Js.(string Colors.hex_border);

    let next i = corners.(Int.rem (i + 1) (Array.length corners)) in
    let prev i = 
        if i - 1 < 0 then Array.last corners
        else corners.(i - 1)
    in

    (* TODO - fix corner lengths *)
    let hscale = info.hex_size *. 0.33 in
    let hscale_center = info.hex_size *. 0.20 in
    let scale_to p v =
        Point.(scale (normalize p) v)
    in
    for i=0 to Array.length corners - 1 do
        let corner = corners.(i) |> Point.floor in
        let center_line = Point.(
            (corner + (scale_to (center - corner) hscale_center)) |> floor
        ) in

        let prev_corner = Point.((scale_to (prev i - corner) hscale) + corner |> floor) in
        let next_corner = Point.((scale_to (next i - corner) hscale)+ corner |> floor) in

        context##beginPath;
        context##moveTo corner.x corner.y;
        context##lineTo next_corner.x next_corner.y;
        context##lineTo center_line.x center_line.y;
        context##lineTo prev_corner.x prev_corner.y;
        context##closePath;
        context##fill;
    done;

    draw_sector_coord info hcoord;
;;

let draw_escape_hatch info hcoord n =
    draw_hex_base info hcoord Colors.black;

    let context = info.context in
    let layout = info.layout in
    context##save;
    let font_size = info.hex_size in
    let text = Int.to_string n |> Js.string in
    context##.font := Js.string Printf.(sprintf "bold %fpx monospace" font_size);
    context##.fillStyle := Js.string "white";
    let center = Layout.hex_to_pixel layout hcoord in
    let text_width = (context##measureText text)##.width in
    let x = center.x -. text_width*.0.5 in
    let y = center.y +. font_size*.0.3 in
    context##fillText text x y;

    (* Draw bars *)
    let line_width = info.hex_size *. 0.11 in
    let pull pt =
        let open Point in
        let amt = 0.30 in
        pt + (scale (center - pt) amt)
    in

    let c1 = Point.(Layout.hex_corner_offset layout 5 + center) |> pull in
    let c2 = Point.(Layout.hex_corner_offset layout 4 + center) |> pull in
    let c3 = Point.(Layout.hex_corner_offset layout 3 + center) |> pull in

    let c4 = Point.(Layout.hex_corner_offset layout 0 + center) |> pull in
    let c5 = Point.(Layout.hex_corner_offset layout 1 + center) |> pull in
    let c6 = Point.(Layout.hex_corner_offset layout 2 + center) |> pull in

    context##.strokeStyle := Js.string "white";
    context##.lineCap := Js.string "butt";
    context##.lineWidth := line_width;
    
    context##beginPath;
    context##moveTo (c1.x -. line_width*.0.5) (c1.y -. line_width*.0.5);
    context##lineTo (c2.x -. line_width*.0.5) (c2.y -. line_width*.0.5);
    context##lineTo (c3.x -. line_width*.0.5) (c3.y -. line_width*.0.5);
    context##stroke;

    context##beginPath;
    context##moveTo c4.x c4.y;
    context##lineTo c5.x c5.y;
    context##lineTo c6.x c6.y;
    context##stroke;

    context##restore;
;;

let draw_human_spawn info hcoord =
    draw_hex_base info hcoord Colors.black;

    let context = info.context in
    let layout = info.layout in

    let center = Layout.hex_to_pixel layout hcoord in
    let c4 = Point.(Layout.hex_corner_offset layout 4 + center) in
    let c5 = Point.(Layout.hex_corner_offset layout 5 + center) in
    let down = Point.make 0. 1. in

    let size = info.hex_size in
    let line_width = size *. 0.11 in
    let line_width_2 = line_width*.0.5 in
    let p1 = Point.(c5 + scale down (size *. 0.7)) in
    let p2 = Point.(c5 + scale down (size *. 1.2)) in
    let p3 = Point.(center + scale down (size *. 0.65)) in
    let p4 = Point.(c4 + scale down (size *. 1.2)) in
    let p5 = Point.(c4 + scale down (size *. 0.7)) in

    let p6 = Point.(c5 + scale down (size *. 0.4)) in
    let p7 = Point.(c4 + scale down (size *. 0.4)) in

    let p8 = Point.(make (center.x +. size*.0.30) (c5.y +. size*.0.3)) in
    let p9 = Point.(make (center.x -. size*.0.30) (c4.y +. size*.0.3)) in

    context##save;
    context##.strokeStyle := Js.string "white";
    context##.lineCap := Js.string "butt";
    context##.lineWidth := line_width; 
    context##beginPath;
    context##moveTo center.x center.y;
    context##lineTo (p1.x -. line_width_2) (p1.y -. line_width_2);
    context##lineTo (p6.x -. line_width_2) (p6.y -. line_width_2);
    context##lineTo (p8.x -. line_width_2) (p8.y -. line_width_2);
    context##moveTo (p6.x -. line_width_2) (p6.y -. line_width_2);
    context##lineTo (p2.x -. line_width_2) (p2.y -. line_width_2);
    context##lineTo p3.x (p3.y -. line_width_2);
    context##lineTo (p4.x (*+. line_width_2*)) (p4.y -. line_width_2);
    context##lineTo (p5.x (*+. line_width_2*)) (p5.y -. line_width_2);
    context##lineTo (p7.x (*+. line_width_2*)) (p7.y -. line_width_2);
    context##lineTo (p9.x (*+. line_width_2*)) (p9.y -. line_width_2);
    context##moveTo (p5.x (*+. line_width_2*)) (p5.y -. line_width_2);
    context##lineTo center.x center.y;
    context##stroke;
    context##restore;
;;

let draw_alien_spawn info hcoord =
    draw_hex_base info hcoord Colors.black;

    let context = info.context in
    let layout = info.layout in

    let center = Layout.hex_to_pixel layout hcoord in
    let c4 = Point.(Layout.hex_corner_offset layout 4 + center) in
    let c5 = Point.(Layout.hex_corner_offset layout 5 + center) in
    let down = Point.make 0. 1. in

    let size = info.hex_size in
    let line_width = size *. 0.11 in
    let line_width_2 = line_width*.0.5 in

    let p1 = Point.(c4 + scale down (size *. 0.25)) in
    let p2 = Point.(make (c5.x -. line_width_2) (center.y -. size*.0.10)) in
    let p3 = Point.(c5 + scale down (size *. 1.4)) in
    let p4 = Point.(make center.x (center.y +. size*.0.2)) in
    let p5 = Point.(c4 + scale down (size *. 1.4)) in
    let p6 = Point.(make (c4.x +. line_width_2) (center.y -. size*.0.10)) in
    let p7 = Point.(c5 + scale down (size *. 0.25)) in

    context##save;
    context##.strokeStyle := Js.string "white";
    context##.lineCap := Js.string "butt";
    context##.lineWidth := line_width; 
    context##beginPath;
    context##moveTo p1.x p1.y;
    context##lineTo p2.x p2.y;
    context##lineTo (p3.x -. line_width_2) p3.y;
    context##lineTo p4.x p4.y;
    context##lineTo (p5.x +. line_width_2) p5.y;
    context##lineTo p6.x p6.y;
    context##lineTo p7.x p7.y;
    context##stroke;
    context##restore;
;;

let draw_hex_item 
    (context : context_info)
    (hcoord : HexCoord.t) 
    (sector : Sector.t) 
    =
    match sector with
    | Dangerous -> draw_dangerous_sector context hcoord
    | Safe -> draw_safe_sector context hcoord
    | EscapeHatch n -> draw_escape_hatch context hcoord n
    | AlienSpawn -> draw_alien_spawn context hcoord
    | HumanSpawn -> draw_human_spawn context hcoord
;;

let calculate_grid_dimensions map_w map_h (canvas : canvas) =
    let client_w = canvas##.clientWidth |> Float.of_int in
    let client_h = canvas##.clientHeight |> Float.of_int in
    let map_wf = Float.of_int map_w in
    let map_hf = Float.of_int map_h in
    let s60 = 0.8660254037844386 in
    let total_w = 1.5 *. map_wf +. 0.5 in
    let total_h = s60 *. (2. *. (map_hf +. 1.) +. 1.) in
    let w_x = client_w /. total_w in
    let h_y = client_h /. total_h in
    let size = Float.min w_x h_y in
    let grid_h = size*.((2. *. map_hf)*.s60) in
    let grid_w = size*.(map_wf*.1.5-.1.5) in
    let yoffset = 2.5*.size in
    if Float.(w_x < h_y) then (
        (*let cy = (client_h -. grid_h) *. 0.5 in*)
        (w_x, yoffset, grid_w, grid_h, size)
    ) else (
        let cx = (client_w -. grid_w) *. 0.5 in
        (cx, yoffset, grid_w, grid_h, size)
    )
;;

let column_letters = GameUtil.column_letters

let draw_top_axis (info : context_info) =
    let context = info.context in
    context##save;
    assert (Array.length column_letters = info.map_w);
    let size = info.hex_size in
    let font_size = size *. 0.75 in
    context##.font := Js.string ((font_size |> Float.to_string) ^ "px monospace");
    let width = info.grid_size.x in
    let spacing = width /. float (Array.length column_letters - 1) in
    context##.fillStyle := Js.string Colors.sector_text;
    Array.iteri column_letters ~f:(fun idx letter ->
        let ti = context##measureText Js.(string letter) in
        let x = (float idx)*.spacing +. info.origin.x in
        let y = info.origin.y -. font_size -. size*.0.75 in
        context##fillText Js.(string letter) (x -. ti##.width*.0.5) y;
    );
    context##restore;
;;

let draw (canvas : canvas) (map : Sector.t HexMap.t) map_w map_h : context_info =
    let context = canvas##getContext Dom_html._2d_ in
    let client_wf = canvas##.clientWidth |> Float.of_int in
    let client_hf = canvas##.clientHeight |> Float.of_int in

    let origin_x, origin_y, grid_w, grid_h, size = 
        calculate_grid_dimensions map_w map_h canvas 
    in

    let origin = Point.make origin_x origin_y in
    let grid_size = Point.make grid_w grid_h in

    let layout = Layout.make Layout.flat Point.(make size size) origin in

    let context_info : context_info = {
        canvas;
        context;
        origin;
        map_w;
        map_h;
        hex_size = size;
        layout;
        client_size = Point.make client_wf client_hf;
        grid_size;
    } in

    draw_background context origin size grid_size;
    draw_top_axis context_info;

    context##.fillStyle := Js.(string "white");
    context##.strokeStyle := Js.(string Colors.hex_border);
    Map.iteri map ~f:(fun ~key ~data ->
        draw_hex_item context_info key data
    );
    context_info
;;
