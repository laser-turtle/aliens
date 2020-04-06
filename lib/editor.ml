open! Base
open! Js_of_ocaml
open! JsUtil
open Ocaml_aliens_game

let gui_canvas = "gui-canvas"
let map_canvas = "map-canvas"
let map_background = "map-background-canvas"
let canvas_parent = "canvas-container"

let get_canvas = Canvas.get_canvas
let reset_canvas_size = Canvas.reset_canvas_size canvas_parent 

type tool =
    | Eraser
    | Sector of Sector.t

type editor_state = {
    mutable info : Grid.context_info;
    mutable map : Sector.t HexMap.t;
    mutable mouseDown : bool;
    mutable lastCoord : HexCoord.t option;
    mutable tool : tool;
}

let draw_map_background (info : Grid.context_info) =
    let canvas = Canvas.get_canvas map_background in
    let context = Canvas.context canvas in
    let info = { info with context = context } in
    Canvas.clear canvas;
    Grid.draw_background context info.origin info.hex_size info.grid_size;
    Grid.draw_top_axis info;
;;

let draw_map (state : editor_state) =
    let canvas = get_canvas map_canvas in
    let context = Canvas.context canvas in
    Canvas.clear canvas;
    let info = { state.info with context = context } in
    Grid.draw_sectors context info state.map;
;;

let setup_resize_handler (state : editor_state) =
    Dom_html.window##.onresize := Dom_html.handler (fun _ ->
        let map_canvas = Canvas.get_canvas map_canvas in
        let map_background = Canvas.get_canvas map_background in
        let gui_canvas = Canvas.get_canvas gui_canvas in
        reset_canvas_size map_canvas;
        reset_canvas_size map_background;
        reset_canvas_size gui_canvas;
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi map_background;
        Canvas.fix_canvas_dpi gui_canvas;

        state.info <- Grid.calculate_info map_canvas Game.board_size_w Game.board_size_h;

        draw_map_background state.info;
        draw_map state;

        Js._false
    );
;;

let update_state_from_click (state : editor_state) (coord : HexCoord.t) =
    begin match state.tool with
    | Eraser -> state.map <- Map.remove state.map coord
    | Sector Safe -> state.map <- Map.set state.map ~key:coord ~data:Sector.Safe
    | Sector Dangerous -> state.map <- Map.set state.map ~key:coord ~data:Sector.Dangerous
    | Sector HumanSpawn -> state.map <- Map.set state.map ~key:coord ~data:Sector.HumanSpawn
    | Sector AlienSpawn -> state.map <- Map.set state.map ~key:coord ~data:Sector.AlienSpawn
    | Sector EscapeHatch 1 -> 
        state.map <- Map.set state.map ~key:coord ~data:Sector.(EscapeHatch 1)
    | Sector EscapeHatch 2 -> 
        state.map <- Map.set state.map ~key:coord ~data:Sector.(EscapeHatch 2)
    | Sector EscapeHatch 3 -> 
        state.map <- Map.set state.map ~key:coord ~data:Sector.(EscapeHatch 3)
    | Sector EscapeHatch 4 -> 
        state.map <- Map.set state.map ~key:coord ~data:Sector.(EscapeHatch 4)
    | _ -> ()
    end;
;;

let draw_preview (context : Canvas.context_2d) (state : editor_state) (coord : HexCoord.t) =
    context##save;
    context##.globalAlpha := 0.75;
    let info = { state.info with context = context } in
    begin match state.tool with
    | Sector Safe -> Grid.draw_safe_sector info coord
    | Sector Dangerous -> Grid.draw_dangerous_sector info coord
    | Sector HumanSpawn -> Grid.draw_human_spawn info coord
    | Sector AlienSpawn -> Grid.draw_alien_spawn info coord
    | Sector (EscapeHatch n) -> Grid.draw_escape_hatch info coord n
    | Eraser -> 
        context##.fillStyle := Js.string "red";
        Grid.polygon_path context Layout.(polygon_corners state.info.layout coord);
        context##fill
    end;
    context##restore;
;;

let coord_in_bounds (coord : HexCoord.t) =
    let c, r = GameUtil.hex_coord_to_sector_offsets coord in
    r >= 1 && r <= 14 && c >= 0 && c <= 22
;;

let try_to_update_coord (state : editor_state) (coord : HexCoord.t) =
    match state.lastCoord with
    | None -> state.lastCoord <- Some coord; true
    | Some c when HexCoord.(c = coord) -> false
    | Some _ -> state.lastCoord <- Some coord; true
;;

let setup_ui (state : editor_state) =
    let hookup_input id tool =
        let input = get_input id in
        input##.onchange := Dom_html.handler (fun _ ->
            if input##.checked |> Js.to_bool then (
                state.tool <- tool;
            );
            Js._false;
        )
    in
    hookup_input "S-input" (Sector Sector.Safe);
    hookup_input "D-input" (Sector Sector.Dangerous);
    hookup_input "H-input" (Sector Sector.HumanSpawn);
    hookup_input "A-input" (Sector Sector.AlienSpawn);
    hookup_input "W-input" (Sector Sector.(EscapeHatch 1));
    hookup_input "X-input" (Sector Sector.(EscapeHatch 2));
    hookup_input "Y-input" (Sector Sector.(EscapeHatch 3));
    hookup_input "Z-input" (Sector Sector.(EscapeHatch 4));
    hookup_input "E-input" Eraser;

    let gui = get_canvas gui_canvas in
    let context = Canvas.context gui in

    let coord_from_mouse_evt evt =
        let clientRect = gui##getBoundingClientRect in
        let x = Caml.float evt##.clientX -. clientRect##.left in
        let y = Caml.float evt##.clientY -. clientRect##.top in
        Point.(make x y)
        |> Layout.pixel_to_hex state.info.layout
        |> FractHex.round 
    in

    gui##.onmousemove := Dom_html.handler (fun evt ->
        let coord = coord_from_mouse_evt evt in
        if coord_in_bounds coord then (
            let is_new_coord = try_to_update_coord state coord in
            if state.mouseDown && is_new_coord then (
                draw_map state;
                update_state_from_click state coord;
            );

            Canvas.clear gui;
            draw_preview context state coord;
        );
        Js._false;
    );
    gui##.onpointerleave := Dom_html.handler (fun _ ->
        Canvas.clear gui;
        Js._false;
    );
    gui##.onmousedown := Dom_html.handler (fun _ ->
        state.mouseDown <- true;
        Js._false;
    );
    gui##.onmouseup := Dom_html.handler (fun _ ->
        state.mouseDown <- false;
        Js._false;
    );
    gui##.onclick := Dom_html.handler (fun evt ->
        let coord = coord_from_mouse_evt evt in
        if coord_in_bounds coord then (
            update_state_from_click state coord;
            draw_map state;
        );
        Js._false;
    );
;;

let setup_buttons (state : editor_state) =
    let text_area = get_text_area "map-string" in
    let copy_btn = get_btn "copy-to-clipboard" in
    let read_btn = get_btn "read-map-string" in
    let gen_btn = get_btn "gen-map-string" in
    read_btn##.onclick := Dom_html.handler (fun _ ->
        (try
            let str = Js.to_string text_area##.value in
            Caml.print_endline str;
            state.map <- (SectorMap.from_map_string str).map;
            draw_map state;
        with Failure str ->
            Dom_html.window##alert Js.(string str);
        );
        Js._false;
    );
    gen_btn##.onclick := Dom_html.handler (fun _ ->
        let str = SectorMap.to_map_string state.map in
        text_area##.value := Js.string str;
        Js._false;
    );
    copy_btn##.onclick := Dom_html.handler (fun _ ->
        gen_btn##click;
        Js._false;
    )
;;

let attach () =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->

        let map_canvas = Canvas.get_canvas map_canvas in
        let map_back_canvas = Canvas.get_canvas map_background in
        let gui_canvas = Canvas.get_canvas gui_canvas in
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi map_back_canvas;
        Canvas.fix_canvas_dpi gui_canvas;

        let state = {
            info = Grid.calculate_info map_canvas Game.board_size_w Game.board_size_h;
            map = HexMap.empty;
            tool = Sector Sector.Safe;
            lastCoord = None;
            mouseDown = false;
        } in

        setup_buttons state;
        setup_resize_handler state;
        setup_ui state;
        draw_map_background state.info;

        Js._false
    )
