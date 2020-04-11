open! Base
open! Js_of_ocaml
open JsUtil
open Ocaml_aliens_game

let map_canvas = "map-canvas"
let annot_canvas = "annot-canvas"
let gui_canvas = "gui-canvas"
let canvas_parent = "canvas-container"

module Annotations = struct

    type annotation = (Game.Player.id, Int.comparator_witness) Set.t

    type t = {
        mutable annots : annotation HexMap.t;
        mutable current_player : Game.Player.id option;
        mutable enabled : bool;
    }

    let add_player_to_coord (t : t) (coord : HexCoord.t) (pid : Game.Player.id) : unit =
        match Map.find t.annots coord with 
        | Some set ->
            let data = Set.add set pid in
            let map = Map.set t.annots ~key:coord ~data in
            t.annots <- map
        | None -> 
            t.annots <- Map.set t.annots ~key:coord ~data:Set.(singleton (module Int) pid)
    ;;

    let remove_player_from_coord (t : t) (coord : HexCoord.t) (pid : Game.Player.id) : unit =
        match Map.find t.annots coord with 
        | Some set ->
            let data = Set.remove set pid in
            let map = Map.set t.annots ~key:coord ~data in
            t.annots <- map
        | None -> ()
    ;;

    let toggle_player_at_coord (t : t) (coord : HexCoord.t) (pid : Game.Player.id) : unit =
        match Map.find t.annots coord with
        | Some set -> 
            if Set.mem set pid then (
                remove_player_from_coord t coord pid
            ) else (
                add_player_to_coord t coord pid
            )
        | None -> add_player_to_coord t coord pid
    ;;

    let toggle_player (t : t) (coord : HexCoord.t) : unit =
        match t.current_player with
        | Some pid -> toggle_player_at_coord t coord pid
        | None -> ()
    ;;

    let iter (t : t) ~f =
        Map.iteri t.annots ~f
    ;;

    let set_player (t : t) (pid : Game.Player.id) : unit =
        t.current_player <- Some pid
    ;;

    let unset_player (t : t) : unit =
        t.current_player <- None;
    ;;

    let enable (t : t) : unit =
        t.enabled <- true
    ;;

    let disable (t : t) : unit =
        t.enabled <- false;
        t.current_player <- None
    ;;

    let create () = {
        annots = HexMap.empty;
        current_player = None;
        enabled = false;
    }
end

let annot_state = Annotations.create()

let pid_to_color = function
    | 0 -> "#e75c3c"
    | 1 -> "#e67e22"
    | 2 -> "#f2ca27"
    | 3 -> "#6c7a89"
    | 4 -> "#9b59b6"
    | 5 -> "#3498db"
    | 6 -> "#2ecc71"
    | 7 -> "#744e2e"
    | _ -> failwith "No color for player 8+"
;;

let draw_annotations (info : Grid.context_info) (annotations : Annotations.t) =
    let annot = Canvas.get_canvas annot_canvas in
    Canvas.clear annot;
    if annotations.enabled then (
        let context = Canvas.context annot in
        context##save;
        Annotations.iter annotations ~f:(fun ~key ~data ->
            let center = Layout.hex_to_pixel info.layout key in
            Set.iter data ~f:(fun pid ->
                let pt =
                    match pid with
                    | 7 -> Point.(center - Point.make 0. (info.hex_size *. 0.6));
                    | 6 -> Point.(center + Point.make 0. (info.hex_size *. 0.6));
                    | 5 | 4 | 3 | 2 | 1 | 0 ->
                        let corner = Layout.hex_corner_offset info.layout pid in
                        Point.(center + (corner * 0.6))
                    | _ -> failwith "Unhandled coordinate"
                in
                context##beginPath;
                context##.fillStyle := Js.string (pid_to_color pid);
                context##arc pt.x pt.y (info.hex_size *. 0.2) 0. (2. *. Float.pi) Js._false;
                context##closePath;
                context##fill;
            )
        );
        context##restore;
    )
;;

let disable_all_annotate_player_toggles () =
    "annotate-player-toggle"
    |> get_elems_by_class 
    |> List.map ~f:cast_input
    |> List.iter ~f:(fun input ->
        input##.checked := Js._false
    )
;;

let enable_annotations_ui_selection (info : Grid.context_info ref) (is_valid_coord : HexCoord.t -> bool) =
    (* Make sure the show annotations toggle is enabled *)
    let toggle = get_input "annotate-toggle" in
    toggle##.checked := Js._true;
    Annotations.enable annot_state;
    draw_annotations !info annot_state;
    let canvas = Canvas.get_canvas annot_canvas in
    let context = Canvas.context canvas in
    canvas##.style##.zIndex := Js.string "999";

    (* Click handlers *)
    canvas##.onmousemove := Dom_html.handler (fun evt -> 
        (* Draw highlight *)
        draw_annotations !info annot_state;

        begin match annot_state.current_player with
        | None -> ()
        | Some pid ->
            let clientRect = canvas##getBoundingClientRect in
            let x = Caml.float evt##.clientX -. clientRect##.left in
            let y = Caml.float evt##.clientY -. clientRect##.top in
            let coord = Layout.pixel_to_hex !info.layout Point.(make x y) |> FractHex.round in
            if is_valid_coord coord then (
                context##save;
                context##.fillStyle := Js.string (pid_to_color pid);
                context##.globalAlpha := 0.2;
                Grid.polygon_path context Layout.(polygon_corners !info.layout coord);
                context##fill;
                context##restore;
            )
        end;

        Js._false;
    );

    canvas##.onclick := Dom_html.handler (fun evt ->
        let clientRect = canvas##getBoundingClientRect in
        let x = Caml.float evt##.clientX -. clientRect##.left in
        let y = Caml.float evt##.clientY -. clientRect##.top in

        let coord = Layout.pixel_to_hex !info.layout Point.(make x y) |> FractHex.round in
        if is_valid_coord coord then (
            Annotations.toggle_player annot_state coord;
            draw_annotations !info annot_state;
        );

        Js._false;
    );
;;

let disable_annotations_ui_selection info =
    let canvas = Canvas.get_canvas annot_canvas in
    canvas##.style##.zIndex := Js.string "1";
    canvas##.onmousemove := Dom_html.no_handler;
    canvas##.onclick := Dom_html.no_handler;
    draw_annotations info annot_state
;;

let pid_to_direction = function
    | 0 -> "MR"
    | 1 -> "BR"
    | 2 -> "BL"
    | 3 -> "ML"
    | 4 -> "TL"
    | 5 -> "TR"
    | 6 -> "CT"
    | 7 -> "CB"
    | _ -> failwith "Unexpected player id"
;;

let setup_annotation_ui (info : Grid.context_info ref) (_my_id : Game.Player.id) (state : Game.state ref) =
    let players = !state.players in
    let container = get_elem_id "annotate-player-container" in
    setText container "";
    let controls = List.map players ~f:(fun p ->
        let name = Printf.sprintf "player-annotate-input-%d" p.id |> Js.string in
        let input = Dom_html.createInput ~_type:(Js.string "checkbox") ~name Dom_html.document in
        let label = Dom_html.createLabel Dom_html.document in
        let div = Dom_html.createDiv Dom_html.document in
        let color_box = Dom_html.createDiv Dom_html.document in
        label##.htmlFor := name;
        setText label (p.name ^ " (" ^ pid_to_direction p.id ^ ")");
        input##.className := Js.string "annotate-player-toggle switch is-dark";
        label##.className := Js.string "annotate-player-label";
        color_box##.style##.width := Js.string "1rem";
        color_box##.style##.height := Js.string "1rem";
        color_box##.style##.display := Js.string "inline-block";
        color_box##.style##.borderRadius := Js.string "0.5rem";
        color_box##.style##.backgroundColor := Js.string (pid_to_color p.id);
        color_box##.style##.position := Js.string "relative";
        color_box##.style##.marginLeft := Js.string "0.25rem";
        color_box##.style##.top := Js.string "0.15rem";
        div##.style##.display := Js.string "block";

        Dom.appendChild div input;
        Dom.appendChild div label;
        Dom.appendChild div color_box;

        input, label, p.id, div
    ) in
    let disable_other_controls me = 
        let my_name = me##.name |> Js.to_string in
        List.iter controls ~f:(fun (input, _, _, _) ->
            let iname = input##.name |> Js.to_string in
            if String.(my_name <> iname) then (
                input##.checked := Js._false;
            )
        )
    in
    let is_valid_coord coord =
        not (SectorMap.is_sector_empty !state.map coord)
    in
    List.iter controls ~f:(fun (input, label, pid, div) ->
        label##.onclick := Dom_html.handler (fun _ ->
            toggle_input input;
            if input##.checked |> Js.to_bool then (
                disable_other_controls input;
                (* Enable annotations and set our ID *)
                Annotations.set_player annot_state pid;
                enable_annotations_ui_selection info is_valid_coord;
            ) else (
                Annotations.unset_player annot_state;
                disable_annotations_ui_selection !info;
            );
            Js._false
        );

        Dom.appendChild container div;
    );
;;

let svg_noise_in_your_sector = "noise_in_your_sector.svg"
let svg_noise_in_any_sector = "noise_any_sector.svg"
let svg_silence_all_sectors = "silence_all_sectors.svg"
let svg_escape_pod_damaged = "escape_pod_damaged.svg"
let svg_escape_pod_undamaged = "escape_pod_undamaged.svg"

let get_canvas = Canvas.get_canvas
let reset_canvas_size = Canvas.reset_canvas_size canvas_parent 

let set_round_counter (game : Game.state) : unit =
    let round = Dom_html.getElementById_exn "round-text" in
    setText round (Printf.sprintf "Round %d/%d" game.round game.max_rounds);
;;

let set_player_turn (state : Game.state) (my_id : Game.Player.id) : unit =
    set_round_counter state;
    let player_content = get_elem_id "player-turns" in
    let player_type = get_elem_id "player-type" in
    let title = get_elem_id "player-turn-title" in
    let title_name =
        if my_id = state.current_player then (
            "Your Turn"
        ) else (
            let name = Game.player_name state state.current_player in
            if String.length name > 11 then (
                Printf.sprintf "%s…'s Turn" String.(sub ~pos:0 ~len:8 name)
            ) else (
                Printf.sprintf "%s's Turn" name
            )

        )
    in
    setText title title_name;
    let player_string =
        match (Game.get_player state my_id).team with
        | Alien -> "Alien"
        | Human -> "Human"
    in
    setText player_type player_string;
    setText player_content "";
    List.iteri state.players ~f:(fun _ player ->
        let span = Dom_html.createSpan Dom_html.document in
        let pid = player.id in
        let className =
            if Poly.(player.Game.Player.alive <> Game.Player.Alive) then (
                "player-dead"
            ) else if pid = state.current_player && pid = my_id then (
                "your-turn"
            ) else if pid = my_id then (
                "you-but-not-active"
            ) else if pid = state.current_player then (
                "active"
            ) else ""
        in
        span##.className := Js.string ("player " ^ className);
        setText span player.name;
        Dom.appendChild player_content span;
    )
;;

let set_sector_history (my_id : Game.Player.id)  (state : Game.state) : unit =
    let history = Dom_html.getElementById_exn "sector-history" in
    setText history "";
    let append_element idx sector =
        let div = Dom_html.createDiv Dom_html.document in
        let s1 = Dom_html.createStrong Dom_html.document in
        let s2 = Dom_html.createStrong Dom_html.document in
        let text = Dom_html.document##createTextNode Js.(string sector) in
        let idx = Printf.sprintf "%02d[" (idx + 1) in
        div##.className := Js.string "sector-item";
        setText s1 idx;
        setText s2 "]";
        Dom.appendChild div s1;
        Dom.appendChild div text;
        Dom.appendChild div s2;
        Dom.appendChild history div;
    in
    let player = Game.get_player state my_id in
    List.iteri List.(rev player.sector_history) ~f:(fun idx coord ->
        let sector = GameUtil.hex_coord_to_sector_location coord in
        append_element idx sector
    )
;;

let set_event_list (state : Game.state) : unit =
    let content = Dom_html.getElementById_exn "event-list-content" in
    setText content "";
    List.iter state.events ~f:(fun event ->
        let event_str = Game.event_to_string state event in
        let p = Dom_html.createP Dom_html.document in
        setText p event_str;
        Dom.appendChild content p
    )
;;

let set_gui_handlers layout ~move_handler ~click_handler =
    let gui = get_canvas gui_canvas in
    gui##.onmousemove := Dom_html.handler (fun evt ->
        let clientRect = gui##getBoundingClientRect in
        let x = Caml.float evt##.clientX -. clientRect##.left in
        let y = Caml.float evt##.clientY -. clientRect##.top in
        move_handler gui layout x y;
        Js._false
    );
    gui##.onclick := Dom_html.handler (fun evt ->
        let clientRect = gui##getBoundingClientRect in
        let x = Caml.float evt##.clientX -. clientRect##.left in
        let y = Caml.float evt##.clientY -. clientRect##.top in
        click_handler layout x y;
        Js._false
    );
;;

let clear_gui_handlers () =
    let gui = get_canvas gui_canvas in
    gui##.onmousemove := Dom_html.no_handler;
    gui##.onclick := Dom_html.no_handler;
;;

let clear_gui () =
    let gui = get_canvas gui_canvas in
    Canvas.clear gui;
;;

let add_gui_pointer gui =
    gui##.className := Js.string "subcanvas pointer"

let remove_gui_pointer gui =
    gui##.className := Js.string "subcanvas"

let draw_player (info : Grid.context_info) (state : Game.state) (my_id : Game.Player.id) =
    let module Player = Game.Player in
    let player = Game.get_player state my_id in
    let coord = player.current_pos in
    let pt = Layout.hex_to_pixel info.layout coord in
    let gui = Canvas.get_canvas gui_canvas in
    let context = Canvas.context gui in
    let size = info.hex_size in
    context##save;
    context##.fillStyle := Js.string "#111111";
    context##.strokeStyle := Js.string "white";
    (*"#FFDB4A";*)
    context##.lineWidth := size *. 0.1;
    context##beginPath;
    context##moveTo (pt.x -. size*.0.5) (pt.y +. size*.0.25);
    context##lineTo (pt.x) (pt.y -. size*.0.5);
    context##lineTo (pt.x +. size*.0.5) (pt.y +. size*.0.25);
    context##closePath;
    context##fill;
    context##stroke;
    context##restore;
;;

let draw_moves (layout : Layout.t) (moves : HexMap.Set.t) : unit =
    let gui_canvas = get_canvas gui_canvas in
    let context = Canvas.context gui_canvas in
    context##save;
    context##clearRect 0. 0. (Caml.float gui_canvas##.width) (Caml.float gui_canvas##.height);
    context##.globalAlpha := 0.5;
    context##.fillStyle := Js.string "#FFFF99";
    HexMap.Set.iter moves ~f:(fun coord ->
        let poly = Layout.polygon_corners layout coord in
        Grid.polygon_path context poly;
        context##fill;
    );
    context##restore;
;;

let hide_attack_container () =
    let div = Dom_html.getElementById_exn "attack-choices" in
    div##.style##.display := Js.string "none";
;;

let pick_move_handler moves gui layout x y =
    let context = Canvas.context gui in
    let coord = 
        Point.(make x y)
        |> Layout.pixel_to_hex layout
        |> FractHex.round 
    in
    clear_gui();
    draw_moves layout moves; 
    if HexMap.Set.mem moves coord then (
        add_gui_pointer gui;
        context##save;
        context##.globalAlpha := 0.5;
        context##.fillStyle := Js.string "#FFDB4A";
        let poly = Layout.polygon_corners layout coord in
        Grid.polygon_path context poly;
        context##fill;
        context##restore;
    ) else (
        remove_gui_pointer gui;
    )
;;

let set_click apply_move id fn = 
    let btn = get_btn id  in
    btn##.onclick := Dom_html.handler (fun _ ->
        fn apply_move;
        Js._false;
    )
;;

let do_decide_to_attack (info : Grid.context_info) (coord : HexCoord.t) apply_move =
    set_click apply_move "attack" (fun _ ->
        apply_move Game.Move.AcceptAttack;
        hide_attack_container();
    );

    set_click apply_move "dont-attack" (fun _ ->
        apply_move Game.Move.DeclineAttack;
        hide_attack_container();
    );

    let layout = info.layout in
    let pt = Layout.hex_to_pixel layout coord in
    let gui = Canvas.get_canvas gui_canvas in
    let div = Dom_html.getElementById_exn "attack-choices" in
    remove_gui_pointer gui;
    div##.style##.display := Js.string "block";
    let bounds = div##getBoundingClientRect in
    let x = pt.x -. (bounds##.right -. bounds##.left)*.0.5 in
    let y = pt.y -. (bounds##.bottom -. bounds##.top)*.0.5 |> Float.to_string in
    let x = if Float.(x < 0.) then "0" else Float.to_string x in
    div##.style##.left := Js.string (x ^ "px");
    div##.style##.top := Js.string (y ^ "px");
;;

let do_game_over (players : Game.Player.t list) (end_state : Game.EndState.t) =
    enable_modal "game-over-modal";
    let game_result = get_elem_id "game-result" in
    let survived = get_elem_id "survived" in
    let escaped = get_elem_id "escaped" in
    let killed = get_elem_id "killed" in

    let win_string =
        match end_state.condition with 
        | AllHumansEscaped -> "Humans Win!"
        | AllHumansKilled -> "Aliens Win!"
        | AllHumansEscapedOrKilled -> "Some humans escaped!"
        | RoundLimit -> "Round Limit Reached"
        | NoEscapePodsLeft -> "No more escape pods!"
    in
    setText game_result win_string;

    let open Game in
    let set_control_html control state =
        let players = List.filter players ~f:(fun p -> Poly.equal p.alive state) in
        match players with
        | [] -> ()
        | lst ->
            let str = List.fold ~init:"" lst ~f:(fun str p ->
                if Poly.(p.Player.alive = state) then (
                    (p.name ^ " (" ^ Player.show_team p.team ^ ") ") ^ str
                ) else 
                    str
            ) in
            setText control str;
    in

    set_control_html survived Player.Alive;
    set_control_html escaped Player.Escaped;
    set_control_html killed Player.Killed;
;;

let timeout amt fn =
    Dom_html.setTimeout fn amt |> ignore
;;

let show_card data onclick =
    let parent = get_elem_id "card-svg-holder" in
    let svg = get_embed "card-svg-object" in
    Dom.removeChild parent svg;
    (* Recreate the embedded container *)
    let svg = Dom_html.createEmbed Dom_html.document in
    svg##.id := Js.string "card-svg-object";
    svg##.className := Js.string "card-svg";
    svg##._type := Js.string "image/svg+xml";
    svg##.src := Js.string data;
    Dom.appendChild parent svg;
    let modal = get_elem_id "card-flip-modal" in
    modal##.onclick := Dom_html.handler (fun _ ->
        modal##.onclick := Dom_html.no_handler;
        onclick();
        disable_modal "card-flip-modal";
        Js._false
    );
    enable_modal "card-flip-modal";
;;

let rec pick_move_click moves click layout x y =
    let coord = 
        Point.(make x y)
        |> Layout.pixel_to_hex layout
        |> FractHex.round 
    in
    if HexMap.Set.mem moves coord then (
        click coord;
    )

and pick_noise_sector (info : Grid.context_info ref) (my_id : Game.Player.id) (state : Game.state) apply_move =
    let moves = Map.keys state.map.map |> HexMap.Set.of_list in
    draw_moves !info.layout moves;
    draw_player !info state my_id;
    set_gui_handlers !info.layout
        ~move_handler:(fun gui layout x y ->
            pick_move_handler moves gui layout x y;
            draw_player !info state my_id;
        )
        ~click_handler:(pick_move_click moves (fun coord ->
            clear_gui_handlers();
            clear_gui();
            draw_player !info state my_id;
            apply_move Game.Move.(NoiseInAnySector coord);
        ))

and update_ui_for_state (info : Grid.context_info ref) (apply_move : Game.Move.t -> unit) (my_id : Game.Player.id) (state : Game.state): unit =
    (*Printf.sprintf "My ID %d" my_id |> Caml.print_endline;
    Printf.sprintf "UPDATING UI STATE %s" (Game.show_state state) |> Caml.print_endline;
    *)
    draw_annotations !info annot_state;
    set_round_counter state;
    set_player_turn state my_id;
    set_event_list state;
    set_sector_history my_id state;
    (*set_buttons info state my_id apply_move;*)
    match state.Game.next with
    | Game.NextAction.CurrentPlayerPickMove moves when state.current_player = my_id -> 
        set_gui_handlers !info.layout
            ~move_handler:(fun gui layout x y ->
                pick_move_handler moves gui layout x y;
                draw_player !info state my_id;
            )
            ~click_handler:(pick_move_click moves (fun coord -> 
                clear_gui_handlers();
                clear_gui();
                let move = Game.Move.PlayerMove coord in
                apply_move move;
            ));
        draw_moves !info.layout moves; 
        draw_player !info state my_id;
    | ConfirmSafeSector ->
        clear_gui_handlers();
        clear_gui();
        apply_move Game.Move.AcceptSafeSector
    | ConfirmSilenceInAllSectors ->
        clear_gui_handlers();
        clear_gui();
        show_card svg_silence_all_sectors (fun () ->
            apply_move Game.Move.AcceptSilenceInAllSectors
        );
    | ConfirmNoiseInYourSector ->
        clear_gui_handlers();
        clear_gui();
        show_card svg_noise_in_your_sector (fun () ->
            apply_move Game.Move.AcceptNoiseInYourSector
        );
    | PickNoiseInAnySector ->
        clear_gui_handlers();
        clear_gui();
        show_card svg_noise_in_any_sector (fun () ->
            pick_noise_sector info my_id state apply_move
        );
    | ConfirmEscapePodDamaged ->
        clear_gui_handlers();
        clear_gui();
        show_card svg_escape_pod_damaged (fun () ->
            apply_move Game.Move.AcceptEscapePodDamaged
        );
    | ConfirmEscapePodUndamaged ->
        clear_gui_handlers();
        clear_gui();
        show_card svg_escape_pod_undamaged (fun () ->
            apply_move Game.Move.AcceptEscapePodUndamaged
        );
    | DecideToAttack coord -> 
        (* Remove pointer *)
        clear_gui_handlers();
        draw_player !info state my_id;
        do_decide_to_attack !info coord apply_move
    | GameOver end_state ->
        clear_gui_handlers();
        clear_gui();
        do_game_over state.players end_state;
    | _ -> 
        clear_gui_handlers();
        clear_gui();
        draw_player !info state my_id;
;;

let update_noise_ping (_state : Game.state) (pid : Game.Player.id) (loc : Point.t) (size : float) : unit =
    let elem = Dom_html.getElementById_exn "noise-ping-container" in
    let px f =
        f |> Float.round |> Int.of_float |> Int.to_string |> fun s -> s ^ "px" |> Js.string
    in
    let x = loc.x -. size*.0.5 in
    let y = loc.y -. size*.0.5 in
    elem##.style##.width := px size;
    elem##.style##.height := px size;
    elem##.style##.left := px x;
    elem##.style##.top := px y;
    elem##.style##.display := Js.string "flex";
    (* Change circle colors *)
    let noises = get_elems_by_class "noise-ping-circle" in
    let color = pid_to_color pid in
    List.iter noises ~f:(fun circle ->
        circle##.style##.backgroundColor := Js.string color;
    );

    timeout 2000. (fun () ->
        elem##.style##.display := Js.string "none";
    );
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

let update_event_diff (info : Grid.context_info) (_pid : Game.Player.id) (game : Game.state) events =
    List.iter events ~f:(function
        | Game.Event.Noise (pid, coord)
        | Game.Event.Attack (pid, coord, _) ->
            let loc = Layout.hex_to_pixel info.layout coord in
            let size = info.hex_size *. 2. in
            update_noise_ping game pid loc size
        | Game.Event.Escape _
        | Game.Event.EscapeFailed _ -> 
            draw_map game
        | _ -> ()
    )
;;

let not_empty_string = Fn.compose not String.is_empty

let setup_lobby_modal ?(on_click=fun()->())  host =
    let btn = get_btn "lobby-start-btn" in
    if not host then (
        btn##.style##.display := Js.string "none";
    ) else (
        btn##.onclick := Dom_html.handler (fun _ ->
            on_click();
            Js._false
        );
    )
;;

let resize_callback (info : Grid.context_info ref) (game : Game.state ref) (my_id : Game.Player.id) apply_move =
    Dom_html.window##.onresize := Dom_html.handler (fun _ ->
        let map_canvas = Canvas.get_canvas map_canvas in
        let annot_canvas = Canvas.get_canvas annot_canvas in
        let gui_canvas = Canvas.get_canvas gui_canvas in
        reset_canvas_size map_canvas;
        reset_canvas_size annot_canvas;
        reset_canvas_size gui_canvas;
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi annot_canvas;
        Canvas.fix_canvas_dpi gui_canvas;

        info := Grid.calculate_info map_canvas Game.board_size_w Game.board_size_h;

        draw_map !game;
        update_ui_for_state info apply_move my_id !game;

        Js._false
    );
;;

let setup_annotations (info : Grid.context_info ref) =
    let toggle = get_input "annotate-toggle" in
    toggle##.checked := Js._false;
    let label = get_elem_id "annotate-toggle-label" in
    label##.onclick := Dom_html.handler (fun _ ->
        toggle_input toggle;
        if toggle##.checked |> Js.to_bool then (
            (* enable annotation mode *)
            Annotations.enable annot_state;
            draw_annotations !info annot_state;
        ) else (
            (* disable annotation mode *)
            disable_all_annotate_player_toggles();
            Annotations.disable annot_state;
            disable_annotations_ui_selection !info;
        );
        Js._false
    );
    (*toggle##.onchange := Dom_html.handler (fun _ ->
        Caml.print_endline "CHANGE";
        Js._false
    );*)
;;

let setup_join_modal () =
    let join_btn = get_btn "join-modal-btn" in
    let name = get_input "join-modal-name" in
    let game = get_input "join-modal-game-id" in
    let server = get_input "join-modal-server" in
    join_btn##.onclick := Dom_html.handler (fun _ ->
        (* Verify each field *)
        let name = input_value name in
        let game_id = input_value game in
        let server = input_value server in
        if not_empty_string name
           && not_empty_string game_id
           && not_empty_string server then (
               disable_modal "join-modal";
               setup_lobby_modal false;
               enable_modal "lobby-modal";

               let game = ref Game.empty_game in
               let map_canvas = Canvas.get_canvas map_canvas in
               let info = ref (Grid.calculate_info map_canvas Game.board_size_w Game.board_size_h) in

               let apply_move move =
                   Multiplayer.(send_to_server_join (create_player_update move));
               in

               let first_draw = ref false in

               let callback = Js.wrap_callback (fun data ->
                   match Js.to_string data##._type with
                   | "game-update" -> 
                       disable_modal "lobby-modal";
                       let update = Multiplayer.parse_game_update data##.data in
                       let event_diff = Game.event_diff ~old:!game ~new_:update in
                       game := update;
                       let id : int = Js.Unsafe.(js_expr "player_id") in
                       if not !first_draw then (
                           setup_annotations info;
                           first_draw := true;
                           resize_callback info game id apply_move;
                           setup_annotation_ui info id game;
                           draw_map !game;
                       );
                       update_ui_for_state info apply_move id !game;
                       update_event_diff !info id !game event_diff;
                   | _ -> ()
               ) in

               let module U = Js.Unsafe in
               let f = U.global##.connectToLobby in
               let _send_callback =
                   U.fun_call f [|
                       U.inject Js.(string name);
                       U.inject Js.(string game_id);
                       U.inject Js.(string server);
                       U.inject callback;
                    |]
               in
               ()
       );

        Js._false
    )
;;

let populate_map_dropdown (map_ref : Maps.map_info ref) =
    let maps = Maps.built_ins in
    let select = get_select "host-modal-maps" in
    let custom_map_div = get_elem_id "map-string-entry" in
    List.iteri maps ~f:(fun idx map ->
        let opt = Dom_html.createOption Dom_html.document in
        setText opt map.name;
        opt##.onclick := Dom_html.handler (fun _ ->
            custom_map_div##.style##.display := Js.string "none";
            map_ref := map;
            Js._false;
        );
        if idx = 0 then (
            opt##.selected := Js._true;
        );
        Dom.appendChild select opt
    );
    (* Custom entry *)
    let opt = Dom_html.createOption Dom_html.document in
    setText opt "Custom (Enter String)";
    let text_area = get_text_area "custom-map-string" in
    opt##.onclick := Dom_html.handler (fun _ ->
        let text_len = text_area##.value##.length in
        custom_map_div##.style##.display := Js.string "";
        if text_len > 0 then (
            map_ref := Maps.{
                name = "Custom Map";
                value = String (text_area##.value |> Js.to_string);
            };
        );
        Js._false;
    );
    text_area##.onkeyup := Dom_html.handler (fun _ ->
        map_ref := Maps.{
            name = "Custom Map";
            value = String (text_area##.value |> Js.to_string);
        };
        Js._false;
    );
    Dom.appendChild select opt;
;;

let setup_host_modal () =
    let host_btn = get_btn "host-modal-btn" in
    let name = get_input "host-modal-name" in
    let num_players = get_select "host-modal-players" in
    let server = get_input "host-modal-server" in

    let map_choice_ref = ref List.(hd_exn Maps.built_ins) in
    populate_map_dropdown map_choice_ref;

    host_btn##.onclick := Dom_html.handler (fun _ ->
        let name = input_value name in
        let player_count = select_value num_players |> Int.of_string in
        let server = input_value server in

        if not_empty_string name 
           && not_empty_string server then (
               let module Host = Multiplayer.Host in
               (* Kind of gross state bleed here *)
               let player_list = ref [] in

               let map_canvas = Canvas.get_canvas map_canvas in
               let info = ref (Grid.calculate_info map_canvas Game.board_size_w Game.board_size_h) in
               let host = ref None in
               let game = ref Game.empty_game in

               let get_host() = Option.value_exn !host in

               disable_modal "host-modal";
               setup_lobby_modal true ~on_click:(fun () -> 
                   setup_annotations info;
                   (* Game has started *)
                   let apply_move move = 
                       Host.do_move (get_host()) move;
                   in
                   let update_ui event_diff state = 
                       (*Caml.print_endline "GUI UPDATE";
                       Game.show_state state |> Caml.print_endline;*)
                       update_ui_for_state info apply_move 0 state;
                       update_event_diff !info 0 !game event_diff;
                   in
                   let _ =
                        let map = Maps.build_map !map_choice_ref.value in
                        game := Game.new_game !player_list map
                   in
                   host := Some (Host.create game update_ui);
                   setup_annotation_ui info 0 game;
                   draw_map !game;
                   resize_callback info game 0 apply_move;
                   Host.send_state_update [] (get_host());
                   disable_modal "lobby-modal";
               );
               enable_modal "lobby-modal";

               let callback = Js.wrap_callback (fun data ->
                   match Js.to_string data##._type with
                   | "player-list-update" -> 
                        player_list := str_array_to_str_list data##.players;
                        List.iter ~f:Caml.print_endline !player_list
                   | "game-update" -> ()
                   | "player-dropped" -> ()
                   | "player-update" ->
                        (* Grab the player move *)
                        let move = 
                            Js._JSON##stringify data##.data
                            |> Js.to_string
                            |> Yojson.Safe.from_string
                            |> Game.Move.of_yojson
                            |> function
                                | Ok move -> move
                                | Error err -> failwith err
                        in
                        Host.do_move (get_host()) move
                   | _ -> ()
               ) in

               let module U = Js.Unsafe in
               let f = U.global##.setupLobby in
               U.fun_call f [|
                   U.inject Js.(string name);
                   U.inject player_count; 
                   U.inject Js.(string server);
                   U.inject callback;
                |]
       );

        Js._false
    )
;;

let initialize_modals () =
    let host_btn = get_btn "host-btn" in
    let join_btn = get_btn "join-btn" in

    host_btn##.onclick := Dom_html.handler (fun _ ->
        disable_modal "host-or-join-modal";
        enable_modal "host-modal";
        Js._false
    );

    join_btn##.onclick := Dom_html.handler (fun _ ->
        disable_modal "host-or-join-modal";
        enable_modal "join-modal";
        Js._false
    );

    setup_join_modal();
    setup_host_modal();
;;

let attach () =
    Dom_html.window##.onload := Dom_html.handler (fun _ ->
        initialize_modals();

        let map_canvas = get_canvas map_canvas in
        let annot_canvas = get_canvas annot_canvas in
        let gui_canvas = get_canvas gui_canvas in
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi gui_canvas;
        Canvas.fix_canvas_dpi annot_canvas;

        Js._false
    );
