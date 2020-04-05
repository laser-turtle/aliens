open! Base
open! Js_of_ocaml
open JsUtil
open Ocaml_aliens_game

module Annotations = struct
    type annotation = {
        pids : Game.Player.id list;
    }

    type t = {
        mutable annots : annotation HexMap.t;
    }

    let add_player_to_coord (t : t) (_coord : HexCoord.t) (_pid : Game.Player.id) =
        (*
        match Map.find t.annots with 
        | Some lst ->
            let new_map = Map.set t.annots ~key:coord ~data:(id :: 
                *)
        t
    ;;

    let create () = {
        annots = HexMap.empty;
    }
end

let map_canvas = "map-canvas"
let annot_canvas = "annot-canvas"
let gui_canvas = "gui-canvas"
let canvas_parent = "canvas-container"

let svg_noise_in_your_sector = "noise_in_your_sector.svg"
let svg_noise_in_any_sector = "noise_any_sector.svg"
let svg_silence_all_sectors = "silence_all_sectors.svg"

let get_canvas = Canvas.get_canvas
let reset_canvas_size = Canvas.reset_canvas_size canvas_parent 

let set_round_counter (game : Game.state) : unit =
    let round = Dom_html.getElementById_exn "round-text" in
    round##.innerHTML := Js.string (Printf.sprintf "Round %d/%d" game.round game.max_rounds);
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
    title##.innerHTML := Js.string title_name;
    let player_string =
        match (Game.get_player state my_id).team with
        | Alien -> "Alien"
        | Human -> "Human"
    in
    player_type##.innerHTML := Js.string player_string;
    player_content##.innerHTML := Js.string "";
    List.iteri state.players ~f:(fun _ player ->
        let span = Dom_html.createSpan Dom_html.document in
        let pid = player.id in
        let className =
            if pid = state.current_player && pid = my_id then (
                "your-turn"
            ) else if pid = my_id then (
                "you-but-not-active"
            ) else if pid = state.current_player then (
                "active"
            ) else ""
        in
        span##.className := Js.string ("player " ^ className);
        span##.innerHTML := Js.string player.name;
        Dom.appendChild player_content span;
    )
;;

let set_sector_history (my_id : Game.Player.id)  (state : Game.state) : unit =
    let history = Dom_html.getElementById_exn "sector-history" in
    history##.innerHTML := Js.string "";
    let append_element idx sector =
        let div = Dom_html.createDiv Dom_html.document in
        let s1 = Dom_html.createStrong Dom_html.document in
        let s2 = Dom_html.createStrong Dom_html.document in
        let text = Dom_html.document##createTextNode Js.(string sector) in
        let idx = Printf.sprintf "%02d[" (idx + 1) in
        div##.className := Js.string "sector-item";
        s1##.innerHTML := Js.string idx;
        s2##.innerHTML := Js.string "]";
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
    content##.innerHTML := Js.string "";
    List.iter state.events ~f:(fun event ->
        let event_str = Game.event_to_string state event in
        let p = Dom_html.createP Dom_html.document in
        p##.innerHTML := Js.(string event_str);
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
    let context = Canvas.context gui in
    context##clearRect 0. 0. (Caml.float gui##.width) (Caml.float gui##.height);
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

(*
let advance_game (game : Game.state) : Game.state =
    let open Game in
    hide_attack_container();
    match game.next with
    | NextAction.CurrentPlayerPickMove moves ->
        let move = Set.choose_exn moves in
        apply_move game Move.(PlayerMove move)
    | ConfirmSilenceInAllSectors -> 
        apply_move game Move.AcceptSilenceInAllSectors
    | ConfirmNoiseInYourSector ->
        apply_move game Move.AcceptNoiseInYourSector
    | ConfirmSafeSector -> 
        apply_move game Move.AcceptSafeSector
    | DecideToAttack _coord ->
        if Random.int 10 = 1 then (
            apply_move game Move.AcceptAttack
        ) else (
            apply_move game Move.DeclineAttack
        )
    | PickNoiseInAnySector -> 
       apply_move game Move.(NoiseInAnySector (List.random_element_exn (Map.keys game.map.map)))
    | GameOver _ ->
        (* Display game over pop-up *)
        game
;;
*)

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
    let gui_loc = gui##getBoundingClientRect in
    let gx = gui_loc##.left in
    let gy = gui_loc##.top in
    let div = Dom_html.getElementById_exn "attack-choices" in
    remove_gui_pointer gui;
    div##.style##.display := Js.string "block";
    let bounds = div##getBoundingClientRect in
    let x = gx +. pt.x -. (bounds##.right -. bounds##.left)*.0.5 in
    let y = gy +. pt.y -. (bounds##.bottom -. bounds##.top)*.0.5 |> Float.to_string in
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
        | AllHumansEscapedOrKilled -> "Mixed - Some humans escaped"
        | RoundLimit -> "Round Limit Reached"
    in
    game_result##.innerHTML := Js.string win_string;

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
            control##.innerHTML := Js.string str;
    in

    set_control_html survived Player.Alive;
    set_control_html escaped Player.Escaped;
    set_control_html killed Player.Killed;
;;

let timeout amt fn =
    Dom_html.setTimeout fn amt |> ignore
;;

let show_card data onclick =
    let svg = get_object "card-svg-object" in
    svg##.data := Js.string data;
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
        (*timeout 1000. (fun () ->
            apply_move Game.Move.AcceptSafeSector
        )
        *)
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
    | DecideToAttack coord -> 
        (* Remove pointer *)
        clear_gui_handlers();
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

let update_noise_ping (loc : Point.t) (size : float) : unit =
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
    timeout 2000. (fun () ->
        elem##.style##.display := Js.string "none";
    );
;;

let update_event_diff (info : Grid.context_info) (_pid : Game.Player.id) (_game : Game.state) events =
    List.iter events ~f:(function
        | Game.Event.Noise (_, coord) ->
            let loc = Layout.hex_to_pixel info.layout coord in
            let size = info.hex_size *. 2. in
            update_noise_ping loc size
        | _ -> ()
    )
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

let setup_annotations () =
    let toggle = get_input "annotate-toggle" in
    let label = get_elem_id "annotate-toggle-label" in
    label##.onclick := Dom_html.handler (fun _ ->
        let flipped = toggle##.checked |> Js.to_bool |> not |> Js.bool in
        toggle##.checked := flipped;
        Js._false
    );
    toggle##.onchange := Dom_html.handler (fun _ ->
        if toggle##.checked |> Js.to_bool then (
            (* enable annotation mode *)
            ()
        ) else (
            (* disable annotation mode *)
            ()
        );
        Js._false
    );
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
                   Multiplayer.(send_to_server_join (create_player_update move))
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
                           setup_annotations();
                           first_draw := true;
                           resize_callback info game id apply_move;
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

let setup_host_modal () =
    let host_btn = get_btn "host-modal-btn" in
    let name = get_input "host-modal-name" in
    let num_players = get_select "host-modal-players" in
    let server = get_input "host-modal-server" in

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
                   setup_annotations();
                   (* Game has started *)
                   let apply_move move = 
                       Host.do_move (get_host()) move
                   in
                   let update_ui event_diff state = 
                       (*Caml.print_endline "GUI UPDATE";
                       Game.show_state state |> Caml.print_endline;*)
                       update_ui_for_state info apply_move 0 state;
                       update_event_diff !info 0 !game event_diff;
                   in
                   let _ =
                        let map = Game.generate_map() in
                        game := Game.new_game !player_list map
                   in
                   host := Some (Host.create game update_ui);
                   draw_map !game;
                   resize_callback info game 0 apply_move;
                   Host.send_state_update [] (get_host());
                   disable_modal "lobby-modal";
               );
               enable_modal "lobby-modal";

               let callback = Js.wrap_callback (fun data ->
                   (*Caml.print_endline (Js._JSON##stringify data |> Js.to_string);*)
                   match Js.to_string data##._type with
                   | "player-list-update" -> 
                        player_list := str_array_to_str_list data##.players;
                        List.iter ~f:Caml.print_endline !player_list
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
                        (*Caml.print_endline "Got player move";*)
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
