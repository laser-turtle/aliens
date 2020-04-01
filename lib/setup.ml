open! Base
open! Js_of_ocaml

let map_canvas = "map-canvas"
let gui_canvas = "gui-canvas"
let canvas_parent = "canvas-container"

let get_canvas = Canvas.get_canvas
let reset_canvas_size = Canvas.reset_canvas_size canvas_parent 

let game = 
    ref (Game.new_game 4 (Game.generate_map()))

let apply_move (game : Game.state) (move : Game.Move.t) : Game.state =
    Game.apply game move
    |> function
       | Ok state -> state
       | Error _ -> Caml.print_endline "GAME ERRORED"; game
;;

let set_round_counter (game : Game.state) : unit =
    let round = Dom_html.getElementById_exn "round-text" in
    round##.innerHTML := Js.string (Printf.sprintf "Round %d/%d" game.round game.max_rounds);
;;

let set_player_turn (state : Game.state) : unit =
    set_round_counter state;
    let player_content = Dom_html.getElementById_exn "player-turns" in
    let player_type = Dom_html.getElementById_exn "player-type" in
    let player_string =
        match (Game.current_player state).team with
        | Alien -> "Alien"
        | Human -> "Human"
    in
    player_type##.innerHTML := Js.string player_string;
    player_content##.innerHTML := Js.string "";
    List.iteri state.players ~f:(fun idx player ->
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
        let p = Dom_html.createP Dom_html.document in
        p##.innerHTML := Js.(string event_str);
        Dom.appendChild content p
    )
;;

let apply_global (move : Game.Move.t) : unit =
    (*Caml.print_endline "Applying global move...";*)
    game := apply_move !game move;
    (*Caml.print_endline Game.(show_state !game);*)
;;

let safe_sector_click () =
    apply_global Game.Move.AcceptSafeSector
;;

let silence_sector_click () =
    apply_global Game.Move.AcceptSilenceInAllSectors
;;

let noise_your_sector_click () =
    apply_global Game.Move.AcceptNoiseInYourSector
;;

let get_elem_id id =
    Dom_html.getElementById_exn id

let get_elem_type id fn =
    id
    |> Dom_html.getElementById_exn
    |> Dom_html.tagged
    |> fn
;;

let get_btn id =
    get_elem_type id (function
        | Button b -> b
        | _ -> failwith "expected button"
    )
;;

let get_input id =
    get_elem_type id (function
        | Input i -> i
        | _ -> failwith "expected input"
    )
;;

let get_select id =
    get_elem_type id (function
        | Select s -> s
        | _ -> failwith "expected select"
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

let draw_player (info : Grid.context_info) (state : Game.state) =
    let module Player = Game.Player in
    let player = Game.current_player state in
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

let do_decide_to_attack (info : Grid.context_info) (coord : HexCoord.t) =
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
    Caml.print_endline x;
    Caml.print_endline y;
    (*let x = Float.max x 0. |> Float.to_string in*)
    div##.style##.left := Js.string (x ^ "px");
    div##.style##.top := Js.string (y ^ "px");
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

and set_buttons (info : Grid.context_info ref) (state : Game.state) : unit =
    (* TODO - might be easier to pull up the click handlers to here *)
    let noise_your = get_btn "noise-in-your-sector-btn" in
    let noise_any = get_btn "noise-in-any-sector-btn" in
    let silence = get_btn "silence-in-all-sectors-btn" in
    let safe = get_btn "safe-btn" in
    let enable el = el##.disabled := Js._false in
    let disable lst =
        List.iter lst ~f:(fun element ->
            element##.disabled := Js._true
        )
    in
    disable [noise_your; noise_any; silence; safe];
    let open Game in
    match state.next with
    | NextAction.ConfirmNoiseInYourSector -> 
        enable noise_your
    | ConfirmSafeSector -> 
        enable safe
    | ConfirmSilenceInAllSectors -> 
        enable silence
    | PickNoiseInAnySector -> 
        enable noise_any;
        let moves = Map.keys state.map.map |> HexMap.Set.of_list in
        draw_moves !info.layout moves;
        draw_player !info state;
        set_gui_handlers !info.layout
            ~move_handler:(fun gui layout x y ->
                pick_move_handler moves gui layout x y;
                draw_player !info state;
            )
            ~click_handler:(pick_move_click moves (fun coord ->
                clear_gui_handlers();
                clear_gui();
                draw_player !info state;
                apply_global Game.Move.(NoiseInAnySector coord);
                update_ui_for_state info !game;
            ));

    | _ -> ()

and update_ui_for_state (info : Grid.context_info ref) (state : Game.state) : unit =
    set_round_counter state;
    set_player_turn state;
    set_event_list state;
    set_sector_history state;
    set_buttons info state;
    match state.Game.next with
    | Game.NextAction.CurrentPlayerPickMove moves -> 
        set_gui_handlers !info.layout
            ~move_handler:(fun gui layout x y ->
                pick_move_handler moves gui layout x y;
                draw_player !info state;
            )
            ~click_handler:(pick_move_click moves (fun coord -> 
                let move = Game.Move.PlayerMove coord in
                apply_global move;
                update_ui_for_state info !game
            ));
        draw_moves !info.layout moves; 
        draw_player !info state;
    | PickNoiseInAnySector -> ()
    | DecideToAttack coord -> 
        (* Remove pointer *)
        clear_gui_handlers();
        do_decide_to_attack !info coord
    | _ -> 
        clear_gui_handlers();
        clear_gui();
        draw_player !info state;
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

let initialize_buttons (info : Grid.context_info ref) =
    let set_click id fn = 
        let btn = get_btn id  in
        btn##.onclick := Dom_html.handler (fun _ ->
            fn();
            update_ui_for_state info !game;
            Js._false;
        )
    in
    set_click "noise-in-your-sector-btn" noise_your_sector_click;
    set_click "silence-in-all-sectors-btn" silence_sector_click;
    set_click "safe-btn" safe_sector_click;

    set_click "attack" (fun () ->
        apply_global Game.Move.AcceptAttack;
        hide_attack_container();
        update_ui_for_state info !game;
    );

    set_click "dont-attack" (fun () ->
        apply_global Game.Move.DeclineAttack;
        hide_attack_container();
        update_ui_for_state info !game;
    );
;;

let enable_modal id =
    Dom_html.(getElementById_exn id)##.className := Js.string "modal is-active"
;;

let disable_modal id =
    Dom_html.(getElementById_exn id)##.className := Js.string "modal"
;;

let input_value input =
    Js.to_string input##.value 

let not_empty_string = Fn.compose not String.is_empty

let setup_lobby_modal host =
    let btn = get_btn "lobby-start-btn" in
    if not host then (
        btn##.style##.display := Js.string "none";
    ) else (
        btn##.onclick := Dom_html.handler (fun _ ->
            Js._false
        );
    )
;;

let setup_join_modal () =
    let join_btn = get_btn "join-modal-btn" in
    let name = get_input "join-modal-name" in
    let game = get_input "join-modal-game-id" in
    let server = get_input "join-modal-server" in
    join_btn##.onclick := Dom_html.handler (fun _ ->
        (* Verify each field *)
        let name = input_value name in
        let game = input_value game in
        let server = input_value server in
        if not_empty_string name
           && not_empty_string game
           && not_empty_string server then (
               disable_modal "join-modal";
               setup_lobby_modal false;
               enable_modal "lobby-modal";
       );

        Js._false
    )
;;

let select_value (select : Dom_html.selectElement Js.t) : string =
    let item = select##.options##item select##.selectedIndex in
    item
    |> Js.Opt.to_option
    |> function
       | Some item -> item##.text |> Js.to_string
       | None -> failwith "expected some item"
;;

let setup_host_modal () =
    let host_btn = get_btn "host-modal-btn" in
    let name = get_input "host-modal-name" in
    let num_players = get_select "host-modal-players" in
    let server = get_input "host-modal-server" in

    host_btn##.onclick := Dom_html.handler (fun _ ->
        let name = input_value name in
        let _player_count = select_value num_players |> Int.of_string in
        let server = input_value server in

        if not_empty_string name 
           && not_empty_string server then (
               disable_modal "host-modal";
               setup_lobby_modal true;
               enable_modal "lobby-modal"
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
        let gui_canvas = get_canvas gui_canvas in
        Canvas.fix_canvas_dpi map_canvas;
        Canvas.fix_canvas_dpi gui_canvas;

        let info = ref (draw_map !game) in

        initialize_buttons info;

        (* Setup generic buttons *)
        update_ui_for_state info !game;

        let loop () =
            (*Caml.print_endline "Making move...";*)
            game := advance_game !game;
            (*Caml.print_endline Game.(show_state !game);*)
            update_ui_for_state info !game;
        in

        let btn = Dom_html.getElementById_exn "advance" in
        btn##.onclick := Dom_html.handler (fun _ ->
            loop();
            Js._false
        );

        Dom_html.window##.onresize := Dom_html.handler (fun _ ->
            reset_canvas_size gui_canvas;
            reset_canvas_size map_canvas;
            Canvas.fix_canvas_dpi gui_canvas;
            Canvas.fix_canvas_dpi map_canvas;

            info := draw_map !game;
            update_ui_for_state info !game;

            Js._false
        );

        Js._false
    );
