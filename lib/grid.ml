open! Base
open! Js_of_ocaml

let float = Caml.float

module HexCoord = struct
    module T = struct
        type t = 
            | Cube of {
                q : int;
                r : int;
                s : int;
            }
            | Axial of {
                q : int;
                r : int;
            }
            [@@deriving compare, sexp]

        let make q r s =
            Cube {q;r;s}

        let make_axis q r = 
            Axial {q;r}

        let q : t -> int = function
            | Cube {q; _} -> q
            | Axial {q; _} -> q

        let r : t -> int = function
            | Cube {r; _} -> r
            | Axial {r; _} -> r

        let s : t -> int = function
            | Cube {s; _} -> s
            | Axial {q; r} -> -q - r

        let qf (t : t) : float =
            q t |> Float.of_int

        let rf (t : t) : float =
            r t |> Float.of_int

        let sf (t : t) : float =
            s t |> Float.of_int

        let equal (t1 : t) (t2 : t) : bool =
            Int.(q t1 = q t2 &&
                 r t1 = r t2 &&
                 s t1 = s t2)

        let add (a : t) (b : t) : t =
            Cube {q = q a + q b;
                  r = r a + r b;
                  s = s a + s b; }

        let sub (a : t) (b : t) : t =
            Cube {q = q a - q b;
                  r = r a - r b;
                  s = s a - s b; }

        let mul (a : t) (k : int) : t =
            match a with
            | Cube {q;r;s} -> Cube {q=q*k; r=r*k; s=s*k}
            | Axial {q;r} -> Axial {q=q*k; r=r*k}

        let length (t : t) : int =
            (abs (q t) + abs (r t) + abs (s t)) / 2

        let distance (a : t) (b : t) : int =
            length (sub a b)

        type dir = 
            | BR
            | B
            | BL
            | TL
            | TR
            | T

        let direction_coord = function
            | BR -> make 1 0 ~-1
            | B -> make 1 ~-1 0
            | BL -> make 0 ~-1 1
            | TL -> make ~-1 0 1
            | TR -> make ~-1 1 0
            | T -> make 0 1 ~-1

        let neighbor (t : t) (d : dir) : t =
            add t (direction_coord d)

        let (+) = add
        let (-) = sub
        let ( * ) = mul
        let (=) = equal
        let (<>) a b = not (a = b)
    end

    include T

    include Comparator.Make(T)
end

module FractHex = struct
    type t = {
        q : float;
        r : float;
        s : float;
    }

    let make q r s = {
        q; r; s;
    }

    let of_hex_coord (h : HexCoord.t) : t = {
        q = HexCoord.qf h;
        r = HexCoord.rf h;
        s = HexCoord.sf h;
    }

    let round (t : t) : HexCoord.t =
        let q = Float.round t.q
        and r = Float.round t.r
        and s = Float.round t.s in
        let q_diff = Float.abs q -. t.q
        and r_diff = Float.abs r -. t.r
        and s_diff = Float.abs s -. t.s in
        let q = Float.to_int q
        and r = Float.to_int r
        and s = Float.to_int s in
        if Float.(q_diff > r_diff && q_diff > s_diff) then
            HexCoord.make (-r - s) r s
        else if Float.(r_diff > s_diff) then
            HexCoord.make q (-q - s) r
        else
            HexCoord.make q r (-q - r)
    ;;
end

module Line = struct
    let lerp (a : float) (b : float) (t : float) =
        a *. (1. -. t) +. b *. t
    ;;

    let hex_lerp (a : HexCoord.t) (b : HexCoord.t) (t : float) : FractHex.t =
        let af = FractHex.of_hex_coord a
        and bf = FractHex.of_hex_coord b in
        FractHex.make (lerp af.q bf.q t)
                      (lerp af.r bf.r t)
                      (lerp af.s bf.s t)
    ;;

    let hex_line (a : HexCoord.t) (b : HexCoord.t) : HexCoord.t array =
        let n = HexCoord.distance a b in
        let nf = Float.of_int n in
        let step = 1. /. Float.max nf 1. in
        Array.init n ~f:(fun idx ->
            hex_lerp a b (step *. Float.of_int idx) |> FractHex.round
        )
    ;;
end

module Orientation = struct
    type t = {
        f0 : float;
        f1 : float;
        f2 : float;
        f3 : float;
        b0 : float;
        b1 : float;
        b2 : float;
        b3 : float;
        start_angle : float;
    }
    
    let make ~f0 ~f1 ~f2 ~f3 ~b0 ~b1 ~b2 ~b3 ~start_angle = {
        f0; f1; f2; f3; b0; b1; b2; b3; start_angle;
    }
end

module Point = struct
    type t = {
        x : float;
        y : float;
    }

    let zero = {
        x = 0.; y = 0.;
    }

    let add (a : t) (b : t) = {
        x = a.x +. b.x;
        y = a.y +. b.y;
    }

    let sub (a : t ) (b : t) = {
        x = a.x -. b.x;
        y = a.y -. b.y;
    }

    let scale (a : t) (s : float) = {
        x = a.x *. s;
        y = a.y *. s;
    }

    let floor (a : t) = {
        x = Float.round_down a.x;
        y = Float.round_down a.y;
    }

    let dot (a : t) (b : t) =
        a.x *. b.x +. a.y *. b.y

    let length (a : t) = 
        Float.sqrt (dot a a)

    let normalize (a : t) = 
        let len = length a in
        { x = a.x /. len;
          y = a.y /. len; }

    let (+) = add
    let (-) = sub
    let ( * ) = scale

    let make x y = {
        x; y;
    }
end

module Layout = struct
    let sqrt_3 = Float.sqrt 3.

    let pointy = Float.(Orientation.make
        ~f0:sqrt_3 ~f1:(sqrt_3 / 2.) ~f2:0. ~f3:(3. / 2.)
        ~b0:(sqrt_3 / 3.) ~b1:(-1. / 3.) ~b2:0. ~b3:(2. / 3.)
        ~start_angle:0.5
    )

    let flat = Float.(Orientation.make
        ~f0:(3.0 / 2.0) ~f1:0.0 ~f2:(sqrt_3 / 2.0) ~f3:sqrt_3
        ~b0:(2.0 / 3.0) ~b1:0.0 ~b2:(-1.0 / 3.0) ~b3:(sqrt_3 / 3.0)
        ~start_angle:0.
    )

    type t = {
        orientation : Orientation.t;
        size : Point.t;
        origin : Point.t;
    }

    let make orientation size origin = {
        orientation; size; origin;
    }

    let hex_to_pixel (t : t) (h : HexCoord.t) : Point.t =
        let module H = HexCoord in
        let open Float in
        let m = t.orientation in
        let x = (m.f0 * H.qf h + m.f1 * H.rf h) * t.size.x in
        let y = (m.f2 * H.qf h + m.f3 * H.rf h) * t.size.y in
        Point.make (x + t.origin.x) (y + t.origin.y)
    ;;

    let pixel_to_hex (t : t) (p : Point.t) =
        let module H = HexCoord in
        let open Float in
        let m = t.orientation in
        let x = (p.x - t.origin.x) / t.size.x in
        let y = (p.y - t.origin.y) / t.size.y in
        let q = m.b0 * x + m.b1 * y in
        let r = m.b2 * x + m.b3 * y in
        (q, r, -q - r)
    ;;

    let hex_corner_offset (t : t) (corner : int) : Point.t =
        let open Float in
        let corner = Float.of_int corner in
        let angle = 2. * Float.pi * (t.orientation.start_angle + corner) / 6. in
        Point.make (t.size.x * cos angle) (t.size.y * sin angle)
    ;;

    let polygon_corners (t : t) (h : HexCoord.t) : Point.t array =
        let center = hex_to_pixel t h in
        let result = [|
            Point.zero; Point.zero; Point.zero;
            Point.zero; Point.zero; Point.zero;
        |] in
        let open Float in
        for i=0 to 5 do
            let offset = hex_corner_offset t i in
            result.(i) <- Point.make (center.x + offset.x) (center.y + offset.y)
        done;
        result
    ;;
end

module HexMap = struct
    type 'a t = (HexCoord.t, 'a, HexCoord.comparator_witness) Map.t

    let empty : 'a t = Map.empty (module HexCoord)

    let create_grid w h ~f =
        let map = ref empty in
        for s=0 to w-1 do
            let s_offset = Float.(round_down ((of_int s) / 2.)) |> Int.of_float in
            for r = -s_offset to (h - s_offset - 1) do
                let h = HexCoord.make s r (-r - s) in
                map := Map.set !map ~key:h ~data:(f h)
            done
        done;
        !map
    ;;
end

let polygon_path (context : Dom_html.canvasRenderingContext2D Js.t) (points : Point.t array) : unit =
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

module Sector = struct
    type t = Dangerous
           | AlienSpawn
           | HumanSpawn
           | Safe
           | Unused
           | EscapeHatch of int
end

type canvas = Dom_html.canvasElement Js.t
type context_2d = Dom_html.canvasRenderingContext2D Js.t

let calculate_grid_dimensions map_w map_h (canvas : canvas) =
    let client_w = canvas##.clientWidth |> Float.of_int in
    let client_h = canvas##.clientHeight |> Float.of_int in
    let map_wf = Float.of_int map_w in
    let map_hf = Float.of_int map_h in
    let s60 = 0.8660254037844386 in
    let total_w = 1.5 *. map_wf +. 0.5 in
    let total_h = s60 *. (2. *. map_hf +. 1.) in
    let w_x = client_w /. total_w in
    let h_y = client_h /. total_h in
    let size = Float.min w_x h_y in
    let grid_h = size*.((2. *. map_hf)*.s60) in
    let grid_w = size*.(map_wf*.1.5-.1.5) in
    if Float.(w_x < h_y) then (
        let cy = (client_h -. grid_h) *. 0.5 in
        (w_x, cy, grid_w, grid_h, size)
    ) else (
        let cx = (client_w -. grid_w) *. 0.5 in
        (cx, h_y, grid_w, grid_h, size)
    )
;;

module Colors = struct
    let background = "#FFFFFF"
    let hex_border = "#797B7C"
    let dangerous_fill = "#CCCCCC"
    let stripes = "#BAB8B9"
    let black = "#100E0C"
    let sector_text = "#646362"
end

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

    context##.lineWidth := 0.7;
    context##.strokeStyle := Js.(string Colors.stripes);
    (* 4 * 23 = 92 lines ? *)
    (* 28 * 6 = 168 *)
    let hyp = Float.sqrt (w*.w +. h*.h) in
    let spacing = Float.round_down (hyp /. 168.) in
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

let column_letters = [|
    "A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J"; "K";
    "L"; "M"; "N"; "O"; "P"; "Q"; "R"; "S"; "T"; "U"; "V"; "W";
|]

let hex_coord_to_sector_location (hcoord : HexCoord.t) : string =
    let col = HexCoord.q hcoord in
    let row = HexCoord.r hcoord + ((HexCoord.q hcoord + -1 * (HexCoord.q hcoord land 1)) / 2) in
    let letter = column_letters.(col) in
    let row = Printf.sprintf "%02d" (row + 1) in
    letter ^ row
;;

let draw_sector_coord (info : context_info) (hcoord : HexCoord.t) =
    (* Draw some text in the middle *)
    let layout = info.layout in
    let context = info.context in
    let size = info.hex_size in
    let font_size = size *. 0.65 in
    context##save;
    let center = Layout.hex_to_pixel layout hcoord in
    let text = Js.string (hex_coord_to_sector_location hcoord) in
    context##.font := Js.string ((font_size |> Float.to_string) ^ "px monospace");
    context##.fillStyle := Js.string Colors.sector_text;
    let text_width = (context##measureText text)##.width in
    let x = center.x -. text_width*.0.5 in
    let y = center.y +. font_size*.0.3 in
    context##fillText text x y;
    context##restore;
;;

let draw_safe_sector info hcoord =
    let context = info.context in
    let layout = info.layout in
    context##.fillStyle := Js.(string "white");
    context##.strokeStyle := Js.(string Colors.hex_border);
    let poly = Layout.polygon_corners layout hcoord in
    polygon_path context poly;
    context##fill;
    polygon_path context poly;
    context##stroke;
    draw_sector_coord info hcoord;
;;

let draw_dangerous_sector info hcoord =
    let context = info.context in
    let layout = info.layout in
    context##.fillStyle := Js.(string Colors.dangerous_fill);
    context##.strokeStyle := Js.(string Colors.hex_border);
    let poly = Layout.polygon_corners info.layout hcoord in
    polygon_path context poly;
    context##fill;
    polygon_path context poly;
    context##stroke;

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
    ()
;;

let draw_human_spawn info hcoord =
    ()
;;

let draw_alien_spawn info hcoord =
    ()
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
    | Unused -> ()
;;

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

let draw (canvas : canvas) =
    let context = canvas##getContext Dom_html._2d_ in
    let client_wf = canvas##.clientWidth |> Float.of_int in
    let client_hf = canvas##.clientHeight |> Float.of_int in
    resize canvas context Dom_html.window##.devicePixelRatio 
        client_wf client_hf;

    let map_w = 23
    and map_h = 14 in

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

    let map = HexMap.create_grid map_w map_h ~f:(fun _ -> ()) in

    draw_background context origin size grid_size;
    draw_top_axis context_info;

    context##.fillStyle := Js.(string "white");
    context##.strokeStyle := Js.(string Colors.hex_border);
    Map.iter_keys map ~f:(fun coord ->
        if Random.int 100 > 50 then (
            let sector_type = 
                let r = Random.int 100 in
                if r < 25 then Sector.Safe
                else Sector.Dangerous
            in
            draw_hex_item context_info coord sector_type
        )
    );
;;
