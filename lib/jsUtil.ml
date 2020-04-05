open! Base
open! Js_of_ocaml

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

let get_object id =
    get_elem_type id (function
        | Object o -> o
        | _ -> failwith "expected object"
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

let str_array_to_str_list (str_array : Js.string_array Js.t) : string list =
    str_array
    |> Js.str_array
    |> Js.to_array
    |> Array.map ~f:Js.to_string
    |> Array.to_list
;;

let input_value input =
    Js.to_string input##.value 

let enable_modal id =
    (get_elem_id id)##.className := Js.string "modal is-active"
;;

let disable_modal id =
    (get_elem_id id)##.className := Js.string "modal"
;;

