open! Base

type escape_hatch_state = Damaged
                        | Undamaged
                        | Used
[@@deriving show{with_path=false}, yojson]

type t = Dangerous
       | AlienSpawn
       | HumanSpawn
       | Safe
       | EscapeHatch of int * escape_hatch_state
       [@@deriving show{with_path=false}, yojson]
