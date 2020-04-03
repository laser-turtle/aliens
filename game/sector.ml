open! Base

type t = Dangerous
       | AlienSpawn
       | HumanSpawn
       | Safe
       | EscapeHatch of int
       [@@deriving show{with_path=false}, yojson]
