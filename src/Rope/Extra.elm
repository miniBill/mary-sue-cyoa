module Rope.Extra exposing (join)

import Rope exposing (Rope)


join : a -> List (Rope a) -> Rope a
join sep ropes =
    ropes
        |> List.intersperse (Rope.singleton sep)
        |> Rope.fromRopeList
