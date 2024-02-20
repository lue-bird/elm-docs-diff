module Diff exposing (toMagnitude, withEquivalence)

import Dict exposing (Dict)
import Dict.LocalExtra
import Elm.SemanticMagnitude exposing (Magnitude(..))


type alias Diff value =
    { added : Dict String value
    , changed : Dict String { old : value, new : value }
    , removed : Dict String value
    }


withEquivalence : (( v, v ) -> Bool) -> Dict String v -> Dict String v -> Diff v
withEquivalence areEquivalent old new =
    let
        overlap : Dict String { old : v, new : v }
        overlap =
            Dict.LocalExtra.intersectionWith (\( o, n ) -> { old = o, new = n }) old new

        changed : Dict String { old : v, new : v }
        changed =
            Dict.filter (\_ oldAndNew -> not (areEquivalent ( oldAndNew.old, oldAndNew.new ))) overlap
    in
    { added = Dict.diff new old
    , changed = changed
    , removed = Dict.diff old new
    }


toMagnitude : Diff v_ -> Magnitude
toMagnitude changes =
    if Dict.size changes.removed > 0 || Dict.size changes.changed > 0 then
        Major

    else if Dict.size changes.added > 0 then
        Minor

    else
        Patch
