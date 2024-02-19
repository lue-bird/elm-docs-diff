module Elm.SemanticMagnitude exposing (Magnitude(..), theMoreSevere, name)

{-| Severity of a diff

@docs Magnitude, theMoreSevere, name

-}


{-| Choose maximum severity between the two [`Magnitude`](#Magnitude)s

    [ Elm.SemanticMagnitude.Minor
    , Elm.SemanticMagnitude.Major
    , Elm.SemanticMagnitude.Patch
    ]
        |> List.foldl
            (\next soFar -> ( soFar, next ) |> Elm.SemanticMagnitude.theMoreSevere)
            Elm.SemanticMagnitude.Patch
    --> Elm.SemanticMagnitude.Major

-}
theMoreSevere : ( Magnitude, Magnitude ) -> Magnitude
theMoreSevere =
    \magnitudes ->
        case magnitudes of
            ( Major, _ ) ->
                Major

            ( _, Major ) ->
                Major

            ( Minor, Minor ) ->
                Minor

            ( Minor, Patch ) ->
                Minor

            ( Patch, Minor ) ->
                Minor

            ( Patch, Patch ) ->
                Patch


{-| What kind of version bump would be necessary?

  - `Major`: stuff has been deleted or a type has changed
  - `Minor`: stuff has been added and the types still fit
  - `Patch`: no API changes, either implementation or documentation changes

-}
type Magnitude
    = Major
    | Minor
    | Patch


{-| Lowercase name

    "-- This is a "
        ++ (Elm.SemanticMagnitude.Minor |> Elm.SemanticMagnitude.name |> String.toUpper)
        ++ " change. --"
    --> "-- This is a MINOR change. --"

-}
name : Magnitude -> String
name =
    \magnitude ->
        case magnitude of
            Major ->
                "major"

            Minor ->
                "minor"

            Patch ->
                "patch"
