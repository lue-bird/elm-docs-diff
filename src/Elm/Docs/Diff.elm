module Elm.Docs.Diff exposing (Diff, for, toMagnitude)

{-| Diff a list of [`Elm.Docs.Module`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Module)s.

@docs Diff, for, toMagnitude

-}

-- Mostly an elm port of <https://github.com/elm/compiler/blob/2f6dd29258e880dbb7effd57a829a0470d8da48b/builder/src/Deps/Diff.hs>

import Dict exposing (Dict)
import Diff
import Elm.Docs
import Elm.Module.Diff
import Elm.SemanticMagnitude exposing (Magnitude)
import Set exposing (Set)


{-| Names of removed and added modules
and an [`Elm.Module.Diff.Diff`](Elm-Module-Diff#Diff) for all modules that were changed.
-}
type alias Diff =
    { modulesAdded : Set String
    , modulesChanged : Dict String Elm.Module.Diff.Diff
    , modulesRemoved : Set String
    }


{-| A [`Diff`](#Diff) with everything that changed from `old -> new`
-}
for : { old : List Elm.Docs.Module, new : List Elm.Docs.Module } -> Diff
for modules =
    let
        changes :
            { added : Dict String Elm.Docs.Module
            , changed : Dict String { old : Elm.Docs.Module, new : Elm.Docs.Module }
            , removed : Dict String Elm.Docs.Module
            }
        changes =
            Diff.withEquivalence (\_ -> False)
                (modules.old |> List.map (\moduleDocs -> ( moduleDocs.name, moduleDocs )) |> Dict.fromList)
                (modules.new |> List.map (\moduleDocs -> ( moduleDocs.name, moduleDocs )) |> Dict.fromList)
    in
    { modulesAdded = Dict.keys changes.added |> Set.fromList
    , modulesChanged =
        Dict.map (\_ -> Elm.Module.Diff.for) changes.changed
            |> Dict.filter
                (\_ change ->
                    case Elm.Module.Diff.toMagnitude change of
                        Elm.SemanticMagnitude.Patch ->
                            False

                        _ ->
                            True
                )
    , modulesRemoved = Dict.keys changes.removed |> Set.fromList
    }


{-| What [`Magnitude`](Elm-SemanticMagnitude#Magnitude) does this [`Diff`](#Diff) have?
-}
toMagnitude : Diff -> Magnitude
toMagnitude packageChanges =
    if packageChanges.modulesRemoved |> Set.isEmpty then
        let
            modulesAddedMagnitude : Magnitude
            modulesAddedMagnitude =
                if packageChanges.modulesAdded |> Set.isEmpty then
                    Elm.SemanticMagnitude.Patch

                else
                    Elm.SemanticMagnitude.Minor
        in
        (modulesAddedMagnitude
            :: (Dict.values packageChanges.modulesChanged
                    |> List.map Elm.Module.Diff.toMagnitude
               )
        )
            |> List.foldl
                (\next soFar -> ( soFar, next ) |> Elm.SemanticMagnitude.theMoreSevere)
                Elm.SemanticMagnitude.Patch

    else
        Elm.SemanticMagnitude.Major
