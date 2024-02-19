module Elm.Module.Diff exposing (Diff, for, toMagnitude)

{-| Diff an [`Elm.Docs.Module`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Module)

@docs Diff, for, toMagnitude

-}

import Dict exposing (Dict)
import Diff
import Elm.Declaration.Diff
import Elm.Docs
import Elm.SemanticMagnitude


{-| For union `type`, `type alias` and function/value declarations:
Names of removed and added declarations and both declarations if a declaration was changed.
-}
type alias Diff =
    { unions :
        { added : Dict String Elm.Docs.Union
        , changed : Dict String { old : Elm.Docs.Union, new : Elm.Docs.Union }
        , removed : Dict String Elm.Docs.Union
        }
    , aliases :
        { added : Dict String Elm.Docs.Alias
        , changed : Dict String { old : Elm.Docs.Alias, new : Elm.Docs.Alias }
        , removed : Dict String Elm.Docs.Alias
        }
    , values :
        { added : Dict String Elm.Docs.Value
        , changed : Dict String { old : Elm.Docs.Value, new : Elm.Docs.Value }
        , removed : Dict String Elm.Docs.Value
        }
    }


{-| A [`Diff`](#Diff) with everything that changed from `old -> new`
-}
for : { old : Elm.Docs.Module, new : Elm.Docs.Module } -> Diff
for module_ =
    { unions =
        Diff.withEquivalence Elm.Declaration.Diff.unionIsEquivalent
            (module_.old.unions |> List.map (\union -> ( union.name, union )) |> Dict.fromList)
            (module_.new.unions |> List.map (\union -> ( union.name, union )) |> Dict.fromList)
    , aliases =
        Diff.withEquivalence Elm.Declaration.Diff.aliasIsEquivalent
            (module_.old.aliases |> List.map (\alias -> ( alias.name, alias )) |> Dict.fromList)
            (module_.new.aliases |> List.map (\alias -> ( alias.name, alias )) |> Dict.fromList)
    , values =
        Diff.withEquivalence Elm.Declaration.Diff.valueIsEquivalent
            (module_.old.values |> List.map (\value -> ( value.name, value )) |> Dict.fromList)
            (module_.new.values |> List.map (\value -> ( value.name, value )) |> Dict.fromList)
    }


{-| What [`Magnitude`](Elm-SemanticMagnitude#Magnitude) does this [`Diff`](#Diff) have?
-}
toMagnitude : Diff -> Elm.SemanticMagnitude.Magnitude
toMagnitude moduleChanges =
    [ Diff.toMagnitude moduleChanges.unions
    , Diff.toMagnitude moduleChanges.aliases
    , Diff.toMagnitude moduleChanges.values
    ]
        |> List.foldl
            (\next soFar -> ( soFar, next ) |> Elm.SemanticMagnitude.theMoreSevere)
            Elm.SemanticMagnitude.Patch
