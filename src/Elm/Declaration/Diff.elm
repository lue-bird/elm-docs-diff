module Elm.Declaration.Diff exposing (aliasesAreEquivalent, unionsAreEquivalent, valuesAreEquivalent)

{-| Diff a declaration

@docs aliasesAreEquivalent, unionsAreEquivalent, valuesAreEquivalent

-}

import Dict
import Dict.LocalExtra
import Elm.Docs
import Elm.Type
import Elm.Type.Diff


{-| Are both [`Elm.Docs.Union`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Union) type declarations equal?

Reminder: Emulates `elm diff` behaviour and therefore explicitly e.g. says exposing the variants necessitates a major version bump.

-}
unionsAreEquivalent : ( Elm.Docs.Union, Elm.Docs.Union ) -> Bool
unionsAreEquivalent ( oldUnion, newUnion ) =
    let
        variantAttachmentTypesAreEquivalent : ( List Elm.Type.Type, List Elm.Type.Type ) -> Bool
        variantAttachmentTypesAreEquivalent =
            \( oldTypes, newTypes ) ->
                (List.length oldTypes == List.length newTypes)
                    && (List.map2 Tuple.pair
                            (oldTypes |> List.map (\type_ -> { type_ = type_, parameters = oldUnion.args }))
                            (newTypes |> List.map (\type_ -> { type_ = type_, parameters = newUnion.args }))
                            |> List.all Elm.Type.Diff.areEquivalent
                       )
    in
    ((oldUnion.tags |> List.length) == (newUnion.tags |> List.length))
        && (List.map2 Tuple.pair
                (List.map Tuple.first oldUnion.tags)
                (List.map Tuple.first newUnion.tags)
                |> List.all (\( old, new ) -> old == new)
           )
        && (Dict.LocalExtra.intersectionWith identity
                (oldUnion.tags |> Dict.fromList)
                (newUnion.tags |> Dict.fromList)
                |> Dict.values
                |> List.all variantAttachmentTypesAreEquivalent
           )


{-| Are both [`Elm.Docs.Value`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Alias)/function declarations equal?
-}
valuesAreEquivalent : ( Elm.Docs.Value, Elm.Docs.Value ) -> Bool
valuesAreEquivalent ( oldValue, newValue ) =
    ( { type_ = oldValue.tipe, parameters = [] }
    , { type_ = newValue.tipe, parameters = [] }
    )
        |> Elm.Type.Diff.areEquivalent


{-| Are both type [`Elm.Docs.Alias`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Docs#Alias) declarations equal?
-}
aliasesAreEquivalent : ( Elm.Docs.Alias, Elm.Docs.Alias ) -> Bool
aliasesAreEquivalent ( oldTypeAlias, newTypeAlias ) =
    ( { type_ = oldTypeAlias.tipe, parameters = oldTypeAlias.args }
    , { type_ = newTypeAlias.tipe, parameters = newTypeAlias.args }
    )
        |> Elm.Type.Diff.areEquivalent
