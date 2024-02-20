module Elm.Type.Diff exposing
    ( areEquivalent
    , Constraint(..), VariableCategory(..), variable, variableCategory
    )

{-| Diff an [`Elm.Type.Type`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Type#Type)

@docs areEquivalent


## variables

@docs Constraint, VariableCategory, variable, variableCategory

-}

import Dict
import Elm.SemanticMagnitude
import Elm.Type
import Set


{-| A normal type variable like `element` or one with a [`Constraint`](#Constraint) like `number`
-}
type VariableCategory
    = VariableConstrained Constraint
    | VariableUnconstrained


{-| Some variables have magic capabilities that only lets a specific set of type match

  - `number-`: `Int` or `Float`
  - `appendable-`: `String` or `List any_`
  - `comparable-`: `number`, `Time.Posix`, `( comparable, comparable )`, `( comparable, comparable, comparable )` or `List comparable`
  - `compappend-`: `appendable` **and** `comparable`

-}
type Constraint
    = CompAppend
    | Comparable
    | Appendable
    | Number


{-| Its [`VariableCategory`](#VariableCategory)
-}
variableCategory : String -> VariableCategory
variableCategory name =
    if String.startsWith "compappend" name then
        CompAppend |> VariableConstrained

    else if String.startsWith "comparable" name then
        Comparable |> VariableConstrained

    else if String.startsWith "appendable" name then
        Appendable |> VariableConstrained

    else if String.startsWith "number" name then
        Number |> VariableConstrained

    else
        VariableUnconstrained


{-| Reminder: Emulates `elm diff` behaviour and therefore explicitly emulates its errors:

  - removing all constraints is considered a patch change,
    even though it removed capabilities and will break your code on upgrade

  - changing constrained variables always trigger a major version bump, with the only exception being
    `number -> comparable` which _should_ actually be major.

  - you will not see minor magnitudes as results because elm considers
    even e.g. `variable -> number` a patch change, even though the resulting type gives you more possibilities.

-}
variable : ( String, String ) -> Elm.SemanticMagnitude.Magnitude
variable ( old, new ) =
    case ( variableCategory old, variableCategory new ) of
        ( VariableConstrained CompAppend, VariableConstrained CompAppend ) ->
            Elm.SemanticMagnitude.Patch

        ( VariableConstrained Comparable, VariableConstrained Comparable ) ->
            Elm.SemanticMagnitude.Patch

        ( VariableConstrained Appendable, VariableConstrained Appendable ) ->
            Elm.SemanticMagnitude.Patch

        ( VariableConstrained Number, VariableConstrained Number ) ->
            Elm.SemanticMagnitude.Patch

        ( VariableConstrained Number, VariableConstrained Comparable ) ->
            Elm.SemanticMagnitude.Patch

        ( _, VariableUnconstrained ) ->
            Elm.SemanticMagnitude.Patch

        ( VariableConstrained Comparable, VariableConstrained CompAppend ) ->
            -- likely an oversight by elm, should be Elm.SemanticMagnitude.Patch
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Comparable, VariableConstrained Number ) ->
            -- likely an oversight by elm, should be Elm.SemanticMagnitude.Patch
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Appendable, VariableConstrained CompAppend ) ->
            -- likely an oversight by elm, should be Elm.SemanticMagnitude.Patch
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Number, VariableConstrained CompAppend ) ->
            -- likely an oversight by elm, should be Elm.SemanticMagnitude.Patch
            Elm.SemanticMagnitude.Major

        ( VariableConstrained CompAppend, VariableConstrained Comparable ) ->
            Elm.SemanticMagnitude.Major

        ( VariableConstrained CompAppend, VariableConstrained Appendable ) ->
            Elm.SemanticMagnitude.Major

        ( VariableConstrained CompAppend, VariableConstrained Number ) ->
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Comparable, VariableConstrained Appendable ) ->
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Appendable, VariableConstrained Comparable ) ->
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Appendable, VariableConstrained Number ) ->
            Elm.SemanticMagnitude.Major

        ( VariableConstrained Number, VariableConstrained Appendable ) ->
            Elm.SemanticMagnitude.Major

        ( VariableUnconstrained, VariableConstrained _ ) ->
            Elm.SemanticMagnitude.Major



--


listAllJustMap : (a -> Maybe b) -> (List a -> Maybe (List b))
listAllJustMap mapToMaybe =
    \list ->
        case list of
            [] ->
                [] |> Just

            head :: tail ->
                case mapToMaybe head of
                    Nothing ->
                        Nothing

                    Just headMapped ->
                        Maybe.map (\tailMapped -> headMapped :: tailMapped)
                            (listAllJustMap mapToMaybe tail)


for : ( Elm.Type.Type, Elm.Type.Type ) -> Maybe (List ( String, String ))
for =
    \( oldType, newType ) ->
        case oldType of
            Elm.Type.Var oldName ->
                case newType of
                    Elm.Type.Var newName ->
                        Just [ ( oldName, newName ) ]

                    _ ->
                        Nothing

            Elm.Type.Lambda oldInput oldOutput ->
                case newType of
                    Elm.Type.Lambda newInput newOutput ->
                        Maybe.map2 (\inDiff outDiff -> inDiff ++ outDiff)
                            (for ( oldInput, newInput ))
                            (for ( oldOutput, newOutput ))

                    _ ->
                        Nothing

            Elm.Type.Type oldName oldArgs ->
                case newType of
                    Elm.Type.Type newName newArgs ->
                        if oldName /= newName then
                            Nothing

                        else if (oldArgs |> List.length) /= (newArgs |> List.length) then
                            Nothing

                        else
                            List.map2 (\oldArg newArg -> ( oldArg, newArg ) |> for) oldArgs newArgs
                                |> listAllJustMap identity
                                |> Maybe.map List.concat

                    _ ->
                        Nothing

            Elm.Type.Record oldFields oldMaybeExt ->
                case newType of
                    Elm.Type.Record newFields newMaybeExt ->
                        case ( oldMaybeExt, newMaybeExt ) of
                            ( Nothing, Just _ ) ->
                                Nothing

                            ( Just _, Nothing ) ->
                                Nothing

                            ( Nothing, Nothing ) ->
                                fieldsDiff { old = oldFields, new = newFields }

                            ( Just oldExt, Just newExt ) ->
                                Maybe.map (\fieldDiffs -> ( oldExt, newExt ) :: fieldDiffs)
                                    (fieldsDiff { old = oldFields, new = newFields })

                    _ ->
                        Nothing

            Elm.Type.Tuple [] ->
                case newType of
                    Elm.Type.Tuple [] ->
                        Just []

                    _ ->
                        Nothing

            Elm.Type.Tuple [ oldInParens ] ->
                case newType of
                    Elm.Type.Tuple [ newInParens ] ->
                        for ( oldInParens, newInParens )

                    _ ->
                        Nothing

            Elm.Type.Tuple [ oldFirst, oldSecond ] ->
                case newType of
                    Elm.Type.Tuple [ newFirst, newSecond ] ->
                        Maybe.map2 (\a b -> a ++ b)
                            (for ( oldFirst, newFirst ))
                            (for ( oldSecond, newSecond ))

                    _ ->
                        Nothing

            Elm.Type.Tuple [ oldFirst, oldSecond, oldThird ] ->
                case newType of
                    Elm.Type.Tuple [ newFirst, newSecond, newThird ] ->
                        Maybe.map3 (\a b c -> [ a, b, c ] |> List.concat)
                            (for ( oldFirst, newFirst ))
                            (for ( oldSecond, newSecond ))
                            (for ( oldThird, newThird ))

                    _ ->
                        Nothing

            Elm.Type.Tuple (_ :: _ :: _ :: _ :: _) ->
                -- impossible
                Nothing


{-| Are both [`Elm.Type.Type`](https://dark.elm.dmy.fr/packages/elm/project-metadata-utils/latest/Elm-Type#Type)s equal?

Put `parameters = []` for value/function/port declaration types
otherwise provide the variable names after the declared name for `type`/`type alias` declarations.

-}
areEquivalent :
    ( { type_ : Elm.Type.Type, parameters : List String }
    , { type_ : Elm.Type.Type, parameters : List String }
    )
    -> Bool
areEquivalent ( oldTypeAlias, newTypeAlias ) =
    case ( oldTypeAlias.type_, newTypeAlias.type_ ) |> for of
        Nothing ->
            False

        Just renamings ->
            ((oldTypeAlias.parameters |> List.length) == (newTypeAlias.parameters |> List.length))
                && isEquivalentRenaming
                    (List.map2 Tuple.pair newTypeAlias.parameters oldTypeAlias.parameters
                        ++ renamings
                    )


isEquivalentRenaming : List ( String, String ) -> Bool
isEquivalentRenaming varPairs =
    let
        renamings : List ( String, List String )
        renamings =
            varPairs
                |> List.foldr
                    (\( old, new ) dict ->
                        Dict.update old
                            (\renamesSoFar ->
                                case renamesSoFar of
                                    Nothing ->
                                        [ new ] |> Just

                                    Just renamesSoFarList ->
                                        new :: renamesSoFarList |> Just
                            )
                            dict
                    )
                    Dict.empty
                |> Dict.toList

        verify : ( a, List b ) -> Maybe ( a, b )
        verify ( old, news ) =
            case news of
                [] ->
                    Nothing

                new :: rest ->
                    if rest |> List.all (\restElement -> new == restElement) then
                        Just ( old, new )

                    else
                        Nothing
    in
    case renamings |> listAllJustMap verify of
        Nothing ->
            False

        Just verifiedRenamings ->
            List.all
                (\v ->
                    case v |> variable of
                        Elm.SemanticMagnitude.Major ->
                            False

                        _ ->
                            True
                )
                verifiedRenamings
                && listAllUnique (List.map Tuple.second verifiedRenamings)


listAllUnique : List comparable_ -> Bool
listAllUnique list =
    List.length list == Set.size (Set.fromList list)


fieldsDiff :
    { old : List ( String, Elm.Type.Type )
    , new : List ( String, Elm.Type.Type )
    }
    -> Maybe (List ( String, String ))
fieldsDiff rawFields =
    if (rawFields.old |> List.length) /= (rawFields.new |> List.length) then
        Nothing

    else
        let
            newFields : List ( String, Elm.Type.Type )
            newFields =
                List.sortBy Tuple.first rawFields.new

            oldFields : List ( String, Elm.Type.Type )
            oldFields =
                List.sortBy Tuple.first rawFields.old
        in
        if List.map2 Tuple.pair oldFields newFields |> List.any (\( ( a, _ ), ( b, _ ) ) -> a == b) then
            Nothing

        else
            List.map2 Tuple.pair oldFields newFields
                |> listAllJustMap (\( ( _, a ), ( _, b ) ) -> for ( a, b ))
                |> Maybe.map List.concat
