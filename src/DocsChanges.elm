module DocsChanges exposing (Changes(..), Magnitude(..), ModuleChanges(..), PackageChanges(..), diff, magnitudeToString, moduleChangeToMagnitude, toMagnitude)

{-| Mostly an elm port of <https://github.com/elm/compiler/blob/2f6dd29258e880dbb7effd57a829a0470d8da48b/builder/src/Deps/Diff.hs>
-}

import Dict exposing (Dict)
import Elm.Docs
import Elm.Type
import Set


type PackageChanges
    = PackageChanges
        { modulesAdded : List String
        , modulesChanged : Dict String ModuleChanges
        , modulesRemoved : List String
        }


type ModuleChanges
    = ModuleChanges
        { unions : Changes String Elm.Docs.Union
        , aliases : Changes String Elm.Docs.Alias
        , values : Changes String Elm.Docs.Value
        }


type Changes k v
    = Changes
        { added : Dict k v
        , changed : Dict k ( v, v )
        , removed : Dict k v
        }


magnitudeMaximum : List Magnitude -> Magnitude
magnitudeMaximum =
    \magnitudes ->
        if magnitudes |> List.member Major then
            Major

        else if magnitudes |> List.member Minor then
            Minor

        else
            Patch



-- DIFF


moduleChangeToMagnitude : ModuleChanges -> Magnitude
moduleChangeToMagnitude (ModuleChanges moduleChanges) =
    magnitudeMaximum
        [ changeMagnitude moduleChanges.unions
        , changeMagnitude moduleChanges.aliases
        , changeMagnitude moduleChanges.values
        ]


changeMagnitude : Changes k_ v_ -> Magnitude
changeMagnitude (Changes changes) =
    if Dict.size changes.removed > 0 || Dict.size changes.changed > 0 then
        Major

    else if Dict.size changes.added > 0 then
        Minor

    else
        Patch



-- EQUIVALENCE


dictIntersectionWith : (( a, b ) -> c) -> Dict comparable a -> Dict comparable b -> Dict comparable c
dictIntersectionWith combine firstDict secondDict =
    firstDict
        |> Dict.foldl
            (\key firstValue ->
                case secondDict |> Dict.get key of
                    Nothing ->
                        identity

                    Just secondValue ->
                        Dict.insert key (combine ( firstValue, secondValue ))
            )
            Dict.empty


getChanges : (( v, v ) -> Bool) -> Dict comparable v -> Dict comparable v -> Changes comparable v
getChanges isEquivalent old new =
    let
        overlap : Dict comparable ( v, v )
        overlap =
            dictIntersectionWith identity old new

        changed : Dict comparable ( v, v )
        changed =
            Dict.filter (\_ oldAndNew -> not (isEquivalent oldAndNew)) overlap
    in
    Changes
        { added = Dict.diff new old
        , changed = changed
        , removed = Dict.diff old new
        }


diff : ( List Elm.Docs.Module, List Elm.Docs.Module ) -> PackageChanges
diff ( oldModules, newModules ) =
    let
        (Changes changes) =
            getChanges (\_ -> False)
                (oldModules |> List.map (\moduleDocs -> ( moduleDocs.name, moduleDocs )) |> Dict.fromList)
                (newModules |> List.map (\moduleDocs -> ( moduleDocs.name, moduleDocs )) |> Dict.fromList)
    in
    PackageChanges
        { modulesAdded = Dict.keys changes.added
        , modulesChanged =
            Dict.map (\_ -> diffModule) changes.changed
                |> Dict.filter
                    (\_ change ->
                        case moduleChangeToMagnitude change of
                            Patch ->
                                False

                            _ ->
                                True
                    )
        , modulesRemoved = Dict.keys changes.removed
        }


diffModule : ( Elm.Docs.Module, Elm.Docs.Module ) -> ModuleChanges
diffModule ( oldModule, newModule ) =
    ModuleChanges
        { unions =
            getChanges isEquivalentUnion
                (oldModule.unions |> List.map (\union -> ( union.name, union )) |> Dict.fromList)
                (newModule.unions |> List.map (\union -> ( union.name, union )) |> Dict.fromList)
        , aliases =
            getChanges isEquivalentAlias
                (oldModule.aliases |> List.map (\alias -> ( alias.name, alias )) |> Dict.fromList)
                (newModule.aliases |> List.map (\alias -> ( alias.name, alias )) |> Dict.fromList)
        , values =
            getChanges isEquivalentValue
                (oldModule.values |> List.map (\value -> ( value.name, value )) |> Dict.fromList)
                (newModule.values |> List.map (\value -> ( value.name, value )) |> Dict.fromList)
        }



-- DIFF TYPES


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


diffType : ( Elm.Type.Type, Elm.Type.Type ) -> Maybe (List ( String, String ))
diffType =
    \( oldType, newType ) ->
        case ( oldType, newType ) of
            ( Elm.Type.Var oldName, Elm.Type.Var newName ) ->
                Just [ ( oldName, newName ) ]

            ( Elm.Type.Lambda oldInput oldOutput, Elm.Type.Lambda newInput newOutput ) ->
                Maybe.map2 (\inDiff outDiff -> inDiff ++ outDiff)
                    (diffType ( oldInput, newInput ))
                    (diffType ( oldOutput, newOutput ))

            ( Elm.Type.Type oldName oldArgs, Elm.Type.Type newName newArgs ) ->
                if (oldName /= newName) || List.length oldArgs /= List.length newArgs then
                    Nothing

                else
                    List.map2 (\oldArg newArg -> ( oldArg, newArg ) |> diffType) oldArgs newArgs
                        |> listAllJustMap identity
                        |> Maybe.map List.concat

            ( Elm.Type.Record fields maybeExt, Elm.Type.Record bFields bMaybeExt ) ->
                case ( maybeExt, bMaybeExt ) of
                    ( Nothing, Just _ ) ->
                        Nothing

                    ( Just _, Nothing ) ->
                        Nothing

                    ( Nothing, Nothing ) ->
                        diffFields fields bFields

                    ( Just oldExt, Just newExt ) ->
                        Maybe.map (\fieldDiffs -> ( oldExt, newExt ) :: fieldDiffs)
                            (diffFields fields bFields)

            ( Elm.Type.Tuple [], Elm.Type.Tuple [] ) ->
                Just []

            ( Elm.Type.Tuple [ _, _ ], Elm.Type.Tuple [ _, _, _ ] ) ->
                Nothing

            ( Elm.Type.Tuple [ _, _, _ ], Elm.Type.Tuple [ _, _ ] ) ->
                Nothing

            ( Elm.Type.Tuple [ oldFirst, oldSecond, oldThird ], Elm.Type.Tuple [ newFirst, newSecond, newThird ] ) ->
                Maybe.map3 (\a b c -> a ++ b ++ c)
                    (diffType ( oldFirst, newFirst ))
                    (diffType ( oldSecond, newSecond ))
                    (diffType ( oldThird, newThird ))

            ( Elm.Type.Tuple [ oldFirst, oldSecond ], Elm.Type.Tuple [ newFirst, newSecond ] ) ->
                Maybe.map2 (\a b -> a ++ b)
                    (diffType ( oldFirst, newFirst ))
                    (diffType ( oldSecond, newSecond ))

            ( _, _ ) ->
                Nothing



-- TYPE VARIABLES


isEquivalentTypeWithArgs :
    ( { a_ | tipe : Elm.Type.Type, args : List String }
    , { b_ | tipe : Elm.Type.Type, args : List String }
    )
    -> Bool
isEquivalentTypeWithArgs ( oldTypeAlias, newTypeAlias ) =
    case ( oldTypeAlias.tipe, newTypeAlias.tipe ) |> diffType of
        Nothing ->
            False

        Just renamings ->
            (List.length oldTypeAlias.args == List.length newTypeAlias.args)
                && isEquivalentRenaming (List.map2 Tuple.pair newTypeAlias.args oldTypeAlias.args ++ renamings)


isEquivalentRenaming : List ( String, String ) -> Bool
isEquivalentRenaming varPairs =
    let
        renamings : List ( String, List String )
        renamings =
            Dict.toList
                (List.foldr
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
                    varPairs
                )

        verify : ( a, List b ) -> Maybe ( a, b )
        verify ( old, news ) =
            case news of
                [] ->
                    Nothing

                new :: rest ->
                    if List.all (\restElement -> new == restElement) rest then
                        Just ( old, new )

                    else
                        Nothing
    in
    case renamings |> listAllJustMap verify of
        Nothing ->
            False

        Just verifiedRenamings ->
            List.all compatibleVars verifiedRenamings
                && listAllUnique (List.map Tuple.second verifiedRenamings)


compatibleVars : ( String, String ) -> Bool
compatibleVars ( old, new ) =
    case ( categorizeVar old, categorizeVar new ) of
        ( CompAppend, CompAppend ) ->
            True

        ( Comparable, Comparable ) ->
            True

        ( Appendable, Appendable ) ->
            True

        ( Number, Number ) ->
            True

        ( Number, Comparable ) ->
            True

        ( _, Var ) ->
            True

        ( _, _ ) ->
            False


categorizeVar : String -> TypeVarCategory
categorizeVar name =
    if String.startsWith "compappend" name then
        CompAppend

    else if String.startsWith "comparable" name then
        Comparable

    else if String.startsWith "appendable" name then
        Appendable

    else if String.startsWith "number" name then
        Number

    else
        Var



-- MAGNITUDE


listAllUnique : List comparable_ -> Bool
listAllUnique list =
    List.length list == Set.size (Set.fromList list)


isEquivalentUnion : ( Elm.Docs.Union, Elm.Docs.Union ) -> Bool
isEquivalentUnion ( oldUnion, newUnion ) =
    let
        equiv : ( List Elm.Type.Type, List Elm.Type.Type ) -> Bool
        equiv ( oldTypes, newTypes ) =
            (List.length oldTypes == List.length newTypes)
                && List.all identity
                    (List.map2
                        (\old new -> ( old, new ) |> isEquivalentTypeWithArgs)
                        (List.map (\tipe -> { tipe = tipe, args = oldUnion.args }) oldTypes)
                        (List.map (\tipe -> { tipe = tipe, args = newUnion.args }) newTypes)
                    )
    in
    List.length oldUnion.tags
        == List.length newUnion.tags
        && List.all identity
            (List.map2 (==)
                (List.map Tuple.first oldUnion.tags)
                (List.map Tuple.first newUnion.tags)
            )
        && List.all identity
            (Dict.values
                (dictIntersectionWith equiv
                    (Dict.fromList oldUnion.tags)
                    (Dict.fromList newUnion.tags)
                )
            )


isEquivalentAlias : ( Elm.Docs.Alias, Elm.Docs.Alias ) -> Bool
isEquivalentAlias ( oldTypeAlias, newTypeAlias ) =
    ( oldTypeAlias, newTypeAlias )
        |> isEquivalentTypeWithArgs


isEquivalentValue : ( Elm.Docs.Value, Elm.Docs.Value ) -> Bool
isEquivalentValue ( oldValue, newValue ) =
    ( { tipe = oldValue.tipe, args = [] }
    , { tipe = newValue.tipe, args = [] }
    )
        |> isEquivalentTypeWithArgs


diffFields : List ( String, Elm.Type.Type ) -> List ( String, Elm.Type.Type ) -> Maybe (List ( String, String ))
diffFields oldRawFields newRawFields =
    if List.length oldRawFields /= List.length newRawFields then
        Nothing

    else
        let
            newFields : List ( String, Elm.Type.Type )
            newFields =
                List.sortBy Tuple.first newRawFields

            oldFields : List ( String, Elm.Type.Type )
            oldFields =
                List.sortBy Tuple.first oldRawFields
        in
        if List.any identity (List.map2 (\( a, _ ) ( b, _ ) -> a == b) oldFields newFields) then
            Nothing

        else
            List.map2 (\( _, a ) ( _, b ) -> diffType ( a, b )) oldFields newFields
                |> listAllJustMap identity
                |> Maybe.map List.concat


type TypeVarCategory
    = CompAppend
    | Comparable
    | Appendable
    | Number
    | Var


type Magnitude
    = Major
    | Minor
    | Patch


magnitudeToString : Magnitude -> String
magnitudeToString =
    \magnitude ->
        case magnitude of
            Major ->
                "major"

            Minor ->
                "minor"

            Patch ->
                "patch"


toMagnitude : PackageChanges -> Magnitude
toMagnitude (PackageChanges packageChanges) =
    case packageChanges.modulesRemoved of
        [] ->
            let
                addMag : Magnitude
                addMag =
                    case packageChanges.modulesAdded of
                        [] ->
                            Patch

                        _ :: _ ->
                            Minor
            in
            (addMag
                :: (Dict.values packageChanges.modulesChanged |> List.map moduleChangeToMagnitude)
            )
                |> magnitudeMaximum

        _ :: _ ->
            Major
