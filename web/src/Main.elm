module Main exposing (main)

import Browser
import Color
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Elm.Docs
import Elm.Docs.Diff
import Elm.Module.Diff
import Elm.Pretty
import Elm.SemanticMagnitude
import Elm.Syntax.Node
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import Elm.Type
import ElmSyntaxHighlight
import File exposing (File)
import File.Select
import Html
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Pretty
import Set
import Task


type State
    = State
        { earlier : DocsJsonSourceState
        , later : DocsJsonSourceState
        , latestError : Maybe String
        , diffViewKind : DiffViewKind
        }


type alias DocsJsonSourceState =
    { packageAuthor : String
    , packageName : String
    , versionMajor : String
    , versionMinor : String
    , versionPatch : String
    , docs : Maybe (List Elm.Docs.Module)
    }


type Event
    = DocsJsonSourceEvent { earlierOrLater : EarlierOrLater, event : DocsJsonSourceEvent }
    | DiffViewKindSelected DiffViewKind


type EarlierOrLater
    = Earlier
    | Later


type DocsJsonSourceEvent
    = PackageDocsJsonSourceEvent PackageDocsJsonSourceEvent
    | DocsRequested VersionedFullPackageName
    | DocsReceived (Result Http.Error (List Elm.Docs.Module))
    | FilesSelectEvent FilesSelectEvent


type PackageDocsJsonSourceEvent
    = PackageAuthorChanged String
    | PackageNameChanged String
    | VersionMajorChanged String
    | VersionMinorChanged String
    | VersionPatchChanged String


main : Program () State Event
main =
    Browser.document
        { init = \() -> ( initialState, Cmd.none )
        , update = \event state -> state |> reactTo event
        , view = ui
        , subscriptions = subscriptions
        }


initialState : State
initialState =
    State
        { earlier =
            { packageAuthor = "elm"
            , packageName = "http"
            , versionMajor = "1"
            , versionMinor = "0"
            , versionPatch = "0"
            , docs = Nothing
            }
        , later =
            { packageAuthor = "elm"
            , packageName = "http"
            , versionMajor = "2"
            , versionMinor = "0"
            , versionPatch = "0"
            , docs = Nothing
            }
        , latestError = Nothing
        , diffViewKind = DiffViewKindHighlighted
        }


subscriptions : State -> Sub Event
subscriptions =
    \_ ->
        Sub.none


docsJsonDecoder : Json.Decode.Decoder (List Elm.Docs.Module)
docsJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map List.singleton Elm.Docs.decoder
        , Json.Decode.list Elm.Docs.decoder
        ]


reactTo : Event -> (State -> ( State, Cmd Event ))
reactTo event =
    case event of
        DocsJsonSourceEvent docsJsonSource ->
            case docsJsonSource.event of
                PackageDocsJsonSourceEvent packageDocsJsonSource ->
                    \(State state) ->
                        ( (case packageDocsJsonSource of
                            PackageAuthorChanged new ->
                                state
                                    |> earlierOrLaterFieldAlter docsJsonSource.earlierOrLater
                                        (\docsJsonSourceState -> { docsJsonSourceState | packageAuthor = new })

                            PackageNameChanged new ->
                                state
                                    |> earlierOrLaterFieldAlter docsJsonSource.earlierOrLater
                                        (\docsJsonSourceState -> { docsJsonSourceState | packageName = new })

                            VersionMajorChanged new ->
                                state
                                    |> earlierOrLaterFieldAlter docsJsonSource.earlierOrLater
                                        (\docsJsonSourceState -> { docsJsonSourceState | versionMajor = new })

                            VersionMinorChanged new ->
                                state
                                    |> earlierOrLaterFieldAlter docsJsonSource.earlierOrLater
                                        (\docsJsonSourceState -> { docsJsonSourceState | versionMinor = new })

                            VersionPatchChanged new ->
                                state
                                    |> earlierOrLaterFieldAlter docsJsonSource.earlierOrLater
                                        (\docsJsonSourceState -> { docsJsonSourceState | versionPatch = new })
                          )
                            |> State
                        , Cmd.none
                        )

                FilesSelectEvent (FilesDropped file) ->
                    \state ->
                        ( state
                        , file
                            |> File.toString
                            |> Task.perform
                                (\fileString ->
                                    DocsJsonSourceEvent
                                        { earlierOrLater = docsJsonSource.earlierOrLater
                                        , event =
                                            fileString
                                                |> Json.Decode.decodeString docsJsonDecoder
                                                |> Result.mapError (\error -> error |> Json.Decode.errorToString |> Http.BadBody)
                                                |> DocsReceived
                                        }
                                )
                        )

                FilesSelectEvent FilesSelectClicked ->
                    \state ->
                        ( state
                        , File.Select.file [ "application/json" ]
                            (\file ->
                                DocsJsonSourceEvent
                                    { earlierOrLater = docsJsonSource.earlierOrLater
                                    , event = FilesSelectEvent (FilesDropped file)
                                    }
                            )
                        )

                DocsRequested fullPackageName ->
                    \state ->
                        ( state
                        , fetchPackageDocs fullPackageName
                            |> Cmd.map
                                (\result ->
                                    DocsJsonSourceEvent
                                        { earlierOrLater = docsJsonSource.earlierOrLater
                                        , event = result |> DocsReceived
                                        }
                                )
                        )

                DocsReceived (Ok docsReceived) ->
                    \(State state) ->
                        ( state
                            |> earlierOrLaterFieldAlter docsJsonSource.earlierOrLater
                                (\docsJsonSourceState -> { docsJsonSourceState | docs = docsReceived |> Just })
                            |> State
                        , Cmd.none
                        )

                DocsReceived (Err httpError) ->
                    \(State state) ->
                        ( { state | latestError = httpError |> Debug.toString |> Just }
                            |> State
                        , Cmd.none
                        )

        DiffViewKindSelected newDiffViewKind ->
            \(State state) ->
                ( { state | diffViewKind = newDiffViewKind } |> State
                , Cmd.none
                )


earlierOrLaterFieldAlter :
    EarlierOrLater
    -> (field -> field)
    -> ({ r | earlier : field, later : field } -> { r | earlier : field, later : field })
earlierOrLaterFieldAlter earlierOrLater alter =
    case earlierOrLater of
        Earlier ->
            \r -> { r | earlier = r.earlier |> alter }

        Later ->
            \r -> { r | later = r.later |> alter }


fetchPackageDocs : VersionedFullPackageName -> Cmd (Result Http.Error (List Elm.Docs.Module))
fetchPackageDocs =
    \(VersionedFullPackageName fullName) ->
        Http.get
            { url =
                [ -- https://package.elm-lang.org/packages/
                  -- does not respond with a header Access-Control-Allow-Origin, so we can't directly fetch data programmatically
                  -- However, we can go through a proxy like elm.dmy.fr which adds this header
                  "https://elm.dmy.fr/packages/"
                , fullName.author
                , "/"
                , fullName.name
                , "/"
                , fullName.version |> versionToString
                , "/docs.json"
                ]
                    |> String.concat
            , expect = Http.expectJson identity docsJsonDecoder
            }


versionToString : SemanticVersion -> String
versionToString =
    \(SemanticVersion version) ->
        [ version.major, version.minor, version.patch ]
            |> List.map String.fromInt
            |> String.join "."


magnitudeUi : Elm.SemanticMagnitude.Magnitude -> Element event_
magnitudeUi =
    \magnitude ->
        magnitude
            |> Elm.SemanticMagnitude.name
            |> Element.text
            |> Element.el
                [ Element.Font.italic
                , Element.Font.color (magnitude |> magnitudeToColor)
                ]


addedColor : Element.Color
addedColor =
    Element.rgb 0.1 0.9 0.2


removedColor : Element.Color
removedColor =
    Element.rgb 1 0.15 0.25


magnitudeToColor : Elm.SemanticMagnitude.Magnitude -> Element.Color
magnitudeToColor =
    \magnitude ->
        case magnitude of
            Elm.SemanticMagnitude.Patch ->
                Element.rgb 0 0 1

            Elm.SemanticMagnitude.Minor ->
                addedColor

            Elm.SemanticMagnitude.Major ->
                removedColor


ui : State -> Browser.Document Event
ui =
    \(State state) ->
        { title = "elm API diff"
        , body =
            (case Maybe.map2 Tuple.pair state.earlier.docs state.later.docs of
                Just ( earlierDocs, laterDocs ) ->
                    let
                        packageChanges : Elm.Docs.Diff.Diff
                        packageChanges =
                            { old = earlierDocs, new = laterDocs } |> Elm.Docs.Diff.for

                        apiIsUnchanged : Bool
                        apiIsUnchanged =
                            (packageChanges.modulesAdded |> Set.isEmpty)
                                && (packageChanges.modulesRemoved |> Set.isEmpty)
                                && (packageChanges.modulesChanged |> Dict.isEmpty)
                    in
                    if apiIsUnchanged then
                        [ "→ no API changes, so this is a " |> Element.text
                        , Elm.SemanticMagnitude.Patch |> magnitudeUi
                        , " version change" |> Element.text
                        ]
                            |> Element.paragraph []

                    else
                        [ [ [ [ "→ " |> Element.text
                              , packageChanges |> Elm.Docs.Diff.toMagnitude |> magnitudeUi
                              , " version change" |> Element.text
                              ]
                                |> Element.paragraph
                                    [ Element.Font.size 23
                                    ]
                            , diffViewKindUi state.diffViewKind
                            ]
                                |> Element.column
                                    [ Element.spacing 10
                                    , Html.Attributes.style "user-select" "none" |> Element.htmlAttribute
                                    ]
                          , packageChanges |> packageChangesUi state.diffViewKind
                          ]
                            |> Element.column
                                [ Element.spacing 50
                                , Element.height Element.fill
                                , Element.paddingEach { top = 50, left = 80, right = 40, bottom = 20 }
                                ]
                            |> Element.el
                                [ Element.height Element.fill
                                ]
                        ]
                            |> Element.column [ Element.spacing 40 ]

                Nothing ->
                    [ [ [ "earlier" |> header
                        , state.earlier
                            |> docsJsonSourceUi
                            |> Element.map (\event -> DocsJsonSourceEvent { earlierOrLater = Earlier, event = event })
                        ]
                            |> Element.column
                                [ Element.spacing 35
                                , Element.height Element.fill
                                ]
                      , [ "later" |> header
                        , state.later
                            |> docsJsonSourceUi
                            |> Element.map (\event -> DocsJsonSourceEvent { earlierOrLater = Later, event = event })
                        ]
                            |> Element.column
                                [ Element.spacing 35
                                , Element.height Element.fill
                                ]
                      ]
                        |> rowCenteredUi
                            [ Element.spacing 150
                            , Element.height Element.fill
                            ]
                    , case state.latestError of
                        Nothing ->
                            Element.none

                        Just error ->
                            ("🚨 " ++ error)
                                |> Element.text
                                |> Element.el [ Element.Font.color (Element.rgb 1 0 0) ]
                    ]
                        |> Element.column
                            [ Element.spacing 80
                            , Element.paddingEach { top = 50, left = 80, right = 40, bottom = 20 }
                            , Element.height Element.fill
                            , Element.centerX
                            ]
            )
                |> Element.layout
                    [ Element.Background.color (Element.rgb 0 0 0)
                    , Element.Font.color (Element.rgb 1 1 1)
                    ]
                |> List.singleton
        }


interactiveColor : Element.Color
interactiveColor =
    Element.rgb 0.3 0.56 0.9


diffViewKindUi : DiffViewKind -> Element Event
diffViewKindUi selectedDiffViewKind =
    [ [ "👁 " |> Element.text -- 💡🔭
      , ([ "Currently in "
         , case selectedDiffViewKind of
            DiffViewKindHighlighted ->
                "highlighted"

            DiffViewKindChangeLog ->
                "change log"
         , " view. Try also "
         ]
            |> String.concat
        )
            |> Element.text
            |> Element.el [ Element.Font.italic ]
      ]
        |> Element.row []
    , Element.Input.radioRow
        [ Element.alignRight
        , Element.width Element.fill
        , Element.spacing 20
        ]
        { onChange = DiffViewKindSelected
        , options =
            [ ( DiffViewKindHighlighted, "🖍️ highlighted view" ) -- ✨
            , ( DiffViewKindChangeLog, "\u{1FAB5} change log view" )
            ]
                |> List.filter (\( viewKind, _ ) -> viewKind /= selectedDiffViewKind)
                |> List.map
                    (\( diffViewKind, diffViewKindName ) ->
                        Element.Input.optionWith diffViewKind
                            (\optionState ->
                                case optionState of
                                    Element.Input.Idle ->
                                        diffViewKindName
                                            |> Element.text
                                            |> Element.el
                                                [ Element.Font.color interactiveColor ]

                                    Element.Input.Focused ->
                                        diffViewKindName
                                            |> Element.text
                                            |> Element.el
                                                [ Element.Font.color interactiveColor ]

                                    Element.Input.Selected ->
                                        [ "active: " |> Element.text
                                        , diffViewKindName
                                            |> Element.text
                                        ]
                                            |> Element.row []
                            )
                    )
        , selected = selectedDiffViewKind |> Just
        , label = Element.Input.labelHidden "diff view kind"
        }
    ]
        |> Element.row [ Element.spacing 5 ]


rowCenteredUi : List (Element.Attribute event) -> List (Element.Element event) -> Element.Element event
rowCenteredUi attributes =
    \elements ->
        elements
            |> List.indexedMap
                (\index element ->
                    if index == 0 then
                        Element.el
                            [ Element.htmlAttribute (Html.Attributes.style "marginLeft" "auto") ]
                            element

                    else if index == (elements |> List.length) - 1 then
                        Element.el
                            [ Element.htmlAttribute (Html.Attributes.style "marginRight" "auto") ]
                            element

                    else
                        element
                )
            |> Element.row
                attributes


packageChangesUi : DiffViewKind -> (Elm.Docs.Diff.Diff -> Element event_)
packageChangesUi diffViewKind =
    case diffViewKind of
        DiffViewKindHighlighted ->
            packageChangesHighlightedUi

        DiffViewKindChangeLog ->
            packageChangesChangeLogUi


toInlineCodeMarkdown : String -> String
toInlineCodeMarkdown =
    \string -> [ "`", string, "`" ] |> String.concat


packageChangesChangeLogUi : Elm.Docs.Diff.Diff -> Element event_
packageChangesChangeLogUi =
    \diff ->
        [ [ "⿻ " |> Element.text -- 🗒
          , "Ctrl+A then Ctrl+C to yoink"
                |> Element.text
                |> Element.el
                    [ Element.Font.italic
                    ]
          ]
            |> Element.row [ Html.Attributes.style "user-select" "none" |> Element.htmlAttribute ]
        , [ diff.modulesRemoved
                |> Set.toList
                |> List.map
                    (\moduleName ->
                        "  - removed module " ++ (moduleName |> toInlineCodeMarkdown)
                    )
          , diff.modulesAdded
                |> Set.toList
                |> List.map
                    (\moduleName ->
                        "  - added module " ++ (moduleName |> toInlineCodeMarkdown)
                    )
          , diff.modulesChanged |> Dict.toList |> List.map moduleChangesToChangeLogChunk
          ]
            |> List.concat
            |> String.join "\n"
            |> Html.text
            |> List.singleton
            |> Html.pre []
            |> Element.html
        ]
            |> Element.column [ Element.spacing 20 ]


aliasToString : Elm.Docs.Alias -> String
aliasToString =
    \docsAlias ->
        docsAlias
            |> docsAliasToSyntax
            |> Elm.Pretty.prettyTypeAlias
            |> Pretty.pretty 60


docsTypeToSyntaxNode : Elm.Type.Type -> Elm.Syntax.Node.Node Elm.Syntax.TypeAnnotation.TypeAnnotation
docsTypeToSyntaxNode =
    \innerDocsType ->
        innerDocsType |> docsTypeToSyntax |> Elm.Syntax.Node.empty


docsAliasToSyntax : Elm.Docs.Alias -> Elm.Syntax.TypeAlias.TypeAlias
docsAliasToSyntax =
    \docsAlias ->
        { documentation = Nothing
        , name = docsAlias.name |> Elm.Syntax.Node.empty
        , generics = docsAlias.args |> List.map Elm.Syntax.Node.empty
        , typeAnnotation = docsAlias.tipe |> docsTypeToSyntaxNode
        }


valueToString : Elm.Docs.Value -> String
valueToString =
    \docsValue ->
        { name = docsValue.name |> Elm.Syntax.Node.empty
        , typeAnnotation = docsValue.tipe |> docsTypeToSyntaxNode
        }
            |> Elm.Pretty.prettySignature
            |> Pretty.pretty 60


docsUnionToSyntax : Elm.Docs.Union -> Elm.Syntax.Type.Type
docsUnionToSyntax =
    \docsUnion ->
        { documentation = Nothing
        , name = docsUnion.name |> Elm.Syntax.Node.empty
        , generics = docsUnion.args |> List.map Elm.Syntax.Node.empty
        , constructors =
            docsUnion.tags
                |> List.map
                    (\( variantName, variantAttachments ) ->
                        { name = variantName |> Elm.Syntax.Node.empty
                        , arguments = variantAttachments |> List.map docsTypeToSyntaxNode
                        }
                            |> Elm.Syntax.Node.empty
                    )
        }


moduleChangesToChangeLogChunk : ( String, Elm.Module.Diff.Diff ) -> String
moduleChangesToChangeLogChunk ( moduleName, moduleChanges ) =
    let
        changedToItems :
            (v -> String)
            -> Dict String { old : v, new : v }
            -> List String
        changedToItems entryToDoc changed =
            changed
                |> Dict.toList
                |> List.map
                    (\( _, value ) ->
                        [ entryToDoc value.old
                        , "\nto"
                        , entryToDoc value.new
                        ]
                            |> String.concat
                    )

        changesToDoc : String -> List String -> List String -> List String -> Maybe String
        changesToDoc categoryName unions aliases values =
            case ( unions, aliases, values ) of
                ( [], [], [] ) ->
                    Nothing

                ( unionsPossiblyFilled, aliasesPossiblyFilled, valuesPossiblyFilled ) ->
                    ([ unionsPossiblyFilled
                     , aliasesPossiblyFilled
                     , valuesPossiblyFilled
                     ]
                        |> List.concat
                        |> List.map
                            (\item ->
                                [ "  - "
                                , categoryName
                                , " "
                                , item |> indentAfterFirstLine
                                ]
                                    |> String.concat
                            )
                        |> String.join "\n"
                    )
                        |> Just

        unionShortDescription : Elm.Docs.Union -> String
        unionShortDescription =
            \union ->
                case union.tags of
                    [] ->
                        [ "opaque type "
                        , (union.name ++ (union.args |> List.map (\par -> " " ++ par) |> String.join " "))
                            |> toInlineCodeMarkdown
                        ]
                            |> String.concat

                    _ :: _ ->
                        [ "type "
                        , (union.name ++ (union.args |> List.map (\par -> " " ++ par) |> String.join " "))
                            |> toInlineCodeMarkdown
                        , " with exposed variants"
                        ]
                            |> String.concat

        unionToString : Elm.Docs.Union -> String
        unionToString =
            \union ->
                case union.tags of
                    [] ->
                        [ "opaque type "
                        , (union.name ++ (union.args |> List.map (\par -> " " ++ par) |> String.join " "))
                            |> toInlineCodeMarkdown
                        ]
                            |> String.concat

                    _ :: _ ->
                        union
                            |> docsUnionToSyntax
                            |> Elm.Pretty.prettyCustomType
                            |> Pretty.pretty 60
                            |> toElmCodeBlockMarkdown
    in
    [ "  - in module "
    , moduleName |> toInlineCodeMarkdown
    , "\n"
    , [ changesToDoc "removed"
            (moduleChanges.unions.removed |> Dict.values |> List.map unionShortDescription)
            (moduleChanges.aliases.removed |> Dict.keys |> List.map (\aliasName -> "type alias " ++ (aliasName |> toInlineCodeMarkdown)))
            (moduleChanges.values.removed |> Dict.keys |> List.map toInlineCodeMarkdown)
      , changesToDoc "added"
            (moduleChanges.unions.added |> Dict.values |> List.map unionShortDescription)
            (moduleChanges.aliases.added |> Dict.keys |> List.map (\aliasName -> "type alias " ++ (aliasName |> toInlineCodeMarkdown)))
            (moduleChanges.values.added |> Dict.keys |> List.map toInlineCodeMarkdown)
      , changesToDoc "changed"
            (moduleChanges.unions.changed |> changedToItems unionToString)
            (moduleChanges.aliases.changed
                |> changedToItems (\alias -> alias |> aliasToString |> toElmCodeBlockMarkdown)
            )
            (moduleChanges.values.changed
                |> changedToItems (\value -> value |> valueToString |> toElmCodeBlockMarkdown)
            )
      ]
        |> List.filterMap identity
        |> String.join "\n"
        |> indent
    ]
        |> String.concat


toElmCodeBlockMarkdown : String -> String
toElmCodeBlockMarkdown =
    \code ->
        [ "\n```elm\n"
        , code
        , "\n```"
        ]
            |> String.concat


indentAfterFirstLine : String -> String
indentAfterFirstLine =
    \string ->
        case string |> String.split "\n" of
            [] ->
                string

            line0 :: line1Up ->
                line0
                    :: (line1Up |> List.map (\line -> "    " ++ line))
                    |> String.join "\n"


indent : String -> String
indent =
    \string ->
        string
            |> String.split "\n"
            |> List.map (\line -> "    " ++ line)
            |> String.join "\n"


elmCodeUi : ElmSyntaxHighlight.SyntaxHighlightable -> Element event_
elmCodeUi syntaxHighlightable =
    Html.code
        [ Html.Attributes.style "font-size" "0.8em"
        , Html.Attributes.style "line-height" "1.5"
        ]
        [ Html.code []
            (syntaxHighlightable
                |> List.map
                    (\segment ->
                        Html.code
                            (case segment.syntaxKind of
                                Nothing ->
                                    []

                                Just syntaxKind ->
                                    [ Html.Attributes.style "color"
                                        (syntaxKind |> syntaxKindToColor |> Color.toCssString)
                                    ]
                            )
                            [ Html.text segment.string
                            ]
                    )
            )
        ]
        |> Element.html


syntaxKindToColor : ElmSyntaxHighlight.SyntaxKind -> Color.Color
syntaxKindToColor =
    -- light purple Color.rgb 0.97 0.42 1
    \syntaxKind ->
        case syntaxKind of
            ElmSyntaxHighlight.Type ->
                Color.rgb 0.9 0.55 1

            ElmSyntaxHighlight.Variant ->
                Color.rgb 0.24 0.75 0.62

            ElmSyntaxHighlight.Field ->
                Color.rgb 0.4 0.9 0

            ElmSyntaxHighlight.ModuleNameOrAlias ->
                Color.rgb 0.45 0.5 1

            ElmSyntaxHighlight.Variable ->
                Color.rgb 0.85 0.8 0.1

            ElmSyntaxHighlight.Flow ->
                Color.rgb 1 0.45 0.35

            ElmSyntaxHighlight.DeclarationRelated ->
                Color.rgb 0.55 0.75 1


packageChangesHighlightedUi : Elm.Docs.Diff.Diff -> Element event_
packageChangesHighlightedUi =
    \diff ->
        let
            removedChunk : List { title : String, magnitude : Elm.SemanticMagnitude.Magnitude, details : Element Never }
            removedChunk =
                if diff.modulesRemoved |> Set.isEmpty then
                    []

                else
                    [ { title = "removed modules"
                      , magnitude = Elm.SemanticMagnitude.Major
                      , details =
                            diff.modulesRemoved
                                |> Set.toList
                                |> List.map
                                    (\moduleName ->
                                        [ { string = moduleName, syntaxKind = ElmSyntaxHighlight.ModuleNameOrAlias |> Just } ]
                                            |> elmCodeUi
                                    )
                                |> Element.column [ Element.spacing 0 ]
                      }
                    ]

            addedChunk : List { title : String, magnitude : Elm.SemanticMagnitude.Magnitude, details : Element Never }
            addedChunk =
                if diff.modulesAdded |> Set.isEmpty then
                    []

                else
                    [ { title = "added modules"
                      , magnitude = Elm.SemanticMagnitude.Minor
                      , details =
                            diff.modulesAdded
                                |> Set.toList
                                |> List.map
                                    (\moduleName ->
                                        [ { string = moduleName, syntaxKind = ElmSyntaxHighlight.ModuleNameOrAlias |> Just } ]
                                            |> elmCodeUi
                                    )
                                |> Element.column [ Element.spacing 0 ]
                      }
                    ]
        in
        [ removedChunk
        , addedChunk
        , diff.modulesChanged |> Dict.toList |> List.map moduleChangesToChunk
        ]
            |> List.concat
            |> List.map chunkUi
            |> Element.column
                [ Element.spacing 40
                , Element.height Element.fill
                ]


chunkUi : { title : String, magnitude : Elm.SemanticMagnitude.Magnitude, details : Element Never } -> Element event_
chunkUi =
    \chunk ->
        [ [ (chunk.title ++ " ") |> Element.text
          , chunk.magnitude |> magnitudeUi
          ]
            |> Element.paragraph [ Element.Font.size 23, Element.Font.bold ]
        , chunk.details
            |> Element.map Basics.never
            |> Element.el [ Element.paddingXY 20 30 ]
        ]
            |> Element.column []


moduleChangesToChunk : ( String, Elm.Module.Diff.Diff ) -> { title : String, magnitude : Elm.SemanticMagnitude.Magnitude, details : Element Never }
moduleChangesToChunk ( moduleName, moduleChanges ) =
    let
        magnitude : Elm.SemanticMagnitude.Magnitude
        magnitude =
            moduleChanges |> Elm.Module.Diff.toMagnitude

        changesUiTriple :
            (v -> ElmSyntaxHighlight.SyntaxHighlightable)
            ->
                { added : Dict String v
                , changed : Dict String { old : v, new : v }
                , removed : Dict String v
                }
            -> ( List (Element Never), List (Element Never), List (Element Never) )
        changesUiTriple entryUi changes =
            let
                diffed : { old : v, new : v } -> Element Never
                diffed value =
                    [ [ "–"
                            |> Element.text
                            |> Element.el
                                [ Element.alignTop
                                , Element.width (Element.px 35)
                                , Element.Font.color removedColor
                                , Element.alignLeft
                                , Element.Font.extraBold
                                , Element.Font.size 27
                                , Element.Font.family [ Element.Font.monospace ]
                                ]
                      , entryUi value.old |> elmCodeUi
                      ]
                        |> Element.row [ Element.spacing 10 ]
                    , [ "+"
                            |> Element.text
                            |> Element.el
                                [ Element.alignTop
                                , Element.width (Element.px 35)
                                , Element.Font.color addedColor
                                , Element.Font.size 23
                                , Element.Font.family [ Element.Font.monospace ]
                                ]
                      , entryUi value.new |> elmCodeUi
                      ]
                        |> Element.row [ Element.spacing 10 ]
                    ]
                        |> Element.column [ Element.spacing 5 ]
            in
            ( changes.added
                |> Dict.toList
                |> List.map (\( _, value ) -> entryUi value |> elmCodeUi)
            , changes.changed |> Dict.toList |> List.map (\( _, value ) -> diffed value)
            , changes.removed
                |> Dict.toList
                |> List.map (\( _, value ) -> entryUi value |> elmCodeUi)
            )

        ( unionAdd, unionChange, unionRemove ) =
            moduleChanges.unions
                |> changesUiTriple (\docsUnion -> docsUnion |> unionToSyntaxHighlightable)

        ( aliasAdd, aliasChange, aliasRemove ) =
            moduleChanges.aliases
                |> changesUiTriple (\docsAlias -> docsAlias |> aliasToString |> ElmSyntaxHighlight.for)

        ( valueAdd, valueChange, valueRemove ) =
            moduleChanges.values
                |> changesUiTriple (\docsValue -> docsValue |> valueToString |> ElmSyntaxHighlight.for)
    in
    { title = moduleName
    , magnitude = magnitude
    , details =
        let
            changesUi : ( String, Element.Color ) -> List (Element Never) -> List (Element Never) -> List (Element Never) -> Maybe (Element Never)
            changesUi ( categoryName, categoryColor ) unions aliases values =
                case ( unions, aliases, values ) of
                    ( [], [], [] ) ->
                        Nothing

                    ( unionsPossiblyFilled, aliasesPossiblyFilled, valuesPossiblyFilled ) ->
                        Just
                            ([ categoryName
                                |> Element.text
                                |> Element.el
                                    [ Element.Font.size 24
                                    , Element.Font.bold
                                    , Element.Font.color categoryColor
                                    ]
                             , [ case unionsPossiblyFilled of
                                    [] ->
                                        Element.none

                                    union0 :: union1Up ->
                                        (union0 :: union1Up) |> Element.column [ Element.spacing 25 ]
                               , case aliasesPossiblyFilled of
                                    [] ->
                                        Element.none

                                    alias0 :: alias1Up ->
                                        (alias0 :: alias1Up)
                                            |> List.map (\el -> el |> Element.el [ Element.paddingEach { left = 0, right = 0, top = 0, bottom = 0 } ])
                                            |> Element.column [ Element.spacing 14 ]
                               , case valuesPossiblyFilled of
                                    [] ->
                                        Element.none

                                    value0 :: value1Up ->
                                        (value0 :: value1Up) |> Element.column [ Element.spacing 25 ]
                               ]
                                |> Element.column
                                    [ Element.spacing 33
                                    , Element.paddingEach { left = 40, right = 5, top = 20, bottom = 40 }
                                    ]
                             ]
                                |> Element.column [ Element.spacing 0 ]
                            )
        in
        [ changesUi ( "removed", removedColor ) unionRemove aliasRemove valueRemove
        , changesUi ( "changed", changedColor ) unionChange aliasChange valueChange
        , changesUi ( "added", addedColor ) unionAdd aliasAdd valueAdd
        ]
            |> List.filterMap identity
            |> Element.column [ Element.spacing 10 ]
    }


changedColor : Element.Color
changedColor =
    Element.rgb 0.9 0.8 0.1


unionToSyntaxHighlightable : Elm.Docs.Union -> ElmSyntaxHighlight.SyntaxHighlightable
unionToSyntaxHighlightable union =
    case union.tags of
        [] ->
            [ { string = "type", syntaxKind = ElmSyntaxHighlight.DeclarationRelated |> Just }
            , { string = " ", syntaxKind = Nothing }
            , { string = union.name, syntaxKind = ElmSyntaxHighlight.Type |> Just }
            , { string = " ", syntaxKind = Nothing }
            ]
                ++ (union.args
                        |> List.map (\attachment -> { string = attachment, syntaxKind = ElmSyntaxHighlight.Variable |> Just })
                        |> List.intersperse { string = " ", syntaxKind = Nothing }
                   )

        _ :: _ ->
            union
                |> docsUnionToSyntax
                |> Elm.Pretty.prettyCustomType
                |> Pretty.pretty 60
                |> ElmSyntaxHighlight.for


header : String -> Element event_
header text =
    text
        |> Element.text
        |> Element.el [ Element.Font.bold, Element.Font.size 24 ]


actionUi : String -> Element ()
actionUi label =
    Element.Input.button
        [ Element.Background.color (Element.rgba 0 0 0 0)
        , Element.Font.color interactiveColor
        ]
        { label = label |> Element.text
        , onPress = () |> Just
        }


docsJsonSourceUi : DocsJsonSourceState -> Element DocsJsonSourceEvent
docsJsonSourceUi =
    \state ->
        case state.docs of
            Just _ ->
                "✔️ docs.json added" |> Element.text

            Nothing ->
                [ [ [ [ "package" |> Element.text
                      , [ textInputUi { state = state.packageAuthor, label = "author" }
                            |> Element.map PackageAuthorChanged
                        , textInputUi { state = state.packageName, label = "name" }
                            |> Element.map PackageNameChanged
                        ]
                            |> Element.row [ Element.spacing 7 ]
                      ]
                        |> Element.column []
                    , [ "version" |> Element.text
                      , [ textInputUi { state = state.versionMajor, label = "major" }
                            |> Element.map VersionMajorChanged
                        , textInputUi { state = state.versionMinor, label = "minor" }
                            |> Element.map VersionMinorChanged
                        , textInputUi { state = state.versionPatch, label = "patch" }
                            |> Element.map VersionPatchChanged
                        ]
                            |> Element.row [ Element.spacing 7 ]
                      ]
                        |> Element.column []
                    ]
                        |> Element.row
                            [ Element.spacing 35
                            ]
                        |> Element.map PackageDocsJsonSourceEvent
                  , case { major = state.versionMajor, minor = state.versionMinor, patch = state.versionPatch } |> semanticVersionFromStrings of
                        Err error ->
                            error |> Element.text

                        Ok version ->
                            actionUi "🌐 fetch"
                                -- ⬇ 🢃
                                |> Element.map
                                    (\() ->
                                        VersionedFullPackageName
                                            { author = state.packageAuthor
                                            , name = state.packageName
                                            , version = version
                                            }
                                            |> DocsRequested
                                    )
                  ]
                    |> Element.column
                        [ Element.spacing 25
                        ]
                , [ "or" |> Element.text
                  , filesSelectUi |> Element.map FilesSelectEvent
                  ]
                    |> Element.row [ Element.spacing 18 ]
                ]
                    |> Element.column
                        [ Element.spacing 35
                        ]


textInputUi : { state : String, label : String } -> Element String
textInputUi config =
    -- wrapped in a row as a trick to make the width scale with the input
    -- thanks: https://github.com/bburdette/elm-ui-examples/blob/master/textinput-sized-to-contents/src/Main.elm
    [ config.label
        |> Element.text
        |> Element.el
            [ Element.Font.size 14
            , Element.Font.family [ Element.Font.sansSerif ]
            ]
    , Element.row
        [ Element.inFront
            (Element.Input.text
                [ Element.width Element.fill
                , Element.Border.color (Element.rgba 0 0 0 0)
                , Element.paddingXY 0 4
                , Element.Font.color interactiveColor
                , Element.Border.rounded 0
                , Element.Background.color (Element.rgb 0 0 0)
                ]
                { onChange = identity
                , text = config.state
                , placeholder = Nothing
                , label =
                    Element.Input.labelHidden config.label
                }
            )
        , Element.paddingXY 10 0
        ]
        [ config.state
            |> Element.text
            |> Element.el
                [ Element.Font.color (Element.rgba 0 0 0 0)
                , Html.Attributes.style "user-select" "none" |> Element.htmlAttribute
                ]
        ]
        |> Element.el
            [ Element.Font.size 20
            , Element.Font.family [ Element.Font.monospace ]
            ]
    ]
        |> Element.column
            [ Element.spacing 5
            , Element.alignLeft
            ]


semanticVersionFromStrings : { major : String, minor : String, patch : String } -> Result String SemanticVersion
semanticVersionFromStrings =
    \raw ->
        case ( raw.major |> String.toInt, ( raw.minor |> String.toInt, raw.patch |> String.toInt ) ) of
            ( Just versionMajor, ( Just versionMinor, Just versionPatch ) ) ->
                SemanticVersion { major = versionMajor, minor = versionMinor, patch = versionPatch }
                    |> Ok

            _ ->
                "version parts need to be natural numbers" |> Err


filesSelectUi : Element FilesSelectEvent
filesSelectUi =
    actionUi "📁 select docs.json file"
        |> Element.map (\() -> FilesSelectClicked)
        |> Element.el
            [ Html.Events.preventDefaultOn "drop"
                (Json.Decode.map (\files -> ( files |> FilesDropped, True ))
                    dropFilesJsonDecoder
                )
                |> Element.htmlAttribute
            ]


dropFilesJsonDecoder : Json.Decode.Decoder File
dropFilesJsonDecoder =
    Json.Decode.at [ "dataTransfer", "files" ]
        (Json.Decode.oneOrMore (\head _ -> head)
            File.decoder
        )


type DiffViewKind
    = DiffViewKindHighlighted
    | DiffViewKindChangeLog


type VersionedFullPackageName
    = VersionedFullPackageName { author : String, name : String, version : SemanticVersion }


type SemanticVersion
    = SemanticVersion { major : Int, minor : Int, patch : Int }


docsTypeToSyntax : Elm.Type.Type -> Elm.Syntax.TypeAnnotation.TypeAnnotation
docsTypeToSyntax =
    \tipe ->
        case tipe of
            Elm.Type.Var name ->
                Elm.Syntax.TypeAnnotation.GenericType name

            Elm.Type.Lambda input output ->
                Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation (input |> docsTypeToSyntaxNode) (output |> docsTypeToSyntaxNode)

            Elm.Type.Tuple [] ->
                Elm.Syntax.TypeAnnotation.Unit

            Elm.Type.Tuple parts ->
                Elm.Syntax.TypeAnnotation.Tupled (parts |> List.map docsTypeToSyntaxNode)

            Elm.Type.Type unqualifiedName arguments ->
                Elm.Syntax.TypeAnnotation.Typed
                    (( [], unqualifiedName ) |> Elm.Syntax.Node.empty)
                    (arguments |> List.map docsTypeToSyntaxNode)

            Elm.Type.Record fields (Just extendedRecordVariableName) ->
                Elm.Syntax.TypeAnnotation.GenericRecord
                    (extendedRecordVariableName |> Elm.Syntax.Node.empty)
                    (fields
                        |> List.map
                            (\( fieldName, fieldValue ) ->
                                ( fieldName |> Elm.Syntax.Node.empty, fieldValue |> docsTypeToSyntaxNode )
                                    |> Elm.Syntax.Node.empty
                            )
                        |> Elm.Syntax.Node.empty
                    )

            Elm.Type.Record fields Nothing ->
                Elm.Syntax.TypeAnnotation.Record
                    (fields
                        |> List.map
                            (\( fieldName, fieldValue ) ->
                                ( fieldName |> Elm.Syntax.Node.empty, fieldValue |> docsTypeToSyntaxNode )
                                    |> Elm.Syntax.Node.empty
                            )
                    )


type FilesSelectEvent
    = FilesDropped File
    | FilesSelectClicked
