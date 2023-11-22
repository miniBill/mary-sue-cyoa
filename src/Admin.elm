module Admin exposing (view)

import CYOAViewer
import Dict exposing (Dict)
import Element exposing (Element, alignRight, alignTop, el, fill, height, inFront, newTabLink, rgb, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import EnglishNumbers
import Parser exposing ((|.), (|=), Parser, Problem(..))
import Password exposing (Password)
import Set
import Theme
import Types exposing (AdminMsg(..), CYOA, CYOAId, Choices(..), InnerAdminModel(..), Power, Requirement(..), Section)
import Url
import Url.Builder


view : { password : Password, cyoas : Dict CYOAId CYOA, inner : InnerAdminModel } -> Element AdminMsg
view admin =
    (case admin.inner of
        Listing ->
            viewAdminList admin.cyoas

        Creating cyoaId ->
            viewCreating cyoaId

        Editing cyoaId old current preview ->
            viewEditing cyoaId old current preview

        Renaming _ _ ->
            [ Theme.centralMessage "branch 'Renaming _ _' not implemented" ]

        Deleting _ ->
            [ Theme.centralMessage "branch 'Deleting _' not implemented" ]
    )
        |> (::) (topRow admin.inner)
        |> Theme.column
            [ Theme.padding
            , width fill
            , height fill
            ]


viewCreating : CYOAId -> List (Element AdminMsg)
viewCreating cyoaId =
    [ Input.text
        [ Background.color Theme.palerViolet
        ]
        { label = Input.labelAbove [] <| text "Id"
        , text = cyoaId
        , placeholder = Just <| Input.placeholder [] <| text "bestest-notebook-ever"
        , onChange = CreatePrepare
        }
    , Theme.button []
        { label = text "Create"
        , onPress = Just <| CreateDo cyoaId
        }
    ]


viewEditing : CYOAId -> String -> String -> Bool -> List (Element AdminMsg)
viewEditing cyoaId old current preview =
    let
        inputBox : Element AdminMsg
        inputBox =
            Input.multiline
                [ alignTop
                , width fill
                , height <| Element.maximum 6969 fill
                , Background.color Theme.palerViolet
                ]
                { label = Input.labelAbove [] <| text "Content"
                , text = current
                , onChange = \newValue -> UpdatePrepare cyoaId old newValue preview
                , placeholder = Nothing
                , spellcheck = True
                }

        parsed : Result (List Parser.DeadEnd) (List Section)
        parsed =
            Parser.run mainParser (current ++ "\n")

        saveButton : Maybe CYOA -> Element AdminMsg
        saveButton cyoa =
            Theme.button []
                { onPress =
                    if old == current then
                        Nothing

                    else
                        Maybe.map (UpdateDo cyoaId) cyoa
                , label = text "Save"
                }

        previewBox : Element msg
        previewBox =
            Theme.column
                [ alignTop
                , width fill
                , height fill
                , spacing 26
                ]
                [ el [] Element.none
                , case parsed of
                    Ok cyoa ->
                        CYOAViewer.view Nothing
                            { choices = Simple Set.empty
                            , data = cyoa
                            }

                    Err e ->
                        errorView e
                ]

        errorView : List Parser.DeadEnd -> Element msg
        errorView e =
            el [ Font.color <| rgb 1 0 0 ] <|
                text <|
                    errorToString e
    in
    [ Theme.row
        [ width fill
        , inFront <|
            Theme.row [ alignRight ]
                [ case parsed of
                    Ok cyoa ->
                        saveButton (Just cyoa)

                    Err e ->
                        if preview then
                            saveButton Nothing

                        else
                            errorView e
                , Theme.button []
                    { onPress = Just <| UpdatePrepare cyoaId old current (not preview)
                    , label = text "Preview"
                    }
                ]
        ]
        [ el [ Font.bold ] <| text <| "Editing " ++ cyoaId
        ]
    , if preview then
        Theme.row [ width fill, height fill ]
            [ inputBox
            , previewBox
            ]

      else
        inputBox
    ]


viewAdminList : Dict CYOAId CYOA -> List (Element AdminMsg)
viewAdminList cyoas =
    cyoas
        |> Dict.toList
        |> List.map
            (\( cyoaId, cyoa ) ->
                let
                    raw : String
                    raw =
                        cyoaToString cyoa

                    url : String
                    url =
                        Url.Builder.absolute [ Url.percentEncode cyoaId ] []
                in
                Theme.row []
                    [ Theme.button []
                        { label = text cyoaId
                        , onPress = Just <| UpdatePrepare cyoaId raw raw False
                        }
                    , text <| "Link:"
                    , newTabLink
                        [ Font.underline
                        , Font.color Theme.violet
                        ]
                        { url = url
                        , label = text <| "https://mary-sue.lamdera.app" ++ url
                        }
                    ]
            )


cyoaToString : CYOA -> String
cyoaToString sections =
    sections
        |> List.map sectionToString
        |> String.join "\n"


sectionToString : Section -> String
sectionToString section =
    let
        blocks : List String
        blocks =
            section.name :: section.description ++ List.map viewPower section.powers
    in
    String.join "\n\n" blocks ++ "\n"


viewPower : Power -> String
viewPower power =
    let
        costString : String
        costString =
            if power.cost < 0 then
                "Grants: +" ++ String.fromInt -power.cost

            else
                "Cost: " ++ String.fromInt power.cost

        idLine : String
        idLine =
            if power.label == power.id then
                ""

            else
                "Id: "
                    ++ power.id
                    ++ "\n"

        requiresLine : String
        requiresLine =
            if List.isEmpty power.requires then
                ""

            else
                "(Requires "
                    ++ String.join " and "
                        (List.map Types.requirementToString power.requires)
                    ++ ")\n"
    in
    "Name: "
        ++ power.label
        ++ " - "
        ++ costString
        ++ " ☐\n"
        ++ idLine
        ++ requiresLine
        ++ power.description


topRow : InnerAdminModel -> Element AdminMsg
topRow inner =
    [ Theme.button []
        { label = text "List"
        , onPress =
            case inner of
                Listing ->
                    Nothing

                _ ->
                    Just List
        }
    , Theme.button []
        { label = text "Create"
        , onPress =
            case inner of
                Creating _ ->
                    Nothing

                _ ->
                    Just <| CreatePrepare ""
        }
    , case inner of
        Editing _ _ _ _ ->
            Theme.button []
                { label = text "Edit"
                , onPress = Nothing
                }

        _ ->
            Element.none
    ]
        |> Theme.row []


mainParser : Parser (List Section)
mainParser =
    Parser.succeed identity
        |= many parseSection
        |. Parser.end


parseSection : Parser Section
parseSection =
    Parser.succeed
        (\name description powers ->
            { name = name
            , description = description
            , powers = powers
            }
        )
        |= Parser.getChompedString (Parser.chompUntil "\n")
        |. Parser.spaces
        |= many nonNameParser
        |. Parser.spaces
        |= many powerParser


nonNameParser : Parser String
nonNameParser =
    Parser.chompUntil "\n"
        |> Parser.getChompedString
        |> Parser.backtrackable
        |> Parser.andThen
            (\s ->
                if String.startsWith "Name: " s then
                    Parser.problem "Starts with name"

                else
                    Parser.succeed s
            )


powerParser : Parser Power
powerParser =
    Parser.succeed
        (\label cost maybeId requires description ->
            { label = label
            , id = Maybe.withDefault label maybeId
            , cost = cost
            , requires = requires
            , description = description
            }
        )
        |. Parser.token "Name: "
        |= Parser.getChompedString (Parser.chompUntil " - ")
        |. Parser.token " - "
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.token "Cost: "
                |= Parser.int
            , Parser.succeed negate
                |. Parser.token "Grants: +"
                |= Parser.int
            ]
        |. Parser.token " ☐"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.token "Id: "
                |= Parser.getChompedString (Parser.chompUntil "\n")
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed
                (\req ->
                    req
                        |> String.split " and "
                        |> List.map String.trim
                        |> List.map
                            (\s ->
                                let
                                    cut =
                                        if String.endsWith "." s then
                                            String.dropRight 1 s

                                        else
                                            s
                                in
                                cut
                                    |> Parser.run requirementParser
                                    |> Result.withDefault (Requirement cut)
                            )
                )
                |. Parser.token "(Requires "
                |= Parser.getChompedString (Parser.chompUntil ")")
                |. Parser.token ")"
            , Parser.succeed []
            ]
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompUntil "\n")


requirementParser : Parser Requirement
requirementParser =
    Parser.succeed (\num reqs -> AtLeastXOf num ((List.map Requirement << String.split ", ") reqs))
        |. Parser.symbol "at least "
        |= numberParser
        |. Parser.symbol " of: "
        |= Parser.getChompedString (Parser.chompUntilEndOr ")")


numberParser : Parser Int
numberParser =
    Parser.getChompedString (Parser.chompWhile Char.isAlphaNum)
        |> Parser.andThen
            (\s ->
                case String.toInt s of
                    Just i ->
                        Parser.succeed i

                    Nothing ->
                        case EnglishNumbers.fromString s of
                            Just i ->
                                Parser.succeed i

                            Nothing ->
                                Parser.problem <| "\"" ++ s ++ "\" is not a valid number"
            )


many : Parser a -> Parser (List a)
many parser =
    Parser.sequence
        { start = ""
        , end = ""
        , trailing = Parser.Optional
        , separator = ""
        , spaces = Parser.spaces
        , item = parser
        }


errorToString : List Parser.DeadEnd -> String
errorToString deadEnds =
    String.join "\n" <|
        "Error:"
            :: List.map deadEndToString deadEnds


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    "At " ++ String.fromInt deadEnd.row ++ ":" ++ String.fromInt deadEnd.col ++ ": " ++ problemToString deadEnd.problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        ExpectingInt ->
            "Expecting int"

        ExpectingHex ->
            "Expecting hex"

        ExpectingOctal ->
            "Expecting octal"

        ExpectingBinary ->
            "Expecting binary"

        ExpectingFloat ->
            "Expecting float"

        ExpectingNumber ->
            "Expecting number"

        ExpectingVariable ->
            "Expecting variable"

        ExpectingSymbol s ->
            "Expecting symbol " ++ s

        ExpectingKeyword k ->
            "Expecting keyword " ++ k

        Expecting e ->
            "Expecting " ++ e

        ExpectingEnd ->
            "Expecting end"

        UnexpectedChar ->
            "Unexpected char"

        Problem p ->
            "Problem: " ++ p

        BadRepeat ->
            "Bad repetition"
