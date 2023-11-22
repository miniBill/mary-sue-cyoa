module Admin exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, alignRight, el, fill, height, inFront, newTabLink, rgb, text, width)
import Element.Font as Font
import Element.Input as Input
import Parser exposing ((|.), (|=), Parser, Problem(..))
import Password exposing (Password)
import Theme
import Types exposing (AdminMsg(..), CYOA, CYOAId, InnerAdminModel(..), Power, Section)
import Url
import Url.Builder


view : { password : Password, cyoas : Dict CYOAId CYOA, inner : InnerAdminModel } -> Element AdminMsg
view admin =
    (case admin.inner of
        Listing ->
            viewAdminList admin.cyoas

        Creating cyoaId ->
            viewCreating cyoaId

        Editing cyoaId old current ->
            viewEditing cyoaId old current

        Renaming _ _ ->
            [ Theme.centralMessage "branch 'Renaming _ _' not implemented" ]

        Deleting _ ->
            [ Theme.centralMessage "branch 'Deleting _' not implemented" ]
    )
        |> (::) (topRow admin.inner)
        |> Theme.column [ Theme.padding, width fill, height fill ]


viewCreating : CYOAId -> List (Element AdminMsg)
viewCreating cyoaId =
    [ Input.text []
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


viewEditing : CYOAId -> String -> String -> List (Element AdminMsg)
viewEditing cyoaId old current =
    [ Theme.row
        [ width fill
        , inFront <|
            el [ alignRight ] <|
                case Parser.run mainParser (current ++ "\n") of
                    Ok cyoa ->
                        Theme.button []
                            { onPress =
                                if old == current then
                                    Nothing

                                else
                                    Just <| UpdateDo cyoaId cyoa
                            , label = text "Save"
                            }

                    Err e ->
                        el [ Font.color <| rgb 1 0 0 ] <|
                            text <|
                                errorToString e
        ]
        [ el [ Font.bold ] <| text <| "Editing " ++ cyoaId
        ]
    , Input.multiline [ width fill ]
        { label = Input.labelAbove [] <| text "Content"
        , text = current
        , onChange = UpdatePrepare cyoaId old
        , placeholder = Nothing
        , spellcheck = True
        }
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
                        , onPress = Just <| UpdatePrepare cyoaId raw raw
                        }
                    , text <| "Link:"
                    , newTabLink [ Font.color <| rgb 0.2 0.5 0.2 ]
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
                "(Requires " ++ String.join " and " power.requires ++ ")\n"
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
        Editing _ _ _ ->
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
                                if String.endsWith "." s then
                                    String.dropRight 1 s

                                else
                                    s
                            )
                )
                |. Parser.token "(Requires "
                |= Parser.getChompedString (Parser.chompUntil ")")
                |. Parser.token ")"
            , Parser.succeed []
            ]
        |. Parser.spaces
        |= Parser.getChompedString (Parser.chompUntil "\n")


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
