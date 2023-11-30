module CYOAParser exposing (errorToString, mainParser)

import EnglishNumbers
import Parser exposing ((|.), (|=), Parser, Problem(..))
import Types exposing (Power, Requirement(..), Section)


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
        (\label cost maybeId replaces requires description ->
            { label = label
            , id = Maybe.withDefault label maybeId
            , cost = cost
            , replaces = replaces
            , requires = requires
            , description = description
            }
        )
        |. Parser.token "Name: "
        |= Parser.getChompedString (Parser.chompUntil " - ")
        |. Parser.token " - "
        |= Parser.oneOf
            [ Parser.succeed identity
                |. Parser.token "Cost:"
                |. Parser.spaces
                |= Parser.int
            , Parser.succeed negate
                |. Parser.token "Grants:"
                |. Parser.spaces
                |. Parser.token "+"
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
            [ Parser.succeed Just
                |. Parser.symbol "(Replaces "
                |= Parser.getChompedString (Parser.chompUntil ")")
                |. Parser.token ")"
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
                                    cut : String
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
        |= Parser.getChompedString (Parser.chompUntil "\n\n")


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
