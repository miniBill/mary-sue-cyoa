module CYOAParser exposing (mainParser, requirementParser)

import EnglishNumbers
import Parser exposing ((|.), (|=), Parser)
import Types exposing (Power, Requirement(..), Section)


mainParser : Parser (List Section)
mainParser =
    Parser.succeed identity
        |= many parseSection
        |. Parser.end


getChompedTrimmed : Parser a -> Parser String
getChompedTrimmed parser =
    Parser.map String.trim <| Parser.getChompedString parser


parseSection : Parser Section
parseSection =
    Parser.succeed
        (\name description powers ->
            { name = name
            , description = description
            , powers = powers
            }
        )
        |= getChompedTrimmed (Parser.chompUntil "\n")
        |. Parser.spaces
        |= many nonNameParser
        |. Parser.spaces
        |= many powerOrBreakParser


nonNameParser : Parser String
nonNameParser =
    Parser.chompUntil "\n"
        |> getChompedTrimmed
        |> Parser.backtrackable
        |> Parser.andThen
            (\s ->
                if String.startsWith "Name: " s then
                    Parser.problem "Starts with name"

                else
                    Parser.succeed s
            )


powerOrBreakParser : Parser (Maybe Power)
powerOrBreakParser =
    Parser.oneOf
        [ Parser.succeed Nothing |. Parser.symbol "<br>" |. Parser.spaces
        , Parser.map Just powerParser
        ]


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
        |. Parser.token "Name:"
        |. Parser.spaces
        |= getChompedTrimmed (Parser.chompUntil " - ")
        |. Parser.token " - "
        |. Parser.spaces
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
        |. Parser.spaces
        |. Parser.token "☐"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.token "Id:"
                |. Parser.spaces
                |= getChompedTrimmed (Parser.chompUntil "\n")
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.symbol "(Replaces"
                |. Parser.spaces
                |= getChompedTrimmed (Parser.chompUntil ")")
                |. Parser.token ")"
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed
                (\req ->
                    req
                        |> String.split ", and"
                        -- |> List.concatMap (String.split ", ")
                        |> List.concatMap (String.split " and ")
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
                |. Parser.token "(Requires"
                |. Parser.spaces
                |= getChompedTrimmed (Parser.chompUntil ")")
                |. Parser.token ")"
            , Parser.succeed []
            ]
        |. Parser.spaces
        |= getChompedTrimmed (Parser.chompUntil "\n\n")


requirementParser : Parser Requirement
requirementParser =
    Parser.succeed (\num reqs -> AtLeastXOf num ((List.map (Requirement << String.trim) << String.split ",") reqs))
        |. Parser.symbol "at least"
        |. Parser.spaces
        |= numberParser
        |. Parser.spaces
        |. Parser.symbol "of:"
        |. Parser.spaces
        |= getChompedTrimmed (Parser.chompUntilEndOr ")")


numberParser : Parser Int
numberParser =
    getChompedTrimmed (Parser.chompWhile Char.isAlphaNum)
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
