module CYOAParser exposing (mainParser, requirementParser)

import CYOA exposing (PowerData, PowerOrBreak(..), Requirement(..), Section, SectionElement(..))
import EnglishNumbers
import Id exposing (Id)
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround


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
            , elements = List.map PowerElement powers
            }
        )
        |= getChompedTrimmed (Parser.Workaround.chompUntilBefore "\n")
        |. Parser.spaces
        |= many nonNameParser
        |. Parser.spaces
        |= many powerOrBreakParser


nonNameParser : Parser String
nonNameParser =
    Parser.Workaround.chompUntilBefore "\n"
        |> getChompedTrimmed
        |> Parser.backtrackable
        |> Parser.andThen
            (\s ->
                if String.startsWith "Name: " s then
                    Parser.problem "Starts with name"

                else
                    Parser.succeed s
            )


powerOrBreakParser : Parser PowerOrBreak
powerOrBreakParser =
    Parser.oneOf
        [ Parser.succeed Break |. Parser.symbol "<br>" |. Parser.spaces
        , Parser.map Power powerParser
        ]


powerParser : Parser PowerData
powerParser =
    Parser.succeed
        (\label cost maybeId replaces requires description ->
            { label = label
            , id = Maybe.withDefault (Id.fromString label) maybeId
            , cost = cost
            , replaces = replaces
            , requires = requires
            , description = description
            }
        )
        |. Parser.token "Name:"
        |. Parser.spaces
        |= getChompedTrimmed (Parser.Workaround.chompUntilBefore " - ")
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
            [ Parser.succeed (Just << Id.fromString)
                |. Parser.token "Id:"
                |. Parser.spaces
                |= getChompedTrimmed (Parser.Workaround.chompUntilBefore "\n")
            , Parser.succeed Nothing
            ]
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed (Just << Id.fromString)
                |. Parser.symbol "(Replaces"
                |. Parser.spaces
                |= getChompedTrimmed (Parser.Workaround.chompUntilBefore ")")
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
                                    |> Result.withDefault (Requirement (Id.fromString cut))
                            )
                )
                |. Parser.token "(Requires"
                |. Parser.spaces
                |= getChompedTrimmed (Parser.Workaround.chompUntilBefore ")")
                |. Parser.token ")"
            , Parser.succeed []
            ]
        |. Parser.spaces
        |= getChompedTrimmed (Parser.Workaround.chompUntilBefore "\n\n")


requirementParser : Parser Requirement
requirementParser =
    Parser.succeed
        (\num reqs ->
            let
                parsedReqs : List (Id PowerOrBreak)
                parsedReqs =
                    reqs
                        |> String.split ","
                        |> List.map (\piece -> Id.fromString (String.trim piece))
            in
            AtLeastXOf num parsedReqs
        )
        |. Parser.symbol "at least"
        |. Parser.spaces
        |= numberParser
        |. Parser.spaces
        |. Parser.symbol "of:"
        |. Parser.spaces
        |= getChompedTrimmed (Parser.Workaround.chompUntilEndOrBefore ")")


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
