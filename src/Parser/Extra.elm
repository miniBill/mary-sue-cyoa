module Parser.Extra exposing (DeadEnd, Extract, errorToHtml, parser, parserAdvanced)

import Html exposing (Html)
import Html.Attributes
import Json.Encode
import List.Extra
import Parser exposing (Problem(..))
import Set


type alias DeadEnd inner problem =
    { inner | row : Int, col : Int, problem : problem }


type alias Output out =
    { text : String -> out
    , colorCaret : out -> out
    , newline : out
    , colorContext : out -> out
    }


type alias Extract inner problem =
    { contextStack :
        DeadEnd inner problem
        -> List { row : Int, col : Int, context : String }
    , problemToString : problem -> Expected
    }


type Expected
    = Expected String
    | Other String


type Line a
    = Line (List a)


parser : Extract {} Problem
parser =
    { contextStack = \_ -> []
    , problemToString = problemToExpected
    }


parserAdvanced :
    Extract
        { contextStack : List { row : Int, col : Int, context : String }
        }
        Problem
parserAdvanced =
    { contextStack = .contextStack
    , problemToString = problemToExpected
    }


errorToHtml :
    Extract inner problem
    -> String
    -> List (DeadEnd inner problem)
    -> List (Html msg)
errorToHtml extract src deadEnds =
    let
        color : String -> Html msg -> Html msg
        color value child =
            Html.span [ Html.Attributes.style "color" value ] [ child ]
    in
    renderError
        { text = Html.text
        , colorContext = color "cyan"
        , colorCaret = color "red"
        , newline = Html.br [] []
        }
        extract
        src
        deadEnds


renderError :
    Output out
    -> Extract inner problem
    -> String
    -> List (DeadEnd inner problem)
    -> List out
renderError output extract src deadEnds =
    let
        lines : List ( Int, String )
        lines =
            src
                |> String.split "\n"
                |> List.indexedMap (\i l -> ( i + 1, l ))
    in
    deadEnds
        |> List.Extra.gatherEqualsBy
            (\{ row, col } -> ( row, col ))
        |> List.concatMap (\line -> deadEndToString output extract lines line)
        |> List.intersperse (Line [ output.newline ])
        |> List.concatMap (\(Line l) -> l)


deadEndToString :
    Output out
    -> Extract inner problem
    -> List ( Int, String )
    -> ( DeadEnd inner problem, List (DeadEnd inner problem) )
    -> List (Line out)
deadEndToString output extract lines ( head, tail ) =
    let
        grouped : List ( List { row : Int, col : Int, context : String }, List problem )
        grouped =
            (head :: tail)
                |> List.Extra.gatherEqualsBy extract.contextStack
                |> List.map
                    (\( ihead, itail ) ->
                        ( extract.contextStack ihead
                        , List.map .problem (ihead :: itail)
                        )
                    )

        sourceFragment : List (Line out)
        sourceFragment =
            formatSourceFragment output { row = head.row, col = head.col } lines

        groupToString :
            ( List { row : Int, col : Int, context : String }, List problem )
            -> List (Line out)
        groupToString ( contextStack, problems ) =
            let
                ( expected, other ) =
                    List.foldl
                        (\problem ( eacc, oacc ) ->
                            case extract.problemToString problem of
                                Expected e ->
                                    ( Set.insert e eacc, oacc )

                                Other o ->
                                    ( eacc, Set.insert o oacc )
                        )
                        ( Set.empty, Set.empty )
                        problems

                groupedExpected : List String
                groupedExpected =
                    case Set.toList expected of
                        [] ->
                            []

                        [ x ] ->
                            [ "Expecting " ++ x ]

                        l ->
                            [ "Expecting one of "
                                ++ String.join ", " l
                            ]

                problemsLines : List (Line out)
                problemsLines =
                    (groupedExpected ++ Set.toList other)
                        |> List.sort
                        |> List.map (\l -> Line [ output.text ("  " ++ l) ])
            in
            if List.isEmpty contextStack then
                problemsLines

            else
                Line
                    [ output.text "- "
                    , output.colorContext (output.text (contextStackToString contextStack))
                    , output.text ":"
                    ]
                    :: problemsLines
    in
    sourceFragment ++ Line [ output.text "" ] :: List.concatMap groupToString grouped


formatSourceFragment : Output a -> { row : Int, col : Int } -> List ( Int, String ) -> List (Line a)
formatSourceFragment cfg head lines =
    let
        line : ( Int, String )
        line =
            lines
                |> List.drop (head.row - 1)
                |> List.head
                |> Maybe.withDefault ( head.row, "" )

        before : List ( Int, String )
        before =
            lines
                |> List.drop (head.row - 3)
                |> List.take 3
                |> List.Extra.takeWhile (\( i, _ ) -> i < head.row)

        after : List ( Int, String )
        after =
            lines
                |> List.drop head.row
                |> List.take 3

        formatLine : ( Int, String ) -> Line a
        formatLine ( row, l ) =
            Line
                [ cfg.text
                    (String.padLeft numLength ' ' (String.fromInt row)
                        ++ "| "
                        ++ l
                    )
                ]

        numLength : Int
        numLength =
            after
                |> List.Extra.last
                |> Maybe.map (\( r, _ ) -> r)
                |> Maybe.withDefault head.row
                |> String.fromInt
                |> String.length

        caret : Line a
        caret =
            Line
                [ cfg.text (String.repeat (numLength + head.col + 1) " ")
                , cfg.colorCaret (cfg.text "^")
                ]
    in
    List.map formatLine before
        ++ formatLine line
        :: caret
        :: List.map formatLine after


contextStackToString : List { row : Int, col : Int, context : String } -> String
contextStackToString frames =
    frames
        |> List.reverse
        |> List.map
            (\{ row, col, context } ->
                context
                    ++ " ("
                    ++ String.fromInt row
                    ++ ":"
                    ++ String.fromInt col
                    ++ ")"
            )
        |> String.join " > "


problemToExpected : Problem -> Expected
problemToExpected problem =
    case problem of
        Expecting x ->
            Expected x

        ExpectingVariable ->
            Expected "a variable"

        ExpectingEnd ->
            Expected "the end"

        ExpectingInt ->
            Expected "an integer"

        ExpectingHex ->
            Expected "an hexadecimal number"

        ExpectingOctal ->
            Expected "an octal number"

        ExpectingBinary ->
            Expected "a binary number"

        ExpectingFloat ->
            Expected "a floating point number"

        ExpectingNumber ->
            Expected "a number"

        ExpectingSymbol s ->
            Expected (Json.Encode.encode 0 (Json.Encode.string s))

        ExpectingKeyword k ->
            Expected (Json.Encode.encode 0 (Json.Encode.string k))

        UnexpectedChar ->
            Other "Unexpected char"

        Problem p ->
            Other p

        BadRepeat ->
            Other "Bad repetition"
