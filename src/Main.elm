module Main exposing (Flags, Model, Msg, Power, Section, main)

import Browser
import Element exposing (Attribute, Element, alignRight, column, el, fill, height, paddingEach, paragraph, rgb, scrollbarY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Parser exposing ((|.), (|=), Parser)
import Theme


type alias Model =
    WebData
        { sections : List Section
        }


type WebData a
    = Loading
    | HttpError Http.Error
    | ParseError String
    | Success a


type alias Section =
    { name : String
    , description : List String
    , powers : List Power
    }


type alias Power =
    { name : String
    , cost : Int
    , description : String
    , selected : Bool
    , requires : List String
    }


type Msg
    = Select String Bool
    | GotRaw (Result Http.Error String)


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view =
            \model ->
                Element.layout [] <|
                    view model
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "./raw.txt"
        , expect = Http.expectString GotRaw
        }
    )


view : Model -> Element Msg
view model =
    case model of
        Loading ->
            text "Loading..."

        HttpError e ->
            text <| Debug.toString e

        ParseError e ->
            text <| Debug.toString e

        Success { sections } ->
            column [ height fill ]
                [ viewScore sections
                , Theme.column
                    [ scrollbarY
                    , paddingEach
                        { top = 0
                        , left = Theme.rythm
                        , right = Theme.rythm
                        , bottom = Theme.rythm
                        }
                    , height fill
                    ]
                    (List.map (viewSection sections) sections)
                ]


viewScore : List Section -> Element Msg
viewScore sections =
    let
        sumBy : (a -> Int) -> List a -> Int
        sumBy f list =
            List.sum (List.map f list)

        sum : Int
        sum =
            sumBy sumSection sections

        sumSection : Section -> Int
        sumSection { powers } =
            sumBy
                (\{ cost, selected } ->
                    if selected then
                        cost

                    else
                        0
                )
                powers

        common : List (Attribute msg)
        common =
            [ Theme.padding
            , Font.bold
            ]
    in
    el
        (if sum > 70 then
            Font.color (rgb 0.7 0 0) :: common

         else
            common
        )
        (text <| "Score " ++ String.fromInt sum ++ "/70")


viewSection : List Section -> Section -> Element Msg
viewSection sections section =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        ]
        (el [ Font.bold ] (text section.name)
            :: List.map (\line -> paragraph [] [ text line ]) section.description
            ++ List.map (viewPower sections) section.powers
        )


viewPower : List Section -> Power -> Element Msg
viewPower sections power =
    let
        missingPrereq : List String
        missingPrereq =
            List.filter (\name -> not <| hasSelected name sections) power.requires

        viewRequirement : String -> Element msg
        viewRequirement requirement =
            el
                [ Font.color <|
                    if List.member requirement missingPrereq then
                        if power.selected then
                            rgb 1 0 0

                        else
                            rgb 0.6 0.4 0

                    else
                        rgb 0.4 0.6 0
                ]
                (text requirement)
    in
    Input.button
        [ Border.width 1
        , Theme.padding
        , width fill
        , Background.color <|
            if power.selected then
                if List.isEmpty missingPrereq then
                    rgb 0.7 1 0.7

                else
                    rgb 1 0.7 0.7

            else if List.isEmpty missingPrereq then
                rgb 0.9 0.9 1

            else
                rgb 0.9 0.9 0.9
        ]
        { onPress = Just <| Select power.name (not power.selected)
        , label =
            Theme.column [ width fill ]
                [ Theme.row [ width fill ]
                    [ el [ Font.bold ] <| text power.name
                    , el [ alignRight ] <|
                        text <|
                            if power.cost >= 0 then
                                "Cost: " ++ String.fromInt power.cost

                            else
                                "Grants: +" ++ String.fromInt -power.cost
                    ]
                , if List.isEmpty power.requires then
                    Element.none

                  else
                    paragraph [ Font.italic ] <|
                        text "Requires: "
                            :: List.intersperse
                                (text " and ")
                                (List.map viewRequirement power.requires)
                , paragraph [] [ text power.description ]
                ]
        }


hasSelected : String -> List Section -> Bool
hasSelected name sections =
    List.any
        (\section ->
            List.any
                (\power ->
                    power.name == name && power.selected
                )
                section.powers
        )
        sections


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotRaw (Err e), _ ) ->
            ( HttpError e, Cmd.none )

        ( GotRaw (Ok raw), _ ) ->
            case Parser.run mainParser raw of
                Err e ->
                    ( ParseError <| Debug.toString e, Cmd.none )

                Ok newModel ->
                    ( Success newModel, Cmd.none )

        ( Select name selected, Success submodel ) ->
            let
                updateSection : Section -> Section
                updateSection section =
                    { section
                        | powers =
                            List.map updatePower section.powers
                    }

                updatePower : Power -> Power
                updatePower power =
                    if power.name == name then
                        { power | selected = selected }

                    else
                        power
            in
            ( Success
                { submodel
                    | sections =
                        List.map updateSection submodel.sections
                }
            , Cmd.none
            )

        ( Select _ _, _ ) ->
            ( model, Cmd.none )


mainParser : Parser { sections : List Section }
mainParser =
    Parser.succeed (\sections -> { sections = sections })
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
        (\name cost requires description ->
            { name = name
            , cost = cost
            , requires = requires
            , description = description
            , selected = False
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
