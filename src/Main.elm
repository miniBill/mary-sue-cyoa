module Main exposing (Choices, Flags, Model, Msg, Power, Section, Tier, WebData, main)

import AppUrl
import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element exposing (Attribute, Color, Element, alignRight, column, el, fill, height, paddingEach, paragraph, rgb, rgb255, row, scrollbarY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Theme
import Url exposing (Url)
import Url.Builder


type alias Model =
    { key : Key
    , choices : Choices
    , data : WebData (List Section)
    }


type Choices
    = Tiered (Dict String Tier)
    | Simple (Set String)


type Tier
    = S
    | A
    | B
    | C
    | D
    | F


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
    , requires : List String
    }


type Msg
    = ChooseTier String (Maybe Tier)
    | GotRaw (Result Http.Error String)
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | ToggleKind Kind


type Kind
    = TieredKind
    | SimpleKind


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "Mary Sue CYOA"
                , body =
                    [ Element.layout [] <|
                        view model
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    ( initialModel url key
    , Http.get
        { url = "./raw.txt"
        , expect = Http.expectString GotRaw
        }
    )


initialModel : Url -> Key -> Model
initialModel url key =
    { key = key
    , choices = urlToChoices url
    , data = Loading
    }


urlToChoices : Url -> Choices
urlToChoices url =
    let
        list : List ( String, List String )
        list =
            (AppUrl.fromUrl url).queryParameters
                |> Dict.toList

        tiered : Maybe (List ( String, Tier ))
        tiered =
            list
                |> Maybe.Extra.traverse
                    (\( key, tier ) ->
                        Maybe.map (Tuple.pair key) (tierFromString tier)
                    )
    in
    case tiered of
        Just t ->
            t
                |> Dict.fromList
                |> Tiered

        Nothing ->
            list
                |> List.map Tuple.first
                |> Set.fromList
                |> Simple


tierFromString : List String -> Maybe Tier
tierFromString strings =
    case strings of
        [ "S" ] ->
            Just S

        [ "A" ] ->
            Just A

        [ "B" ] ->
            Just B

        [ "C" ] ->
            Just C

        [ "D" ] ->
            Just D

        [ "F" ] ->
            Just F

        _ ->
            Nothing


choicesToUrl : Choices -> String
choicesToUrl selected =
    case selected of
        Tiered tiers ->
            tiers
                |> Dict.toList
                |> List.map (\( key, value ) -> Url.Builder.string key <| tierToString value)
                |> Url.Builder.absolute []

        Simple choices ->
            choices
                |> Set.toList
                |> List.map (\key -> Url.Builder.string key "Y")
                |> Url.Builder.absolute []


tierToString : Tier -> String
tierToString tier =
    case tier of
        S ->
            "S"

        A ->
            "A"

        B ->
            "B"

        C ->
            "C"

        D ->
            "D"

        F ->
            "F"


tierToColor : Tier -> Color
tierToColor tier =
    case tier of
        S ->
            rgb255 0x02 0xAF 0xF0

        A ->
            rgb255 0x00 0xAE 0x50

        B ->
            rgb255 0x92 0xCF 0x50

        C ->
            rgb255 0xFE 0xD9 0x66

        D ->
            rgb255 0xF7 0x86 0x1C

        F ->
            rgb255 0xAC 0x00 0x00


view : Model -> Element Msg
view model =
    case model.data of
        Loading ->
            text "Loading..."

        HttpError e ->
            text <| Debug.toString e

        ParseError e ->
            text <| Debug.toString e

        Success sections ->
            column [ height fill ]
                [ row [ Theme.padding, width fill ]
                    [ viewScore model.choices sections
                    , viewToggle model.choices
                    ]
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
                    (List.map
                        (viewSection model.choices)
                        sections
                    )
                ]


viewToggle : Choices -> Element Msg
viewToggle choices =
    { onChange = ToggleKind
    , label = Input.labelHidden "Kind"
    , selected =
        case choices of
            Tiered _ ->
                Just TieredKind

            Simple _ ->
                Just SimpleKind
    , options =
        [ Input.option TieredKind (text "Tiered")
        , Input.option SimpleKind (text "Simple")
        ]
    }
        |> Input.radioRow [ Theme.spacing ]
        |> el [ alignRight ]


viewScore : Choices -> List Section -> Element Msg
viewScore choices sections =
    let
        sum : Int
        sum =
            sections
                |> List.map sumSection
                |> List.sum

        sumSection : Section -> Int
        sumSection { powers } =
            powers
                |> List.map
                    (\{ name, cost } ->
                        if powerTier choices name == Nothing then
                            0

                        else
                            cost
                    )
                |> List.sum

        common : List (Attribute msg)
        common =
            [ Font.bold ]
    in
    el
        (if sum > 70 then
            Font.color (rgb 0.7 0 0) :: common

         else
            common
        )
        (text <| "Score " ++ String.fromInt sum ++ "/70")


viewSection : Choices -> Section -> Element Msg
viewSection choices section =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        ]
        (el [ Font.bold ] (text section.name)
            :: List.map (\line -> paragraph [] [ text line ]) section.description
            ++ List.map
                (viewPower choices)
                section.powers
        )


viewPower : Choices -> Power -> Element Msg
viewPower choices power =
    let
        tier : Maybe Tier
        tier =
            powerTier choices power.name

        missingPrereq : List String
        missingPrereq =
            List.filter
                (\name -> powerTier choices name == Nothing)
                power.requires

        viewRequirement : String -> Element msg
        viewRequirement requirement =
            el
                [ Font.color <|
                    if List.member requirement missingPrereq then
                        if tier == Nothing then
                            rgb 0.6 0.4 0

                        else
                            rgb 1 0 0

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
            if tier == Nothing then
                if List.isEmpty missingPrereq then
                    rgb 0.9 0.9 1

                else
                    rgb 0.9 0.9 0.9

            else if List.isEmpty missingPrereq then
                rgb 0.7 1 0.7

            else
                rgb 1 0.7 0.7
        ]
        { onPress =
            Just <|
                ChooseTier power.name <|
                    if tier == Nothing then
                        Just S

                    else
                        Nothing
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


powerTier : Choices -> String -> Maybe Tier
powerTier choices name =
    case choices of
        Tiered tiered ->
            Dict.get name tiered

        Simple simple ->
            if Set.member name simple then
                Just S

            else
                Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRaw (Err e) ->
            ( { model | data = HttpError e }, Cmd.none )

        GotRaw (Ok raw) ->
            case Parser.run mainParser raw of
                Err e ->
                    ( { model | data = ParseError <| errorToString e }, Cmd.none )

                Ok newModel ->
                    ( { model
                        | data = Success newModel
                      }
                    , Cmd.none
                    )

        ToggleKind kind ->
            let
                newChoices : Choices
                newChoices =
                    case ( kind, model.choices ) of
                        ( TieredKind, Tiered _ ) ->
                            model.choices

                        ( TieredKind, Simple simple ) ->
                            Set.toList simple
                                |> List.map (\name -> ( name, S ))
                                |> Dict.fromList
                                |> Tiered

                        ( SimpleKind, Simple _ ) ->
                            model.choices

                        ( SimpleKind, Tiered tiered ) ->
                            Dict.keys tiered
                                |> Set.fromList
                                |> Simple
            in
            ( { model | choices = newChoices }
            , Browser.Navigation.replaceUrl model.key
                (choicesToUrl newChoices)
            )

        ChooseTier name tier ->
            let
                newChoices : Choices
                newChoices =
                    case ( tier, model.choices ) of
                        ( Just t, Tiered tiered ) ->
                            Tiered <| Dict.insert name t tiered

                        ( Nothing, Tiered tiered ) ->
                            Tiered <| Dict.remove name tiered

                        ( Just _, Simple simple ) ->
                            Simple <| Set.insert name simple

                        ( Nothing, Simple simple ) ->
                            Simple <| Set.remove name simple
            in
            ( { model | choices = newChoices }
            , Browser.Navigation.replaceUrl model.key
                (choicesToUrl newChoices)
            )

        UrlChange url ->
            ( { model | choices = urlToChoices url }, Cmd.none )

        UrlRequest (Browser.External ext) ->
            ( model, Browser.Navigation.load ext )

        UrlRequest (Browser.Internal url) ->
            ( { model | choices = urlToChoices url }
            , Browser.Navigation.pushUrl model.key (Url.toString url)
            )


errorToString : List Parser.DeadEnd -> String
errorToString deadEnds =
    String.join "\n" <|
        "Error:"
            :: List.map deadEndToString deadEnds


deadEndToString : Parser.DeadEnd -> String
deadEndToString deadEnd =
    Debug.toString deadEnd


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
        (\name cost requires description ->
            { name = name
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
