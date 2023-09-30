module Main exposing (Flags, Model, Msg, Power, Section, WebData, main)

import AppUrl
import Base64
import Browser
import Browser.Navigation exposing (Key)
import Codec.Bare exposing (Codec)
import Element exposing (Attribute, Element, alignRight, column, el, fill, height, paddingEach, paragraph, rgb, scrollbarY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import List.Extra
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Theme
import Url exposing (Url)


type alias Model =
    { key : Key
    , selected : List (Set Int)
    , data : WebData (List Section)
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
    , requires : List String
    }


type Msg
    = Select Int Int Bool
    | GotRaw (Result Http.Error String)
    | UrlChange Url
    | UrlRequest Browser.UrlRequest


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view =
            \model ->
                { title = "no vices for you to exploit"
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
    , selected = urlToSelected url
    , data = Loading
    }


urlToSelected : Url -> List (Set Int)
urlToSelected url =
    AppUrl.fromUrl url
        |> .fragment
        |> Maybe.andThen Base64.toBytes
        |> Maybe.andThen (Codec.Bare.decodeValue urlCodec)
        |> Maybe.withDefault []


selectedToUrl : List (Set Int) -> String
selectedToUrl selected =
    selected
        |> Codec.Bare.encodeToValue urlCodec
        |> Base64.fromBytes
        |> Maybe.withDefault ""


urlCodec : Codec (List (Set Int))
urlCodec =
    Codec.Bare.list (Codec.Bare.set Codec.Bare.int)


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
            let
                selected : Set String
                selected =
                    sections
                        |> List.map2 Tuple.pair model.selected
                        |> List.concatMap
                            (\( selectedInSection, { powers } ) ->
                                powers
                                    |> List.indexedMap
                                        (\index power ->
                                            if Set.member index selectedInSection then
                                                Just power.name

                                            else
                                                Nothing
                                        )
                                    |> List.filterMap identity
                            )
                        |> Set.fromList
            in
            column [ height fill ]
                [ viewScore selected sections
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
                    (List.indexedMap
                        (\sectionIndex section ->
                            Element.map
                                (\( powerIndex, value ) -> Select sectionIndex powerIndex value)
                                (viewSection selected section)
                        )
                        sections
                    )
                ]


viewScore : Set String -> List Section -> Element Msg
viewScore selected sections =
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
                        if Set.member name selected then
                            cost

                        else
                            0
                    )
                |> List.sum

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


viewSection : Set String -> Section -> Element ( Int, Bool )
viewSection selected section =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        ]
        (el [ Font.bold ] (text section.name)
            :: List.map (\line -> paragraph [] [ text line ]) section.description
            ++ List.indexedMap
                (\powerIndex power -> Element.map (Tuple.pair powerIndex) (viewPower selected power))
                section.powers
        )


viewPower : Set String -> Power -> Element Bool
viewPower selected power =
    let
        missingPrereq : List String
        missingPrereq =
            List.filter (\name -> not <| Set.member name selected) power.requires

        viewRequirement : String -> Element msg
        viewRequirement requirement =
            el
                [ Font.color <|
                    if List.member requirement missingPrereq then
                        if Set.member power.name selected then
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
            if Set.member power.name selected then
                if List.isEmpty missingPrereq then
                    rgb 0.7 1 0.7

                else
                    rgb 1 0.7 0.7

            else if List.isEmpty missingPrereq then
                rgb 0.9 0.9 1

            else
                rgb 0.9 0.9 0.9
        ]
        { onPress = Just <| not <| Set.member power.name selected
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRaw (Err e) ->
            ( { model | data = HttpError e }, Cmd.none )

        GotRaw (Ok raw) ->
            case Parser.run mainParser raw of
                Err e ->
                    ( { model | data = ParseError <| Debug.toString e }, Cmd.none )

                Ok newModel ->
                    ( { model
                        | data = Success newModel
                        , selected =
                            model.selected
                                ++ List.repeat
                                    (List.length newModel - List.length model.selected)
                                    Set.empty
                      }
                    , Cmd.none
                    )

        Select section powerIndex selected ->
            let
                newSelected : List (Set Int)
                newSelected =
                    List.Extra.updateAt section
                        (if selected then
                            Set.insert powerIndex

                         else
                            Set.remove powerIndex
                        )
                        model.selected
            in
            ( { model
                | selected =
                    newSelected
              }
            , Browser.Navigation.replaceUrl model.key
                ("#" ++ selectedToUrl newSelected)
            )

        UrlChange url ->
            ( { model | selected = urlToSelected url }, Cmd.none )

        UrlRequest (Browser.External ext) ->
            ( model, Browser.Navigation.load ext )

        UrlRequest (Browser.Internal url) ->
            ( { model | selected = urlToSelected url }
            , Browser.Navigation.pushUrl model.key (Url.toString url)
            )


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
