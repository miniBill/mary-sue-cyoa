module Frontend exposing (app)

import Admin
import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation exposing (Key)
import Color
import Dict
import Element exposing (Attribute, Color, Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, link, paddingEach, paragraph, rgb, row, scrollbarY, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Lamdera exposing (UrlRequest)
import Maybe.Extra
import Password exposing (Password)
import Set
import Theme
import Types exposing (AdminMsg(..), CYOAId, Choices(..), FrontendModel, FrontendMsg(..), InnerAdminModel(..), InnerModel(..), Kind(..), Power, Section, TBAuthenticated(..), Tier(..), ToBackend(..), ToFrontend(..))
import Url exposing (Url)
import Url.Builder


app :
    { init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , view =
            \model ->
                { title = "Mary Sue CYOA"
                , body =
                    [ Element.layout
                        [ width fill
                        , height fill
                        , Font.color Theme.darkViolet
                        , Background.color Theme.paleViolet
                        ]
                      <|
                        view model
                    ]
                }
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        , updateFromBackend = updateFromBackend
        }


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg ({ inner } as model) =
    let
        newInner : InnerModel
        newInner =
            case msg of
                TFCYOAMissing cyoaId ->
                    NotFound cyoaId

                TFRenamedCYOA from to ->
                    case inner of
                        Admin admin ->
                            case Dict.get from admin.cyoas of
                                Nothing ->
                                    inner

                                Just cyoa ->
                                    Admin
                                        { admin
                                            | cyoas =
                                                admin.cyoas
                                                    |> Dict.remove from
                                                    |> Dict.insert to cyoa
                                        }

                        _ ->
                            inner

                TFDeletedCYOA cyoaId ->
                    case inner of
                        Loading id _ ->
                            if id == cyoaId then
                                Homepage

                            else
                                inner

                        Admin admin ->
                            Admin
                                { admin
                                    | cyoas =
                                        Dict.remove cyoaId admin.cyoas
                                    , inner =
                                        case admin.inner of
                                            Deleting id ->
                                                if id == cyoaId then
                                                    Listing

                                                else
                                                    admin.inner

                                            Editing id _ _ ->
                                                if id == cyoaId then
                                                    Listing

                                                else
                                                    admin.inner

                                            Renaming from _ ->
                                                if from == cyoaId then
                                                    Listing

                                                else
                                                    admin.inner

                                            _ ->
                                                admin.inner
                                }

                        _ ->
                            inner

                TFGotCYOA cyoaId cyoa ->
                    case inner of
                        Loading id choices ->
                            if id == cyoaId then
                                Loaded
                                    { cyoaId = cyoaId
                                    , choices = choices
                                    , data = cyoa
                                    }

                            else
                                inner

                        Loaded loaded ->
                            if loaded.cyoaId == cyoaId then
                                Loaded { loaded | data = cyoa }

                            else
                                inner

                        Admin admin ->
                            Admin
                                { admin
                                    | cyoas = Dict.insert cyoaId cyoa admin.cyoas
                                }

                        _ ->
                            inner

                TFAdmin cyoas ->
                    case inner of
                        Login { password } ->
                            Admin
                                { password = password
                                , cyoas = cyoas
                                , inner = Listing
                                }

                        _ ->
                            inner
    in
    ( { model | inner = newInner }, Cmd.none )


init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url
    in
    case appUrl.path of
        [] ->
            ( { key = key
              , inner = Homepage
              }
            , Cmd.none
            )

        [ "admin" ] ->
            let
                ( inner, cmd ) =
                    case Dict.get "key" appUrl.queryParameters of
                        Just [ passwordString ] ->
                            let
                                password : Password
                                password =
                                    Password.password passwordString
                            in
                            ( { password = password
                              , loggingIn = True
                              }
                            , Lamdera.sendToBackend <| TBAuthenticated password TBLogin
                            )

                        _ ->
                            ( { password = Password.password ""
                              , loggingIn = False
                              }
                            , Cmd.none
                            )
            in
            ( { key = key
              , inner = Login inner
              }
            , cmd
            )

        _ ->
            let
                cyoaId : CYOAId
                cyoaId =
                    String.join "/" appUrl.path
            in
            ( { key = key
              , inner = Loading cyoaId <| urlToChoices appUrl
              }
            , Lamdera.sendToBackend <| TBGetCYOA cyoaId
            )


view : FrontendModel -> Element FrontendMsg
view { inner } =
    case inner of
        Homepage ->
            link
                [ centerX
                , centerY
                , Font.size 48
                , Font.underline
                ]
                { url = "https://en.wikipedia.org/wiki/Mary_Sue"
                , label = text "Mary Sue"
                }

        Loading cyoaId _ ->
            Theme.centralMessage <| "Loading " ++ cyoaId ++ "..."

        NotFound _ ->
            image [ centerX, centerY ]
                { src = "/404.png"
                , description = "CYOA not found"
                }

        Loaded innerModel ->
            column [ height fill ]
                [ row [ Theme.padding, width fill ]
                    [ viewScore innerModel.choices innerModel.data
                    , viewToggle innerModel.choices
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
                        (viewSection innerModel.choices)
                        innerModel.data
                    )
                ]

        Login login ->
            if login.loggingIn then
                Theme.centralMessage "Logging in..."

            else
                Theme.column [ centerX, centerY ]
                    [ Element.map Password <| Password.input login.password
                    , Input.button [ Border.width 1, Theme.padding ]
                        { label = text "Login"
                        , onPress = Just TryLogin
                        }
                    ]

        Admin admin ->
            Element.map AdminMsg <|
                Admin.view admin


urlToChoices : AppUrl -> Choices
urlToChoices appUrl =
    let
        list : List ( String, List String )
        list =
            appUrl.queryParameters
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


tierToColor : Tier -> Color.Color
tierToColor tier =
    case tier of
        S ->
            Color.rgb255 0x02 0xAF 0xF0

        A ->
            Color.rgb255 0x00 0xAE 0x50

        B ->
            Color.rgb255 0x92 0xCF 0x50

        C ->
            Color.rgb255 0xFE 0xD9 0x66

        D ->
            Color.rgb255 0xF7 0x86 0x1C

        F ->
            Color.rgb255 0xAC 0x00 0x00


viewToggle : Choices -> Element FrontendMsg
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
        [ Input.option SimpleKind (text "Simple")
        , Input.option TieredKind (text "Tiered")
        ]
    }
        |> Input.radioRow [ Theme.spacing ]
        |> el [ alignRight, alignTop ]


viewScore : Choices -> List Section -> Element FrontendMsg
viewScore choices sections =
    let
        sum : List Tier -> List Tier -> Int
        sum costTiers gainTiers =
            sections
                |> List.map (sumSection costTiers gainTiers)
                |> List.sum

        sumSection : List Tier -> List Tier -> Section -> Int
        sumSection costTiers gainTiers { powers } =
            powers
                |> List.map
                    (\{ id, cost } ->
                        case powerTier choices id of
                            Nothing ->
                                0

                            Just tier ->
                                if
                                    (cost > 0 && List.member tier costTiers)
                                        || (cost < 0 && List.member tier gainTiers)
                                then
                                    cost

                                else
                                    0
                    )
                |> List.sum
    in
    case choices of
        Tiered _ ->
            let
                tierLabel : Tier -> Element msg
                tierLabel tier =
                    ("S->" ++ tierToString tier)
                        |> text
                        |> el ([ Theme.padding, Font.center ] ++ tierButtonAttrs True tier)

                accTiers : List ( Tier, List Tier )
                accTiers =
                    [ S, A, B, C, D, F ]
                        |> List.foldl
                            (\tier ( acc, lacc ) ->
                                ( tier :: acc, ( tier, tier :: acc ) :: lacc )
                            )
                            ( [], [] )
                        |> Tuple.second
                        |> List.reverse
            in
            { data = accTiers
            , columns =
                accTiers
                    |> List.map
                        (\( colTier, colAll ) ->
                            { width = shrink
                            , header = tierLabel colTier
                            , view =
                                \( rowTier, rowAll ) ->
                                    let
                                        cellSum : Int
                                        cellSum =
                                            sum rowAll colAll
                                    in
                                    el
                                        (Font.center :: tierButtonAttrs (cellSum <= 70) rowTier)
                                        (text <| String.fromInt cellSum)
                            }
                        )
                    |> (::)
                        { width = shrink
                        , header = Element.none
                        , view = \( rowTier, _ ) -> tierLabel rowTier
                        }
            }
                |> Element.table []

        Simple _ ->
            let
                s : Int
                s =
                    sum
                        [ S, A, B, C, D, F ]
                        [ S, A, B, C, D, F ]
            in
            el
                (if s > 70 then
                    [ Font.color (rgb 0.7 0 0), Font.bold ]

                 else
                    [ Font.bold ]
                )
                (text <| "Score " ++ String.fromInt s ++ "/70")


viewSection : Choices -> Section -> Element FrontendMsg
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


viewPower : Choices -> Power -> Element FrontendMsg
viewPower choices power =
    let
        currentTier : Maybe Tier
        currentTier =
            powerTier choices power.id

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
                        if currentTier == Nothing then
                            rgb 0.6 0.4 0

                        else
                            rgb 1 0 0

                    else
                        rgb 0.4 0.6 0
                ]
                (text requirement)

        label : List (Element msg) -> Element msg
        label children =
            Theme.column [ width fill ]
                [ Theme.row [ width fill ]
                    [ el [ Font.bold ] <| text power.label
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
                , Theme.row [ width fill ]
                    (paragraph [ width fill ]
                        [ text power.description ]
                        :: children
                    )
                ]

        common : List (Attribute msg)
        common =
            [ Theme.padding
            , Border.width 1
            , width fill
            ]
    in
    case choices of
        Tiered _ ->
            el
                ((Background.color <|
                    if currentTier == Nothing then
                        if List.isEmpty missingPrereq then
                            rgb 0.9 0.9 1

                        else
                            rgb 0.9 0.9 0.9

                    else if List.isEmpty missingPrereq then
                        rgb 0.7 1 0.7

                    else
                        rgb 1 0.7 0.7
                 )
                    :: common
                )
            <|
                label <|
                    List.map
                        (\tier ->
                            let
                                selected : Bool
                                selected =
                                    Just tier == currentTier
                            in
                            Input.button
                                (tierButtonAttrs selected tier)
                                { onPress =
                                    Just
                                        (ChooseTier power.id <|
                                            if selected then
                                                Nothing

                                            else
                                                Just tier
                                        )
                                , label = text <| tierToString tier
                                }
                        )
                        [ S, A, B, C, D, F ]

        Simple _ ->
            Input.button
                ((Background.color <|
                    if currentTier == Nothing then
                        if List.isEmpty missingPrereq then
                            rgb 0.9 0.9 1

                        else
                            rgb 0.9 0.9 0.9

                    else if List.isEmpty missingPrereq then
                        rgb 0.7 1 0.7

                    else
                        rgb 1 0.7 0.7
                 )
                    :: common
                )
                { onPress =
                    Just <|
                        ChooseTier power.id <|
                            if currentTier == Nothing then
                                Just S

                            else
                                Nothing
                , label = label []
                }


tierButtonAttrs : Bool -> Tier -> List (Attribute msg)
tierButtonAttrs selected tier =
    [ if selected then
        Background.color <| colorToColor <| tierToColor tier

      else
        Background.color <| colorToColor <| hslaMap (\hsla -> { hsla | saturation = 0.2 }) <| tierToColor tier
    , Theme.padding
    , Border.width 1
    ]


hslaMap : (Hsla -> Hsla) -> Color.Color -> Color.Color
hslaMap f color =
    color
        |> Color.toHsla
        |> f
        |> Color.fromHsla


type alias Hsla =
    { alpha : Float
    , lightness : Float
    , hue : Float
    , saturation : Float
    }


colorToColor : Color.Color -> Color
colorToColor color =
    let
        rgba : { red : Float, green : Float, blue : Float, alpha : Float }
        rgba =
            Color.toRgba color
    in
    Element.rgba rgba.red rgba.green rgba.blue rgba.alpha


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


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.inner ) of
        ( ToggleKind kind, Loaded inner ) ->
            let
                newChoices : Choices
                newChoices =
                    case ( kind, inner.choices ) of
                        ( TieredKind, Tiered _ ) ->
                            inner.choices

                        ( TieredKind, Simple simple ) ->
                            Set.toList simple
                                |> List.map (\name -> ( name, S ))
                                |> Dict.fromList
                                |> Tiered

                        ( SimpleKind, Simple _ ) ->
                            inner.choices

                        ( SimpleKind, Tiered tiered ) ->
                            Dict.keys tiered
                                |> Set.fromList
                                |> Simple
            in
            ( { model | inner = Loaded { inner | choices = newChoices } }
            , Browser.Navigation.replaceUrl model.key
                (choicesToUrl newChoices)
            )

        ( ToggleKind _, _ ) ->
            ( model, Cmd.none )

        ( ChooseTier name tier, Loaded inner ) ->
            let
                newChoices : Choices
                newChoices =
                    case ( tier, inner.choices ) of
                        ( Just t, Tiered tiered ) ->
                            Tiered <| Dict.insert name t tiered

                        ( Nothing, Tiered tiered ) ->
                            Tiered <| Dict.remove name tiered

                        ( Just _, Simple simple ) ->
                            Simple <| Set.insert name simple

                        ( Nothing, Simple simple ) ->
                            Simple <| Set.remove name simple
            in
            ( { model | inner = Loaded { inner | choices = newChoices } }
            , Browser.Navigation.replaceUrl model.key
                (choicesToUrl newChoices)
            )

        ( ChooseTier _ _, _ ) ->
            ( model, Cmd.none )

        ( UrlChange url, Loaded inner ) ->
            ( { model
                | inner =
                    Loaded
                        { inner
                            | choices =
                                urlToChoices (AppUrl.fromUrl url)
                        }
              }
            , Cmd.none
            )

        ( UrlChange _, _ ) ->
            ( model, Cmd.none )

        ( UrlRequest (Browser.External ext), _ ) ->
            ( model, Browser.Navigation.load ext )

        ( UrlRequest (Browser.Internal url), _ ) ->
            ( model
            , Browser.Navigation.pushUrl model.key (Url.toString url)
            )

        ( Password password, Login login ) ->
            ( { model | inner = Login { login | password = password } }, Cmd.none )

        ( Password _, _ ) ->
            ( model, Cmd.none )

        ( TryLogin, Login login ) ->
            ( { model | inner = Login { login | loggingIn = True } }
            , Lamdera.sendToBackend <| TBAuthenticated login.password TBLogin
            )

        ( TryLogin, _ ) ->
            ( model, Cmd.none )

        ( AdminMsg innerMsg, Admin admin ) ->
            let
                ( newInner, maybeAuthenticatedMsg ) =
                    adminUpdate innerMsg
            in
            ( { model | inner = Admin { admin | inner = newInner } }
            , case maybeAuthenticatedMsg of
                Nothing ->
                    Cmd.none

                Just authenticatedMsg ->
                    Lamdera.sendToBackend <| TBAuthenticated admin.password authenticatedMsg
            )

        ( AdminMsg _, _ ) ->
            ( model, Cmd.none )


adminUpdate : AdminMsg -> ( InnerAdminModel, Maybe TBAuthenticated )
adminUpdate msg =
    case msg of
        CreatePrepare cyoaId ->
            ( Creating cyoaId, Nothing )

        CreateDo cyoaId ->
            ( Listing, Just <| TBCreateCYOA cyoaId )

        UpdatePrepare cyoaId old current ->
            ( Editing cyoaId old current, Nothing )

        UpdateDo cyoaId cyoa ->
            ( Listing, Just <| TBUpdateCYOA cyoaId cyoa )

        RenamePrepare from to ->
            ( Renaming from to, Nothing )

        RenameDo from to ->
            ( Listing, Just <| TBRenameCYOA from to )

        DeletePrepare cyoaId ->
            ( Deleting cyoaId, Nothing )

        DeleteDo cyoaId ->
            ( Listing, Just <| TBDeleteCYOA cyoaId )

        List ->
            ( Listing, Nothing )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none
