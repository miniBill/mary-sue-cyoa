module Frontend exposing (app)

import AppUrl exposing (AppUrl)
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation exposing (Key)
import Color exposing (rgb)
import Dict
import Html
import Lamdera exposing (UrlRequest)
import Maybe.Extra
import Set exposing (Set)
import Task
import Theme
import Theme.Colors
import Types exposing (AdminModel, AdminMsg(..), CYOAId, Choices(..), DeviceClass(..), FrontendModel, FrontendMsg(..), InnerAdminModel(..), InnerModel(..), Kind(..), Section, TBAuthenticated(..), Tier(..), ToBackend(..), ToFrontend(..))
import Types.Password as Password exposing (Password)
import Ui exposing (Element, alignRight, alignTop, centerX, centerY, column, el, fill, height, image, shrink, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Lazy
import Ui.Table
import Url exposing (Url)
import Url.Builder
import View.Admin
import View.CYOA


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
                    [ Html.node "style"
                        []
                        [ Html.text """
                            p {
                                margin: 0;
                            }
                            """
                        ]
                    , Ui.layout Ui.default
                        [ width fill
                        , height fill
                        , Theme.Colors.font Theme.Colors.darkViolet
                        , Theme.Colors.background Theme.Colors.paleViolet
                        ]
                        (Ui.Lazy.lazy2 view model.deviceClass model.inner)
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
        ( newInner, cmd ) =
            case msg of
                TFCYOAMissing cyoaId ->
                    ( NotFound cyoaId, Cmd.none )

                TFRenamedCYOA from to ->
                    ( case inner of
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
                    , Cmd.none
                    )

                TFTransferredCYOA cyoaId userId ->
                    ( case inner of
                        Admin admin ->
                            case Dict.get cyoaId admin.cyoas of
                                Nothing ->
                                    inner

                                Just cyoa ->
                                    Admin
                                        { admin
                                            | cyoas =
                                                Dict.insert cyoaId
                                                    { cyoa | userId = userId }
                                                    admin.cyoas
                                        }

                        _ ->
                            inner
                    , Cmd.none
                    )

                TFDeletedCYOA cyoaId ->
                    ( case inner of
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

                                            Editing id _ _ _ ->
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
                    , Cmd.none
                    )

                TFGotCYOA cyoaId cyoa ->
                    case inner of
                        Loading id choices ->
                            if id == cyoaId then
                                let
                                    cleaned :
                                        { cyoaId : CYOAId
                                        , choices : Choices
                                        , compact : Bool
                                        , data : Types.CYOA
                                        }
                                    cleaned =
                                        { cyoaId = cyoaId
                                        , choices = choices
                                        , data = cyoa
                                        , compact = False
                                        }
                                            |> cleanObsoleteChoices
                                in
                                ( Loaded cleaned
                                , Browser.Navigation.replaceUrl model.key
                                    (choicesToUrl cyoaId cleaned.choices)
                                )

                            else
                                ( inner, Cmd.none )

                        Loaded loaded ->
                            if loaded.cyoaId == cyoaId then
                                let
                                    cleaned :
                                        { cyoaId : CYOAId
                                        , choices : Choices
                                        , compact : Bool
                                        , data : Types.CYOA
                                        }
                                    cleaned =
                                        { loaded | data = cyoa }
                                            |> cleanObsoleteChoices
                                in
                                ( Loaded cleaned
                                , Browser.Navigation.replaceUrl model.key
                                    (choicesToUrl cyoaId cleaned.choices)
                                )

                            else
                                ( inner, Cmd.none )

                        Admin admin ->
                            ( Admin
                                { admin
                                    | cyoas = Dict.insert cyoaId cyoa admin.cyoas
                                }
                            , Cmd.none
                            )

                        _ ->
                            ( inner, Cmd.none )

                TFAdmin cyoas ->
                    ( case inner of
                        Login { password } ->
                            Admin
                                { password = password
                                , cyoas = cyoas
                                , inner = Listing
                                }

                        Admin admin ->
                            Admin { admin | cyoas = cyoas }

                        _ ->
                            inner
                    , Cmd.none
                    )

                TFUsers users ->
                    ( case inner of
                        Admin admin ->
                            Admin { admin | inner = ListingUsers users }

                        _ ->
                            inner
                    , Cmd.none
                    )

                TFResetPassword userId password ->
                    ( case inner of
                        Admin admin ->
                            Admin { admin | inner = PasswordResetDone userId password }

                        _ ->
                            inner
                    , Cmd.none
                    )

                TFCreatedUser userId password ->
                    ( case inner of
                        Admin admin ->
                            Admin { admin | inner = CreateUserDone userId password }

                        _ ->
                            inner
                    , Cmd.none
                    )
    in
    ( { model | inner = newInner }, cmd )


cleanObsoleteChoices :
    { cyoaId : CYOAId
    , choices : Choices
    , data : Types.CYOA
    , compact : Bool
    }
    ->
        { cyoaId : CYOAId
        , choices : Choices
        , compact : Bool
        , data : Types.CYOA
        }
cleanObsoleteChoices model =
    let
        existing : Set CYOAId
        existing =
            model.data.sections
                |> List.concatMap
                    (\section ->
                        section.powers
                            |> List.filterMap (Maybe.map (\power -> power.id))
                    )
                |> Set.fromList
    in
    { model
        | choices =
            case model.choices of
                Tiered dict ->
                    dict
                        |> Dict.filter (\key _ -> Set.member key existing)
                        |> Tiered

                Simple simple ->
                    simple
                        |> Set.filter (\key -> Set.member key existing)
                        |> Simple
    }


init : Url -> Key -> ( FrontendModel, Cmd FrontendMsg )
init url key =
    let
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url

        ( inner, cmd ) =
            case appUrl.path of
                [] ->
                    ( Homepage, Cmd.none )

                [ "admin" ] ->
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
                                |> Login
                            , Lamdera.sendToBackend <| TBAuthenticated password TBLogin
                            )

                        _ ->
                            ( { password = Password.password ""
                              , loggingIn = False
                              }
                                |> Login
                            , Cmd.none
                            )

                _ ->
                    let
                        cyoaId : CYOAId
                        cyoaId =
                            String.join "/" appUrl.path
                    in
                    ( Loading cyoaId <| urlToChoices appUrl
                    , Lamdera.sendToBackend <| TBGetCYOA cyoaId
                    )
    in
    ( { key = key
      , deviceClass = TabletOrDesktop
      , inner = inner
      }
    , Cmd.batch [ cmd, measureScreen ]
    )


measureScreen : Cmd FrontendMsg
measureScreen =
    Browser.Dom.getViewport
        |> Task.perform
            (\{ viewport } ->
                Resize
                    (floor viewport.width)
                    (floor viewport.height)
            )


view : DeviceClass -> InnerModel -> Element FrontendMsg
view deviceClass inner =
    case inner of
        Homepage ->
            el
                [ centerX
                , centerY
                , Font.size 48
                , Font.underline
                , Ui.link "https://en.wikipedia.org/wiki/Mary_Sue"
                ]
                (text "Mary Sue")

        Loading cyoaId _ ->
            Theme.centralMessage <| "Loading " ++ cyoaId ++ "..."

        NotFound _ ->
            image [ centerX, centerY ]
                { source = "/404.png"
                , description = "CYOA not found"
                , onLoad = Nothing
                }

        Loaded innerModel ->
            let
                isTiered : Bool
                isTiered =
                    case innerModel.choices of
                        Tiered _ ->
                            True

                        Simple _ ->
                            False
            in
            column [ height fill, width fill ]
                [ (if deviceClass == Phone && isTiered then
                    Theme.column

                   else
                    Theme.row
                  )
                    [ Theme.padding, width fill ]
                    [ viewScore innerModel.choices innerModel.data.sections
                    , viewToggles innerModel.choices innerModel.compact
                    ]
                , View.CYOA.view deviceClass (Just ChooseTier) innerModel
                ]

        Login login ->
            if login.loggingIn then
                Theme.centralMessage "Logging in..."

            else
                Theme.column [ centerX, centerY ]
                    [ Ui.map Password <| Password.input login.password
                    , el
                        [ Ui.border 1
                        , Theme.padding
                        , Input.button TryLogin
                        ]
                        (text "Login")
                    ]

        Admin admin ->
            Ui.map AdminMsg <|
                View.Admin.view deviceClass admin


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
                |> Maybe.Extra.combineMap
                    (\( key, tier ) ->
                        Maybe.map (Tuple.pair key) (tierFromString tier)
                    )
    in
    case tiered of
        Just ((_ :: _) as t) ->
            t
                |> Dict.fromList
                |> Dict.remove tieredKey
                |> Tiered

        _ ->
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


choicesToUrl : CYOAId -> Choices -> String
choicesToUrl cyoaid selected =
    case selected of
        Tiered tiers ->
            (if Dict.isEmpty tiers then
                Dict.singleton tieredKey S

             else
                tiers
            )
                |> Dict.toList
                |> List.map (\( key, value ) -> Url.Builder.string key <| Types.tierToString value)
                |> Url.Builder.absolute (String.split "/" cyoaid)

        Simple choices ->
            choices
                |> Set.toList
                |> List.map (\key -> Url.Builder.string key "Y")
                |> Url.Builder.absolute (String.split "/" cyoaid)


tieredKey : String
tieredKey =
    "__tiered__"


viewToggles : Choices -> Bool -> Element FrontendMsg
viewToggles choices compact =
    let
        kindRadio : Element FrontendMsg
        kindRadio =
            Input.chooseOne Ui.row
                [ Theme.spacing
                , alignRight
                ]
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

        compactCheck : Element FrontendMsg
        compactCheck =
            let
                label : { element : Element msg, id : Input.Label }
                label =
                    Input.label "compact" [] (text "Hide unselected options")
            in
            Ui.row []
                [ Input.checkbox
                    [ Theme.spacing
                    , alignRight
                    ]
                    { checked = compact
                    , onChange = Compact
                    , label = label.id
                    , icon = Nothing
                    }
                , label.element
                ]
    in
    Theme.column [ alignRight, alignTop ]
        [ kindRadio
        , compactCheck
        ]


viewScore : Choices -> List Section -> Element FrontendMsg
viewScore choices sections =
    let
        sumCosts : List Tier -> Int
        sumCosts costTiers =
            sumIf (\cost tier -> cost > 0 && List.member tier costTiers)

        sumGains : List Tier -> Int
        sumGains gainTiers =
            70 - sumIf (\cost tier -> cost < 0 && List.member tier gainTiers)

        sumIf : (Int -> Tier -> Bool) -> Int
        sumIf cond =
            sections
                |> List.map
                    (\{ powers } ->
                        powers
                            |> List.filterMap identity
                            |> List.map
                                (\{ id, cost } ->
                                    case Types.powerTier choices id of
                                        Nothing ->
                                            0

                                        Just tier ->
                                            if cond cost tier then
                                                cost

                                            else
                                                0
                                )
                            |> List.sum
                    )
                |> List.sum
    in
    case choices of
        Tiered _ ->
            let
                tierLabel : Tier -> Element msg
                tierLabel tier =
                    ("S->" ++ Types.tierToString tier)
                        |> text
                        |> el ([ Theme.padding, Font.center ] ++ Theme.tierButtonAttrs True tier)

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
            Ui.Table.view
                [ Ui.scrollableX
                , height shrink
                , Ui.widthMin 126
                , width fill
                ]
                (Ui.Table.columns
                    (Ui.Table.column
                        { header = Ui.Table.cell [] Ui.none
                        , view = \( rowTier, _ ) -> Ui.Table.cell [ Theme.padding ] <| text rowTier
                        }
                        :: List.map
                            (\( colTier, colAll ) ->
                                Ui.Table.column
                                    { header = Ui.Table.cell [] (tierLabel colTier)
                                    , view =
                                        \( _, rowSum ) ->
                                            Ui.Table.cell
                                                (Font.center :: Theme.tierButtonAttrs True colTier)
                                                (text <| String.fromInt <| rowSum colAll)
                                    }
                            )
                            accTiers
                    )
                )
                [ ( "Costs", sumCosts )
                , ( "Points", sumGains )
                ]

        Simple _ ->
            let
                costs : Int
                costs =
                    sumCosts [ S, A, B, C, D, F ]

                total : Int
                total =
                    sumGains [ S, A, B, C, D, F ]
            in
            el
                (if costs > total then
                    [ Font.color (rgb 0.7 0 0), Font.bold ]

                 else
                    [ Font.bold ]
                )
                (text <| "Score " ++ String.fromInt costs ++ "/" ++ String.fromInt total)


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
update msg model =
    case ( msg, model.inner ) of
        ( Resize w _, _ ) ->
            ( { model
                | deviceClass =
                    if w > 600 then
                        TabletOrDesktop

                    else
                        Phone
              }
            , Cmd.none
            )

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
                (choicesToUrl inner.cyoaId newChoices)
            )

        ( ToggleKind _, _ ) ->
            ( model, Cmd.none )

        ( Compact compact, Loaded inner ) ->
            ( { model | inner = Loaded { inner | compact = compact } }, Cmd.none )

        ( Compact _, _ ) ->
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
                (choicesToUrl inner.cyoaId newChoices)
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
                    adminUpdate admin innerMsg
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


adminUpdate : AdminModel -> AdminMsg -> ( InnerAdminModel, Maybe TBAuthenticated )
adminUpdate model msg =
    case msg of
        CreatePrepare cyoaId ->
            ( Creating cyoaId, Nothing )

        CreateDo cyoaId ->
            ( Listing, Just <| TBCreateCYOA cyoaId )

        CreateUserPrepare userId ->
            ( CreatingUser userId, Nothing )

        CreateUserDo userId ->
            ( Listing, Just <| TBCreateUser userId )

        UpdatePrepare cyoaId old current preview ->
            ( Editing cyoaId old current preview, Nothing )

        UpdateDo cyoaId cyoa ->
            ( Listing, Just <| TBUpdateCYOA cyoaId cyoa )

        RenamePrepare from to ->
            ( Renaming from to, Nothing )

        RenameDo from to ->
            ( Listing, Just <| TBRenameCYOA from to )

        TransferPrepare from to ->
            ( Transferring from to, Nothing )

        TransferDo from to ->
            ( Listing, Just <| TBTransferCYOA from to )

        DeletePrepare cyoaId ->
            ( Deleting cyoaId, Nothing )

        DeleteDo cyoaId ->
            ( Listing, Just <| TBDeleteCYOA cyoaId )

        List ->
            ( Listing, Nothing )

        Users ->
            ( model.inner, Just TBListUsers )

        ResetPassword userId ->
            ( model.inner, Just <| TBResetPassword userId )


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Browser.Events.onResize Resize
