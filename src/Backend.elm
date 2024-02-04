module Backend exposing (app)

import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Platform.Cmd as Cmd
import Random
import Types exposing (BackendModel, BackendMsg(..), CYOAId, TBAuthenticated(..), ToBackend(..), ToFrontend(..))
import Types.Password as Password


app :
    { init : ( BackendModel, Cmd BackendMsg )
    , update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
    , subscriptions : BackendModel -> Sub BackendMsg
    }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init : ( BackendModel, Cmd backendMsg )
init =
    ( { cyoas = Dict.empty
      , connections = Dict.empty
      , users = Dict.empty
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        BackendDisconnected clientId ->
            ( { model | connections = Dict.remove clientId model.connections }, Cmd.none )

        DoPasswordReset clientId userId password ->
            ( { model
                | users =
                    Dict.insert userId
                        { password = Password.password password }
                        model.users
              }
            , Lamdera.sendToFrontend clientId <| TFResetPassword userId password
            )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        TBGetCYOA cyoaId ->
            case Dict.get cyoaId model.cyoas of
                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId <| TFCYOAMissing cyoaId )

                Just cyoa ->
                    ( { model | connections = Dict.insert clientId cyoaId model.connections }
                    , Lamdera.sendToFrontend clientId <| TFGotCYOA cyoaId cyoa
                    )

        TBAuthenticated password inner ->
            if Password.isCorrect password then
                case inner of
                    TBLogin ->
                        ( { model | connections = Dict.insert clientId "admin" model.connections }
                        , Lamdera.sendToFrontend clientId <| TFAdmin model.cyoas
                        )

                    TBListUsers ->
                        ( model, Lamdera.sendToFrontend clientId <| TFUsers model.users )

                    TBResetPassword userId ->
                        ( model, resetPassword clientId userId )

                    TBUpdateCYOA cyoaId cyoa ->
                        ( { model | cyoas = Dict.insert cyoaId cyoa model.cyoas }
                        , sendTo clientId cyoaId (TFGotCYOA cyoaId cyoa) model.connections
                        )

                    TBCreateCYOA cyoaId ->
                        ( { model | cyoas = Dict.insert cyoaId [] model.cyoas }
                        , sendTo clientId cyoaId (TFGotCYOA cyoaId []) model.connections
                        )

                    TBDeleteCYOA cyoaId ->
                        let
                            newModel : BackendModel
                            newModel =
                                { model | cyoas = Dict.remove cyoaId model.cyoas }
                        in
                        ( newModel
                        , Lamdera.sendToFrontend clientId <| TFAdmin newModel.cyoas
                        )

                    TBRenameCYOA oldCyoadId newCyoadId ->
                        case Dict.get oldCyoadId model.cyoas of
                            Nothing ->
                                ( model, Cmd.none )

                            Just cyoa ->
                                let
                                    newModel : BackendModel
                                    newModel =
                                        { model
                                            | cyoas =
                                                model.cyoas
                                                    |> Dict.remove oldCyoadId
                                                    |> Dict.insert newCyoadId cyoa
                                        }
                                in
                                ( newModel
                                , Lamdera.sendToFrontend clientId <| TFRenamedCYOA oldCyoadId newCyoadId
                                )

            else
                ( model, Cmd.none )


resetPassword : ClientId -> Types.UserId -> Cmd BackendMsg
resetPassword clientId userId =
    Random.generate (DoPasswordReset clientId userId) randomPassword


randomPassword : Random.Generator String
randomPassword =
    List.range 0x22 0x7E
        |> List.map Char.fromCode
        |> Random.uniform '!'
        |> Random.list 8
        |> Random.map String.fromList


sendTo : ClientId -> CYOAId -> ToFrontend -> Dict ClientId CYOAId -> Cmd msg
sendTo admin cyoaId msg connections =
    connections
        |> Dict.toList
        |> List.filterMap
            (\( clientId, id ) ->
                if id == cyoaId then
                    Just clientId

                else
                    Nothing
            )
        |> (::) admin
        |> List.map (\id -> Lamdera.sendToFrontend id msg)
        |> Cmd.batch


subscriptions : BackendModel -> Sub BackendMsg
subscriptions _ =
    Lamdera.onDisconnect <| \_ clientId -> BackendDisconnected clientId
