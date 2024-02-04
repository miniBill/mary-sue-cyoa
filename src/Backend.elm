module Backend exposing (app)

import Dict exposing (Dict)
import Env
import Lamdera exposing (ClientId)
import List.Extra
import Platform.Cmd as Cmd
import Random
import Types exposing (BackendModel, BackendMsg(..), CYOA, CYOAId, TBAuthenticated(..), ToBackend(..), ToFrontend(..), User, UserId)
import Types.Password as Password exposing (Password)


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
      , users = Dict.singleton "admin" { password = Password.password Env.password }
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
            case checkPassword password model.users of
                Just userId ->
                    case inner of
                        TBLogin ->
                            ( model
                            , Lamdera.sendToFrontend clientId <|
                                TFAdmin <|
                                    if userId == "admin" then
                                        model.cyoas

                                    else
                                        Dict.filter (\_ cyoa -> cyoa.userId == userId) model.cyoas
                            )

                        TBListUsers ->
                            ( model
                            , Lamdera.sendToFrontend clientId <|
                                TFUsers <|
                                    if userId == "admin" then
                                        model.users

                                    else
                                        Dict.filter (\uid _ -> uid == userId) model.users
                            )

                        TBResetPassword targetId ->
                            if userId == "admin" || targetId == userId then
                                ( model, resetPassword clientId targetId )

                            else
                                ( model, Cmd.none )

                        TBUpdateCYOA cyoaId sections ->
                            checkingUserId userId cyoaId model <|
                                \cyoa ->
                                    let
                                        updated : CYOA
                                        updated =
                                            { cyoa | sections = sections }
                                    in
                                    ( { model | cyoas = Dict.insert cyoaId updated model.cyoas }
                                    , sendTo clientId cyoaId (TFGotCYOA cyoaId updated) model.connections
                                    )

                        TBCreateCYOA cyoaId ->
                            if Dict.member cyoaId model.cyoas then
                                ( model, Cmd.none )

                            else
                                let
                                    newCYOA : CYOA
                                    newCYOA =
                                        { sections = [], userId = userId }
                                in
                                ( { model | cyoas = Dict.insert cyoaId newCYOA model.cyoas }
                                , sendTo clientId cyoaId (TFGotCYOA cyoaId newCYOA) model.connections
                                )

                        TBDeleteCYOA cyoaId ->
                            checkingUserId userId cyoaId model <|
                                \_ ->
                                    let
                                        newModel : BackendModel
                                        newModel =
                                            { model | cyoas = Dict.remove cyoaId model.cyoas }
                                    in
                                    ( newModel
                                    , Lamdera.sendToFrontend clientId <| TFAdmin newModel.cyoas
                                    )

                        TBRenameCYOA oldCyoaId newCyoaId ->
                            checkingUserId userId oldCyoaId model <|
                                \cyoa ->
                                    let
                                        newModel : BackendModel
                                        newModel =
                                            { model
                                                | cyoas =
                                                    model.cyoas
                                                        |> Dict.remove oldCyoaId
                                                        |> Dict.insert newCyoaId cyoa
                                            }
                                    in
                                    ( newModel
                                    , Lamdera.sendToFrontend clientId <| TFRenamedCYOA oldCyoaId newCyoaId
                                    )

                Nothing ->
                    ( model, Cmd.none )


checkingUserId : UserId -> CYOAId -> BackendModel -> (CYOA -> ( BackendModel, Cmd msg )) -> ( BackendModel, Cmd msg )
checkingUserId userId cyoaId model inner =
    case Dict.get cyoaId model.cyoas of
        Nothing ->
            ( model, Cmd.none )

        Just cyoa ->
            if userId == "admin" || cyoa.userId == userId then
                inner cyoa

            else
                ( model, Cmd.none )


checkPassword : Password -> Dict UserId User -> Maybe UserId
checkPassword password users =
    users
        |> Dict.toList
        |> List.Extra.findMap
            (\( userId, user ) ->
                if Password.isCorrect user.password password then
                    Just userId

                else
                    Nothing
            )


resetPassword : ClientId -> UserId -> Cmd BackendMsg
resetPassword clientId userId =
    Random.generate (DoPasswordReset clientId userId) randomPassword


randomPassword : Random.Generator String
randomPassword =
    let
        vowels : ( Char, List Char )
        vowels =
            ( 'a', [ 'e', 'i', 'o', 'u' ] )

        consonants : ( Char, List Char )
        consonants =
            ( 'b', [ 'c', 'd', 'f', 'g', 'j', 'k', 'l', 'm', 'n', 'p', 'r', 's', 't', 'v', 'w', 'x', 'z' ] )

        uniform : ( a, List a ) -> Random.Generator a
        uniform ( first, rest ) =
            Random.uniform first rest
    in
    Random.pair (uniform consonants) (uniform vowels)
        |> Random.list 16
        |> Random.map
            (List.concatMap (\( c, v ) -> [ c, v ])
                >> String.fromList
            )


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
