module Backend exposing (app)

import CYOA exposing (CYOA)
import Env
import Id exposing (Id)
import Lamdera exposing (ClientId)
import List.Extra
import Platform.Cmd as Cmd
import Random
import SeqDict exposing (SeqDict)
import Types exposing (BackendModel, BackendMsg(..), TBAuthenticated(..), ToBackend(..), ToFrontend(..), User)
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
    ( { cyoas = SeqDict.empty
      , connections = SeqDict.empty
      , users = SeqDict.singleton (Id.fromString "admin") { password = Password.password Env.password }
      }
    , Cmd.none
    )


update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        BackendDisconnected clientId ->
            ( { model | connections = SeqDict.remove clientId model.connections }, Cmd.none )

        DoPasswordReset clientId userId password ->
            ( { model
                | users =
                    SeqDict.insert userId
                        { password = Password.password password }
                        model.users
              }
            , Lamdera.sendToFrontend clientId <| TFResetPassword userId password
            )

        DoCreateUser clientId userId password ->
            ( { model
                | users =
                    SeqDict.insert userId
                        { password = Password.password password }
                        model.users
              }
            , Lamdera.sendToFrontend clientId <| TFCreatedUser userId password
            )


updateFromFrontend : Lamdera.SessionId -> Lamdera.ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        TBGetCYOA cyoaId ->
            case SeqDict.get cyoaId model.cyoas of
                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId <| TFCYOAMissing cyoaId )

                Just ( cyoaUserId, cyoa ) ->
                    ( { model | connections = SeqDict.insert clientId cyoaId model.connections }
                    , Lamdera.sendToFrontend clientId <| TFGotCYOA cyoaId cyoaUserId cyoa
                    )

        TBAuthenticated password inner ->
            case checkPassword password model.users of
                Just userId ->
                    case inner of
                        TBLogin ->
                            ( model
                            , sendCYOAsToFrontend clientId userId model
                            )

                        TBListUsers ->
                            ( model
                            , Lamdera.sendToFrontend clientId <|
                                TFUsers <|
                                    if isAdmin userId then
                                        SeqDict.keys model.users

                                    else
                                        [ userId ]
                            )

                        TBResetPassword targetId ->
                            if isAdmin userId || targetId == userId then
                                ( model, resetPassword clientId targetId )

                            else
                                ( model, Cmd.none )

                        TBUpdateCYOA cyoaId sections ->
                            checkingUserId userId cyoaId model <|
                                \( cyoaUserId, _ ) ->
                                    ( { model | cyoas = SeqDict.insert cyoaId ( userId, sections ) model.cyoas }
                                    , sendTo clientId cyoaId (TFGotCYOA cyoaId cyoaUserId sections) model.connections
                                    )

                        TBCreateCYOA cyoaId ->
                            if SeqDict.member cyoaId model.cyoas then
                                ( model, Cmd.none )

                            else
                                let
                                    newCYOA : CYOA
                                    newCYOA =
                                        CYOA.empty
                                in
                                ( { model | cyoas = SeqDict.insert cyoaId ( userId, newCYOA ) model.cyoas }
                                , sendTo clientId cyoaId (TFGotCYOA cyoaId userId newCYOA) model.connections
                                )

                        TBCreateUser newUserId ->
                            if SeqDict.member newUserId model.users then
                                ( model, Cmd.none )

                            else
                                ( model
                                , createUser clientId newUserId
                                )

                        TBDeleteCYOA cyoaId ->
                            checkingUserId userId cyoaId model <|
                                \_ ->
                                    let
                                        newModel : BackendModel
                                        newModel =
                                            { model | cyoas = SeqDict.remove cyoaId model.cyoas }
                                    in
                                    ( newModel
                                    , sendCYOAsToFrontend clientId userId newModel
                                    )

                        TBRenameCYOA oldCyoaId newCyoaId ->
                            checkingUserId userId oldCyoaId model <|
                                \( cyoaUserId, cyoa ) ->
                                    let
                                        newModel : BackendModel
                                        newModel =
                                            { model
                                                | cyoas =
                                                    model.cyoas
                                                        |> SeqDict.remove oldCyoaId
                                                        |> SeqDict.insert newCyoaId ( cyoaUserId, cyoa )
                                            }
                                    in
                                    ( newModel
                                    , Lamdera.sendToFrontend clientId <| TFRenamedCYOA oldCyoaId newCyoaId
                                    )

                        TBTransferCYOA cyoaId newUserId ->
                            checkingUserId userId cyoaId model <|
                                \( _, cyoa ) ->
                                    let
                                        newModel : BackendModel
                                        newModel =
                                            { model
                                                | cyoas =
                                                    SeqDict.insert cyoaId
                                                        ( newUserId, cyoa )
                                                        model.cyoas
                                            }
                                    in
                                    ( newModel
                                    , Lamdera.sendToFrontend clientId <| TFTransferredCYOA cyoaId newUserId
                                    )

                Nothing ->
                    ( model, Cmd.none )


sendCYOAsToFrontend : ClientId -> Id User -> BackendModel -> Cmd BackendMsg
sendCYOAsToFrontend clientId userId model =
    Lamdera.sendToFrontend clientId <|
        TFAdmin <|
            if isAdmin userId then
                model.cyoas

            else
                SeqDict.filter (\_ ( cyoaUserId, _ ) -> cyoaUserId == userId) model.cyoas


isAdmin : Id User -> Bool
isAdmin userId =
    Id.toString userId == "admin"


checkingUserId : Id User -> Id CYOA -> BackendModel -> (( Id User, CYOA ) -> ( BackendModel, Cmd msg )) -> ( BackendModel, Cmd msg )
checkingUserId userId cyoaId model inner =
    case SeqDict.get cyoaId model.cyoas of
        Nothing ->
            ( model, Cmd.none )

        Just ( cyoaUserId, cyoa ) ->
            if isAdmin userId || cyoaUserId == userId then
                inner ( cyoaUserId, cyoa )

            else
                ( model, Cmd.none )


checkPassword : Password -> SeqDict (Id User) User -> Maybe (Id User)
checkPassword password users =
    users
        |> SeqDict.toList
        |> List.Extra.findMap
            (\( userId, user ) ->
                if Password.isCorrect user.password password then
                    Just userId

                else
                    Nothing
            )


resetPassword : ClientId -> Id User -> Cmd BackendMsg
resetPassword clientId userId =
    Random.generate (DoPasswordReset clientId userId) randomPassword


createUser : ClientId -> Id User -> Cmd BackendMsg
createUser clientId userId =
    Random.generate (DoCreateUser clientId userId) randomPassword


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


sendTo : ClientId -> Id CYOA -> ToFrontend -> SeqDict ClientId (Id CYOA) -> Cmd msg
sendTo admin cyoaId msg connections =
    connections
        |> SeqDict.toList
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
