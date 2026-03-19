module Types exposing (AdminModel, AdminMsg(..), BackendModel, BackendMsg(..), Choices(..), FrontendModel, FrontendMsg(..), InnerAdminModel(..), InnerModel(..), Kind(..), TBAuthenticated(..), Tier(..), ToBackend(..), ToFrontend(..), User, getAlternatives, groupPowers, powerTier, tierToString)

import Browser
import Browser.Navigation exposing (Key)
import CYOA exposing (CYOA, PowerData, PowerOrBreak(..), Section, SectionElement(..))
import Element exposing (DeviceClass)
import Id exposing (Id)
import Lamdera exposing (ClientId)
import SeqDict exposing (SeqDict)
import SeqSet exposing (SeqSet)
import Types.Password exposing (Password)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : InnerModel
    , deviceClass : DeviceClass
    }


type InnerModel
    = Homepage
    | Loading (Id CYOA) Choices
    | NotFound (Id CYOA)
    | Login
        { password : Password
        , loggingIn : Bool
        }
    | Admin AdminModel
    | Loaded
        { cyoaId : Id CYOA
        , choices : Choices
        , compact : Bool
        , data : CYOA
        }


type alias AdminModel =
    { password : Password
    , cyoas : SeqDict (Id CYOA) ( Id User, CYOA )
    , inner : InnerAdminModel
    }


type InnerAdminModel
    = Listing
    | ListingUsers (List (Id User))
    | PasswordResetDone (Id User) String
    | Creating (Id CYOA)
    | CreatingUser (Id User)
    | CreateUserDone (Id User) String
    | Editing (Id CYOA) String String Bool
    | Deleting (Id CYOA)
    | Renaming (Id CYOA) (Id CYOA)
    | Transferring (Id CYOA) (Id User)


type Choices
    = Tiered (SeqDict (Id PowerOrBreak) Tier)
    | Simple (SeqSet (Id PowerOrBreak))


type Tier
    = S
    | A
    | B
    | C
    | D
    | F


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


type FrontendMsg
    = ChooseTier (Id PowerOrBreak) (Maybe Tier)
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | ToggleKind Kind
    | Password Password
    | TryLogin
    | AdminMsg AdminMsg
    | Resize Int Int
    | Compact Bool


type AdminMsg
    = CreatePrepare (Id CYOA)
    | CreateDo (Id CYOA)
    | CreateUserPrepare (Id User)
    | CreateUserDo (Id User)
    | UpdatePrepare (Id CYOA) String String Bool
    | UpdateDo (Id CYOA) (List Section)
    | RenamePrepare (Id CYOA) (Id CYOA)
    | RenameDo (Id CYOA) (Id CYOA)
    | TransferPrepare (Id CYOA) (Id User)
    | TransferDo (Id CYOA) (Id User)
    | DeletePrepare (Id CYOA)
    | DeleteDo (Id CYOA)
    | ResetPassword (Id User)
    | List
    | Users


type Kind
    = TieredKind
    | SimpleKind


type alias BackendModel =
    { cyoas : SeqDict (Id CYOA) ( Id User, CYOA )
    , connections : SeqDict ClientId (Id CYOA)
    , users : SeqDict (Id User) User
    }


type alias User =
    { password : Password
    }


type BackendMsg
    = BackendDisconnected ClientId
    | DoPasswordReset ClientId (Id User) String
    | DoCreateUser ClientId (Id User) String


type ToBackend
    = TBGetCYOA (Id CYOA)
    | TBAuthenticated Password TBAuthenticated


type TBAuthenticated
    = TBLogin
    | TBCreateCYOA (Id CYOA)
    | TBRenameCYOA (Id CYOA) (Id CYOA)
    | TBTransferCYOA (Id CYOA) (Id User)
    | TBUpdateCYOA (Id CYOA) (List Section)
    | TBDeleteCYOA (Id CYOA)
    | TBListUsers
    | TBCreateUser (Id User)
    | TBResetPassword (Id User)


type ToFrontend
    = TFGotCYOA (Id CYOA) (Id User) CYOA
    | TFRenamedCYOA (Id CYOA) (Id CYOA)
    | TFTransferredCYOA (Id CYOA) (Id User)
    | TFDeletedCYOA (Id CYOA)
    | TFCYOAMissing (Id CYOA)
    | TFAdmin (SeqDict (Id CYOA) ( Id User, CYOA ))
    | TFUsers (List (Id User))
    | TFResetPassword (Id User) String
    | TFCreatedUser (Id User) String


powerTier : Choices -> Id PowerOrBreak -> Maybe Tier
powerTier choices name =
    case choices of
        Tiered tiered ->
            SeqDict.get name tiered

        Simple simple ->
            if SeqSet.member name simple then
                Just S

            else
                Nothing


groupPowers : List PowerOrBreak -> List ( PowerData, List PowerData )
groupPowers powers =
    let
        ( finalLastGroup, finalAcc ) =
            powers
                |> List.foldl
                    (\maybePower ( lastGroup, acc ) ->
                        case lastGroup of
                            Nothing ->
                                case maybePower of
                                    Break ->
                                        ( Nothing, acc )

                                    Power power ->
                                        ( Just ( power, [] ), acc )

                            Just ( lastPower, lastGroupAcc ) ->
                                case maybePower of
                                    Break ->
                                        ( Nothing
                                        , ( lastPower, List.reverse lastGroupAcc ) :: acc
                                        )

                                    Power power ->
                                        if power.replaces == Just lastPower.id || (power.replaces /= Nothing && power.replaces == lastPower.replaces) then
                                            ( Just ( lastPower, { power | requires = lastPower.requires } :: lastGroupAcc ), acc )

                                        else
                                            ( Just ( power, [] )
                                            , ( lastPower, List.reverse lastGroupAcc ) :: acc
                                            )
                    )
                    ( Nothing, [] )
    in
    case finalLastGroup of
        Nothing ->
            List.reverse finalAcc

        Just ( lp, lg ) ->
            List.reverse (( lp, lg ) :: finalAcc)


getAlternatives : List Section -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak))
getAlternatives sections =
    let
        upsert : Id PowerOrBreak -> Id PowerOrBreak -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak))
        upsert key value acc =
            let
                newValue : List (Id PowerOrBreak)
                newValue =
                    case SeqDict.get key acc of
                        Nothing ->
                            [ value ]

                        Just existing ->
                            value :: existing
            in
            SeqDict.insert key newValue acc

        foldSectionElement : SectionElement -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak))
        foldSectionElement element acc =
            case element of
                PowerElement p ->
                    foldSubsectionElement p acc

                Subsection subsection ->
                    List.foldl foldSubsectionElement acc subsection.powers

        foldSubsectionElement : PowerOrBreak -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak))
        foldSubsectionElement element acc =
            case element of
                Break ->
                    acc

                Power p ->
                    upsert (Maybe.withDefault p.id p.replaces) p.id acc
    in
    sections
        |> List.foldl
            (\section acc ->
                List.foldl foldSectionElement acc section.elements
            )
            SeqDict.empty
        |> SeqDict.map (\_ -> List.reverse)
        |> SeqDict.filter (\_ v -> List.length v > 1)
