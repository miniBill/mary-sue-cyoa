module Types exposing (AdminMsg(..), BackendModel, BackendMsg(..), CYOA, CYOAId, Choices(..), FrontendModel, FrontendMsg(..), InnerAdminModel(..), InnerModel(..), Kind(..), Power, Requirement(..), Section, TBAuthenticated(..), Tier(..), ToBackend(..), ToFrontend(..), getAlternatives, groupPowers, powerTier, requirementToString, tierToString)

import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element exposing (DeviceClass)
import EnglishNumbers
import Lamdera exposing (ClientId)
import Set exposing (Set)
import Types.Password exposing (Password)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : InnerModel
    , deviceClass : DeviceClass
    }


type InnerModel
    = Homepage
    | Loading CYOAId Choices
    | NotFound CYOAId
    | Login
        { password : Password
        , loggingIn : Bool
        }
    | Admin
        { password : Password
        , cyoas : Dict CYOAId CYOA
        , inner : InnerAdminModel
        }
    | Loaded
        { cyoaId : CYOAId
        , choices : Choices
        , data : CYOA
        }


type InnerAdminModel
    = Listing
    | Creating CYOAId
    | Editing CYOAId String String Bool
    | Deleting CYOAId
    | Renaming CYOAId CYOAId


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


type alias CYOA =
    List Section


type alias Section =
    { name : String
    , description : List String
    , powers : List Power
    }


type alias Power =
    { label : String
    , id : CYOAId
    , cost : Int
    , replaces : Maybe CYOAId
    , requires : List Requirement
    , description : String
    }


type Requirement
    = Requirement CYOAId
    | AtLeastXOf Int (List Requirement)


type FrontendMsg
    = ChooseTier String (Maybe Tier)
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | ToggleKind Kind
    | Password Password
    | TryLogin
    | AdminMsg AdminMsg
    | Resize Int Int


type AdminMsg
    = CreatePrepare CYOAId
    | CreateDo CYOAId
    | UpdatePrepare CYOAId String String Bool
    | UpdateDo CYOAId CYOA
    | RenamePrepare CYOAId CYOAId
    | RenameDo CYOAId CYOAId
    | DeletePrepare CYOAId
    | DeleteDo CYOAId
    | List


type Kind
    = TieredKind
    | SimpleKind


type alias CYOAId =
    String


type alias BackendModel =
    { cyoas : Dict CYOAId CYOA
    , connections : Dict ClientId CYOAId
    }


type BackendMsg
    = BackendDisconnected ClientId


type ToBackend
    = TBGetCYOA CYOAId
    | TBAuthenticated Password TBAuthenticated


type TBAuthenticated
    = TBLogin
    | TBCreateCYOA CYOAId
    | TBRenameCYOA CYOAId CYOAId
    | TBUpdateCYOA CYOAId CYOA
    | TBDeleteCYOA CYOAId


type ToFrontend
    = TFGotCYOA CYOAId CYOA
    | TFRenamedCYOA CYOAId CYOAId
    | TFDeletedCYOA CYOAId
    | TFCYOAMissing CYOAId
    | TFAdmin (Dict CYOAId CYOA)


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


requirementToString : Requirement -> String
requirementToString requirement =
    case requirement of
        Requirement name ->
            name

        AtLeastXOf required alternatives ->
            "at least "
                ++ EnglishNumbers.toString required
                ++ " of: "
                ++ String.join ", "
                    (List.map
                        (\child ->
                            let
                                s : String
                                s =
                                    requirementToString child
                            in
                            if String.contains "," s then
                                "(" ++ s ++ ")"

                            else
                                s
                        )
                        alternatives
                    )


groupPowers : List Power -> List ( Power, List Power )
groupPowers powers =
    let
        ( finalLastGroup, finalAcc ) =
            powers
                |> List.foldl
                    (\power ( lastGroup, acc ) ->
                        case lastGroup of
                            Nothing ->
                                ( Just ( power, [] ), acc )

                            Just ( lastPower, lastGroupAcc ) ->
                                if power.replaces == Just lastPower.id then
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


getAlternatives : CYOA -> Dict CYOAId (List CYOAId)
getAlternatives sections =
    let
        upsert : CYOAId -> CYOAId -> Dict CYOAId (List CYOAId) -> Dict CYOAId (List CYOAId)
        upsert key value acc =
            Dict.insert key (value :: Maybe.withDefault [] (Dict.get key acc)) acc
    in
    sections
        |> List.foldl
            (\section acc ->
                List.foldl
                    (\power ->
                        upsert (Maybe.withDefault power.id power.replaces) power.id
                    )
                    acc
                    section.powers
            )
            Dict.empty
        |> Dict.map (\_ -> List.reverse)
        |> Dict.filter (\_ v -> List.length v > 1)
