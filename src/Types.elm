module Types exposing (AdminMsg(..), BackendModel, BackendMsg(..), CYOA, CYOAId, Choices(..), FrontendModel, FrontendMsg(..), InnerAdminModel(..), InnerModel(..), Kind(..), Power, Section, TBAuthenticated(..), Tier(..), ToBackend(..), ToFrontend(..), powerTier, tierToString)

import Browser
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId)
import Password exposing (Password)
import Set exposing (Set)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , inner : InnerModel
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
    , id : String
    , cost : Int
    , description : String
    , requires : List String
    }


type FrontendMsg
    = ChooseTier String (Maybe Tier)
    | UrlChange Url
    | UrlRequest Browser.UrlRequest
    | ToggleKind Kind
    | Password Password
    | TryLogin
    | AdminMsg AdminMsg


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
