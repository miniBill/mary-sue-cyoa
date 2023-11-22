module Evergreen.V5.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V5.Password
import Lamdera
import Set
import Url


type alias CYOAId =
    String


type Tier
    = S
    | A
    | B
    | C
    | D
    | F


type Choices
    = Tiered (Dict.Dict String Tier)
    | Simple (Set.Set String)


type alias Power =
    { label : String
    , id : String
    , cost : Int
    , description : String
    , requires : List String
    }


type alias Section =
    { name : String
    , description : List String
    , powers : List Power
    }


type alias CYOA =
    List Section


type InnerAdminModel
    = Listing
    | Creating CYOAId
    | Editing CYOAId String String Bool
    | Deleting CYOAId
    | Renaming CYOAId CYOAId


type InnerModel
    = Homepage
    | Loading CYOAId Choices
    | NotFound CYOAId
    | Login
        { password : Evergreen.V5.Password.Password
        , loggingIn : Bool
        }
    | Admin
        { password : Evergreen.V5.Password.Password
        , cyoas : Dict.Dict CYOAId CYOA
        , inner : InnerAdminModel
        }
    | Loaded
        { cyoaId : CYOAId
        , choices : Choices
        , data : CYOA
        }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , inner : InnerModel
    }


type alias BackendModel =
    { cyoas : Dict.Dict CYOAId CYOA
    , connections : Dict.Dict Lamdera.ClientId CYOAId
    }


type Kind
    = TieredKind
    | SimpleKind


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


type FrontendMsg
    = ChooseTier String (Maybe Tier)
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest
    | ToggleKind Kind
    | Password Evergreen.V5.Password.Password
    | TryLogin
    | AdminMsg AdminMsg


type TBAuthenticated
    = TBLogin
    | TBCreateCYOA CYOAId
    | TBRenameCYOA CYOAId CYOAId
    | TBUpdateCYOA CYOAId CYOA
    | TBDeleteCYOA CYOAId


type ToBackend
    = TBGetCYOA CYOAId
    | TBAuthenticated Evergreen.V5.Password.Password TBAuthenticated


type BackendMsg
    = BackendDisconnected Lamdera.ClientId


type ToFrontend
    = TFGotCYOA CYOAId CYOA
    | TFRenamedCYOA CYOAId CYOAId
    | TFDeletedCYOA CYOAId
    | TFCYOAMissing CYOAId
    | TFAdmin (Dict.Dict CYOAId CYOA)
