module Evergreen.V23.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Element
import Evergreen.V23.Types.Password
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


type Requirement
    = Requirement CYOAId
    | AtLeastXOf Int (List Requirement)


type alias Power =
    { label : String
    , id : CYOAId
    , cost : Int
    , replaces : Maybe CYOAId
    , requires : List Requirement
    , description : String
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
        { password : Evergreen.V23.Types.Password.Password
        , loggingIn : Bool
        }
    | Admin
        { password : Evergreen.V23.Types.Password.Password
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
    , deviceClass : Element.DeviceClass
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
    | Password Evergreen.V23.Types.Password.Password
    | TryLogin
    | AdminMsg AdminMsg
    | Resize Int Int


type TBAuthenticated
    = TBLogin
    | TBCreateCYOA CYOAId
    | TBRenameCYOA CYOAId CYOAId
    | TBUpdateCYOA CYOAId CYOA
    | TBDeleteCYOA CYOAId


type ToBackend
    = TBGetCYOA CYOAId
    | TBAuthenticated Evergreen.V23.Types.Password.Password TBAuthenticated


type BackendMsg
    = BackendDisconnected Lamdera.ClientId


type ToFrontend
    = TFGotCYOA CYOAId CYOA
    | TFRenamedCYOA CYOAId CYOAId
    | TFDeletedCYOA CYOAId
    | TFCYOAMissing CYOAId
    | TFAdmin (Dict.Dict CYOAId CYOA)
