module Evergreen.V31.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Element
import Evergreen.V31.Types.Password
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


type alias UserId =
    String


type alias User =
    { password : Evergreen.V31.Types.Password.Password
    }


type InnerAdminModel
    = Listing
    | ListingUsers (Dict.Dict UserId User)
    | PasswordResetDone UserId String
    | Creating CYOAId
    | Editing CYOAId String String Bool
    | Deleting CYOAId
    | Renaming CYOAId CYOAId


type alias AdminModel =
    { password : Evergreen.V31.Types.Password.Password
    , cyoas : Dict.Dict CYOAId CYOA
    , inner : InnerAdminModel
    }


type InnerModel
    = Homepage
    | Loading CYOAId Choices
    | NotFound CYOAId
    | Login
        { password : Evergreen.V31.Types.Password.Password
        , loggingIn : Bool
        }
    | Admin AdminModel
    | Loaded
        { cyoaId : CYOAId
        , choices : Choices
        , compact : Bool
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
    , users : Dict.Dict UserId User
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
    | ResetPassword UserId
    | List
    | Users


type FrontendMsg
    = ChooseTier String (Maybe Tier)
    | UrlChange Url.Url
    | UrlRequest Browser.UrlRequest
    | ToggleKind Kind
    | Password Evergreen.V31.Types.Password.Password
    | TryLogin
    | AdminMsg AdminMsg
    | Resize Int Int
    | Compact Bool


type TBAuthenticated
    = TBLogin
    | TBCreateCYOA CYOAId
    | TBRenameCYOA CYOAId CYOAId
    | TBUpdateCYOA CYOAId CYOA
    | TBDeleteCYOA CYOAId
    | TBListUsers
    | TBResetPassword UserId


type ToBackend
    = TBGetCYOA CYOAId
    | TBAuthenticated Evergreen.V31.Types.Password.Password TBAuthenticated


type BackendMsg
    = BackendDisconnected Lamdera.ClientId
    | DoPasswordReset Lamdera.ClientId UserId String


type ToFrontend
    = TFGotCYOA CYOAId CYOA
    | TFRenamedCYOA CYOAId CYOAId
    | TFDeletedCYOA CYOAId
    | TFCYOAMissing CYOAId
    | TFAdmin (Dict.Dict CYOAId CYOA)
    | TFUsers (Dict.Dict UserId User)
    | TFResetPassword UserId String
