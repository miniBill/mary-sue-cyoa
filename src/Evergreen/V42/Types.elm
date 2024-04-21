module Evergreen.V42.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Element
import Evergreen.V42.Types.Password
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
    , powers : List (Maybe Power)
    }


type alias UserId =
    String


type alias CYOA =
    { sections : List Section
    , userId : UserId
    }


type InnerAdminModel
    = Listing
    | ListingUsers (List UserId)
    | PasswordResetDone UserId String
    | Creating CYOAId
    | CreatingUser UserId
    | CreateUserDone UserId String
    | Editing CYOAId String String Bool
    | Deleting CYOAId
    | Renaming CYOAId CYOAId
    | Transferring CYOAId UserId


type alias AdminModel =
    { password : Evergreen.V42.Types.Password.Password
    , cyoas : Dict.Dict CYOAId CYOA
    , inner : InnerAdminModel
    }


type InnerModel
    = Homepage
    | Loading CYOAId Choices
    | NotFound CYOAId
    | Login
        { password : Evergreen.V42.Types.Password.Password
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


type alias User =
    { password : Evergreen.V42.Types.Password.Password
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
    | CreateUserPrepare UserId
    | CreateUserDo UserId
    | UpdatePrepare CYOAId String String Bool
    | UpdateDo CYOAId (List Section)
    | RenamePrepare CYOAId CYOAId
    | RenameDo CYOAId CYOAId
    | TransferPrepare CYOAId UserId
    | TransferDo CYOAId UserId
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
    | Password Evergreen.V42.Types.Password.Password
    | TryLogin
    | AdminMsg AdminMsg
    | Resize Int Int
    | Compact Bool


type TBAuthenticated
    = TBLogin
    | TBCreateCYOA CYOAId
    | TBRenameCYOA CYOAId CYOAId
    | TBTransferCYOA CYOAId UserId
    | TBUpdateCYOA CYOAId (List Section)
    | TBDeleteCYOA CYOAId
    | TBListUsers
    | TBCreateUser UserId
    | TBResetPassword UserId


type ToBackend
    = TBGetCYOA CYOAId
    | TBAuthenticated Evergreen.V42.Types.Password.Password TBAuthenticated


type BackendMsg
    = BackendDisconnected Lamdera.ClientId
    | DoPasswordReset Lamdera.ClientId UserId String
    | DoCreateUser Lamdera.ClientId UserId String


type ToFrontend
    = TFGotCYOA CYOAId CYOA
    | TFRenamedCYOA CYOAId CYOAId
    | TFTransferredCYOA CYOAId UserId
    | TFDeletedCYOA CYOAId
    | TFCYOAMissing CYOAId
    | TFAdmin (Dict.Dict CYOAId CYOA)
    | TFUsers (List UserId)
    | TFResetPassword UserId String
    | TFCreatedUser UserId String
