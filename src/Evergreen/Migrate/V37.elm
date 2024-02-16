module Evergreen.Migrate.V37 exposing (..)

import Dict
import Evergreen.V36.Types
import Evergreen.V36.Types.Password
import Evergreen.V37.Types
import Evergreen.V37.Types.Password
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V36.Types.FrontendModel -> ModelMigration Evergreen.V37.Types.FrontendModel Evergreen.V37.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V36.Types.BackendModel -> ModelMigration Evergreen.V37.Types.BackendModel Evergreen.V37.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V36.Types.FrontendMsg -> MsgMigration Evergreen.V37.Types.FrontendMsg Evergreen.V37.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V36.Types.ToBackend -> MsgMigration Evergreen.V37.Types.ToBackend Evergreen.V37.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V36.Types.BackendMsg -> MsgMigration Evergreen.V37.Types.BackendMsg Evergreen.V37.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V36.Types.ToFrontend -> MsgMigration Evergreen.V37.Types.ToFrontend Evergreen.V37.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_FrontendModel : Evergreen.V36.Types.FrontendModel -> Evergreen.V37.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    , deviceClass = old.deviceClass
    }


migrate_Types_AdminModel : Evergreen.V36.Types.AdminModel -> Evergreen.V37.Types.AdminModel
migrate_Types_AdminModel old =
    { password = old.password |> migrate_Types_Password_Password
    , cyoas = old.cyoas |> Dict.map (\k -> migrate_Types_CYOA)
    , inner = old.inner |> migrate_Types_InnerAdminModel
    }


migrate_Types_AdminMsg : Evergreen.V36.Types.AdminMsg -> Evergreen.V37.Types.AdminMsg
migrate_Types_AdminMsg old =
    case old of
        Evergreen.V36.Types.CreatePrepare p0 ->
            Evergreen.V37.Types.CreatePrepare p0

        Evergreen.V36.Types.CreateDo p0 ->
            Evergreen.V37.Types.CreateDo p0

        Evergreen.V36.Types.CreateUserPrepare p0 ->
            Evergreen.V37.Types.CreateUserPrepare p0

        Evergreen.V36.Types.CreateUserDo p0 ->
            Evergreen.V37.Types.CreateUserDo p0

        Evergreen.V36.Types.UpdatePrepare p0 p1 p2 p3 ->
            Evergreen.V37.Types.UpdatePrepare p0 p1 p2 p3

        Evergreen.V36.Types.UpdateDo p0 p1 ->
            Evergreen.V37.Types.UpdateDo p0 (p1 |> List.map migrate_Types_Section)

        Evergreen.V36.Types.RenamePrepare p0 p1 ->
            Evergreen.V37.Types.RenamePrepare p0 p1

        Evergreen.V36.Types.RenameDo p0 p1 ->
            Evergreen.V37.Types.RenameDo p0 p1

        Evergreen.V36.Types.DeletePrepare p0 ->
            Evergreen.V37.Types.DeletePrepare p0

        Evergreen.V36.Types.DeleteDo p0 ->
            Evergreen.V37.Types.DeleteDo p0

        Evergreen.V36.Types.ResetPassword p0 ->
            Evergreen.V37.Types.ResetPassword p0

        Evergreen.V36.Types.List ->
            Evergreen.V37.Types.List

        Evergreen.V36.Types.Users ->
            Evergreen.V37.Types.Users


migrate_Types_CYOA : Evergreen.V36.Types.CYOA -> Evergreen.V37.Types.CYOA
migrate_Types_CYOA old =
    { sections = old.sections |> List.map migrate_Types_Section
    , userId = old.userId
    }


migrate_Types_Choices : Evergreen.V36.Types.Choices -> Evergreen.V37.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V36.Types.Tiered p0 ->
            Evergreen.V37.Types.Tiered (p0 |> Dict.map (\k -> migrate_Types_Tier))

        Evergreen.V36.Types.Simple p0 ->
            Evergreen.V37.Types.Simple p0


migrate_Types_FrontendMsg : Evergreen.V36.Types.FrontendMsg -> Evergreen.V37.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V36.Types.ChooseTier p0 p1 ->
            Evergreen.V37.Types.ChooseTier p0 (p1 |> Maybe.map migrate_Types_Tier)

        Evergreen.V36.Types.UrlChange p0 ->
            Evergreen.V37.Types.UrlChange p0

        Evergreen.V36.Types.UrlRequest p0 ->
            Evergreen.V37.Types.UrlRequest p0

        Evergreen.V36.Types.ToggleKind p0 ->
            Evergreen.V37.Types.ToggleKind (p0 |> migrate_Types_Kind)

        Evergreen.V36.Types.Password p0 ->
            Evergreen.V37.Types.Password (p0 |> migrate_Types_Password_Password)

        Evergreen.V36.Types.TryLogin ->
            Evergreen.V37.Types.TryLogin

        Evergreen.V36.Types.AdminMsg p0 ->
            Evergreen.V37.Types.AdminMsg (p0 |> migrate_Types_AdminMsg)

        Evergreen.V36.Types.Resize p0 p1 ->
            Evergreen.V37.Types.Resize p0 p1

        Evergreen.V36.Types.Compact p0 ->
            Evergreen.V37.Types.Compact p0


migrate_Types_InnerAdminModel : Evergreen.V36.Types.InnerAdminModel -> Evergreen.V37.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V36.Types.Listing ->
            Evergreen.V37.Types.Listing

        Evergreen.V36.Types.ListingUsers p0 ->
            Evergreen.V37.Types.ListingUsers (p0 |> Dict.map (\k -> migrate_Types_User))

        Evergreen.V36.Types.PasswordResetDone p0 p1 ->
            Evergreen.V37.Types.PasswordResetDone p0 p1

        Evergreen.V36.Types.Creating p0 ->
            Evergreen.V37.Types.Creating p0

        Evergreen.V36.Types.CreatingUser p0 ->
            Evergreen.V37.Types.CreatingUser p0

        Evergreen.V36.Types.CreateUserDone p0 p1 ->
            Evergreen.V37.Types.CreateUserDone p0 p1

        Evergreen.V36.Types.Editing p0 p1 p2 p3 ->
            Evergreen.V37.Types.Editing p0 p1 p2 p3

        Evergreen.V36.Types.Deleting p0 ->
            Evergreen.V37.Types.Deleting p0

        Evergreen.V36.Types.Renaming p0 p1 ->
            Evergreen.V37.Types.Renaming p0 p1


migrate_Types_InnerModel : Evergreen.V36.Types.InnerModel -> Evergreen.V37.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V36.Types.Homepage ->
            Evergreen.V37.Types.Homepage

        Evergreen.V36.Types.Loading p0 p1 ->
            Evergreen.V37.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V36.Types.NotFound p0 ->
            Evergreen.V37.Types.NotFound p0

        Evergreen.V36.Types.Login p0 ->
            Evergreen.V37.Types.Login
                { password = p0.password |> migrate_Types_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V36.Types.Admin p0 ->
            Evergreen.V37.Types.Admin (p0 |> migrate_Types_AdminModel)

        Evergreen.V36.Types.Loaded p0 ->
            Evergreen.V37.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , compact = p0.compact
                , data = p0.data |> migrate_Types_CYOA
                }


migrate_Types_Kind : Evergreen.V36.Types.Kind -> Evergreen.V37.Types.Kind
migrate_Types_Kind old =
    case old of
        Evergreen.V36.Types.TieredKind ->
            Evergreen.V37.Types.TieredKind

        Evergreen.V36.Types.SimpleKind ->
            Evergreen.V37.Types.SimpleKind


migrate_Types_Password_Password : Evergreen.V36.Types.Password.Password -> Evergreen.V37.Types.Password.Password
migrate_Types_Password_Password old =
    case old of
        Evergreen.V36.Types.Password.Password p0 ->
            Evergreen.V37.Types.Password.password p0


migrate_Types_Power : Evergreen.V36.Types.Power -> Evergreen.V37.Types.Power
migrate_Types_Power old =
    { label = old.label
    , id = old.id
    , cost = old.cost
    , replaces = old.replaces
    , requires = old.requires |> List.map migrate_Types_Requirement
    , description = old.description
    }


migrate_Types_Requirement : Evergreen.V36.Types.Requirement -> Evergreen.V37.Types.Requirement
migrate_Types_Requirement old =
    case old of
        Evergreen.V36.Types.Requirement p0 ->
            Evergreen.V37.Types.Requirement p0

        Evergreen.V36.Types.AtLeastXOf p0 p1 ->
            Evergreen.V37.Types.AtLeastXOf p0 (p1 |> List.map migrate_Types_Requirement)


migrate_Types_Section : Evergreen.V36.Types.Section -> Evergreen.V37.Types.Section
migrate_Types_Section old =
    { name = old.name
    , description = old.description
    , powers = old.powers |> List.map migrate_Types_Power
    }


migrate_Types_TBAuthenticated : Evergreen.V36.Types.TBAuthenticated -> Evergreen.V37.Types.TBAuthenticated
migrate_Types_TBAuthenticated old =
    case old of
        Evergreen.V36.Types.TBLogin ->
            Evergreen.V37.Types.TBLogin

        Evergreen.V36.Types.TBCreateCYOA p0 ->
            Evergreen.V37.Types.TBCreateCYOA p0

        Evergreen.V36.Types.TBRenameCYOA p0 p1 ->
            Evergreen.V37.Types.TBRenameCYOA p0 p1

        Evergreen.V36.Types.TBUpdateCYOA p0 p1 ->
            Evergreen.V37.Types.TBUpdateCYOA p0 (p1 |> List.map migrate_Types_Section)

        Evergreen.V36.Types.TBDeleteCYOA p0 ->
            Evergreen.V37.Types.TBDeleteCYOA p0

        Evergreen.V36.Types.TBListUsers ->
            Evergreen.V37.Types.TBListUsers

        Evergreen.V36.Types.TBCreateUser p0 ->
            Evergreen.V37.Types.TBCreateUser p0

        Evergreen.V36.Types.TBResetPassword p0 ->
            Evergreen.V37.Types.TBResetPassword p0


migrate_Types_Tier : Evergreen.V36.Types.Tier -> Evergreen.V37.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V36.Types.S ->
            Evergreen.V37.Types.S

        Evergreen.V36.Types.A ->
            Evergreen.V37.Types.A

        Evergreen.V36.Types.B ->
            Evergreen.V37.Types.B

        Evergreen.V36.Types.C ->
            Evergreen.V37.Types.C

        Evergreen.V36.Types.D ->
            Evergreen.V37.Types.D

        Evergreen.V36.Types.F ->
            Evergreen.V37.Types.F


migrate_Types_ToBackend : Evergreen.V36.Types.ToBackend -> Evergreen.V37.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V36.Types.TBGetCYOA p0 ->
            Evergreen.V37.Types.TBGetCYOA p0

        Evergreen.V36.Types.TBAuthenticated p0 p1 ->
            Evergreen.V37.Types.TBAuthenticated (p0 |> migrate_Types_Password_Password)
                (p1 |> migrate_Types_TBAuthenticated)


migrate_Types_ToFrontend : Evergreen.V36.Types.ToFrontend -> Evergreen.V37.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V36.Types.TFGotCYOA p0 p1 ->
            Evergreen.V37.Types.TFGotCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V36.Types.TFRenamedCYOA p0 p1 ->
            Evergreen.V37.Types.TFRenamedCYOA p0 p1

        Evergreen.V36.Types.TFDeletedCYOA p0 ->
            Evergreen.V37.Types.TFDeletedCYOA p0

        Evergreen.V36.Types.TFCYOAMissing p0 ->
            Evergreen.V37.Types.TFCYOAMissing p0

        Evergreen.V36.Types.TFAdmin p0 ->
            Evergreen.V37.Types.TFAdmin (p0 |> Dict.map (\k -> migrate_Types_CYOA))

        Evergreen.V36.Types.TFUsers p0 ->
            Evergreen.V37.Types.TFUsers (p0 |> Dict.map (\k -> migrate_Types_User))

        Evergreen.V36.Types.TFResetPassword p0 p1 ->
            Evergreen.V37.Types.TFResetPassword p0 p1

        Evergreen.V36.Types.TFCreatedUser p0 p1 ->
            Evergreen.V37.Types.TFCreatedUser p0 p1


migrate_Types_User : Evergreen.V36.Types.User -> Evergreen.V37.Types.User
migrate_Types_User old =
    { password = old.password |> migrate_Types_Password_Password
    }
