module Evergreen.Migrate.V36 exposing (..)

import Dict
import Evergreen.V33.Types
import Evergreen.V33.Types.Password
import Evergreen.V36.Types
import Evergreen.V36.Types.Password
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V33.Types.FrontendModel -> ModelMigration Evergreen.V36.Types.FrontendModel Evergreen.V36.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V33.Types.BackendModel -> ModelMigration Evergreen.V36.Types.BackendModel Evergreen.V36.Types.BackendMsg
backendModel _ =
    ModelUnchanged


frontendMsg : Evergreen.V33.Types.FrontendMsg -> MsgMigration Evergreen.V36.Types.FrontendMsg Evergreen.V36.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V33.Types.ToBackend -> MsgMigration Evergreen.V36.Types.ToBackend Evergreen.V36.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V33.Types.BackendMsg -> MsgMigration Evergreen.V36.Types.BackendMsg Evergreen.V36.Types.BackendMsg
backendMsg old =
    MsgMigrated ( migrate_Types_BackendMsg old, Cmd.none )


toFrontend : Evergreen.V33.Types.ToFrontend -> MsgMigration Evergreen.V36.Types.ToFrontend Evergreen.V36.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_FrontendModel : Evergreen.V33.Types.FrontendModel -> Evergreen.V36.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    , deviceClass = old.deviceClass
    }


migrate_Types_AdminModel : Evergreen.V33.Types.AdminModel -> Evergreen.V36.Types.AdminModel
migrate_Types_AdminModel old =
    { password = old.password |> migrate_Types_Password_Password
    , cyoas = old.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
    , inner = old.inner |> migrate_Types_InnerAdminModel
    }


migrate_Types_AdminMsg : Evergreen.V33.Types.AdminMsg -> Evergreen.V36.Types.AdminMsg
migrate_Types_AdminMsg old =
    case old of
        Evergreen.V33.Types.CreatePrepare p0 ->
            Evergreen.V36.Types.CreatePrepare p0

        Evergreen.V33.Types.CreateDo p0 ->
            Evergreen.V36.Types.CreateDo p0

        Evergreen.V33.Types.UpdatePrepare p0 p1 p2 p3 ->
            Evergreen.V36.Types.UpdatePrepare p0 p1 p2 p3

        Evergreen.V33.Types.UpdateDo p0 p1 ->
            Evergreen.V36.Types.UpdateDo p0 (p1 |> List.map migrate_Types_Section)

        Evergreen.V33.Types.RenamePrepare p0 p1 ->
            Evergreen.V36.Types.RenamePrepare p0 p1

        Evergreen.V33.Types.RenameDo p0 p1 ->
            Evergreen.V36.Types.RenameDo p0 p1

        Evergreen.V33.Types.DeletePrepare p0 ->
            Evergreen.V36.Types.DeletePrepare p0

        Evergreen.V33.Types.DeleteDo p0 ->
            Evergreen.V36.Types.DeleteDo p0

        Evergreen.V33.Types.ResetPassword p0 ->
            Evergreen.V36.Types.ResetPassword p0

        Evergreen.V33.Types.List ->
            Evergreen.V36.Types.List

        Evergreen.V33.Types.Users ->
            Evergreen.V36.Types.Users


migrate_Types_BackendMsg : Evergreen.V33.Types.BackendMsg -> Evergreen.V36.Types.BackendMsg
migrate_Types_BackendMsg old =
    case old of
        Evergreen.V33.Types.BackendDisconnected p0 ->
            Evergreen.V36.Types.BackendDisconnected p0

        Evergreen.V33.Types.DoPasswordReset p0 p1 p2 ->
            Evergreen.V36.Types.DoPasswordReset p0 p1 p2


migrate_Types_CYOA : Evergreen.V33.Types.CYOA -> Evergreen.V36.Types.CYOA
migrate_Types_CYOA old =
    { sections = old.sections |> List.map migrate_Types_Section
    , userId = old.userId
    }


migrate_Types_Choices : Evergreen.V33.Types.Choices -> Evergreen.V36.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V33.Types.Tiered p0 ->
            Evergreen.V36.Types.Tiered (p0 |> Dict.map (\_ -> migrate_Types_Tier))

        Evergreen.V33.Types.Simple p0 ->
            Evergreen.V36.Types.Simple p0


migrate_Types_FrontendMsg : Evergreen.V33.Types.FrontendMsg -> Evergreen.V36.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V33.Types.ChooseTier p0 p1 ->
            Evergreen.V36.Types.ChooseTier p0 (p1 |> Maybe.map migrate_Types_Tier)

        Evergreen.V33.Types.UrlChange p0 ->
            Evergreen.V36.Types.UrlChange p0

        Evergreen.V33.Types.UrlRequest p0 ->
            Evergreen.V36.Types.UrlRequest p0

        Evergreen.V33.Types.ToggleKind p0 ->
            Evergreen.V36.Types.ToggleKind (p0 |> migrate_Types_Kind)

        Evergreen.V33.Types.Password p0 ->
            Evergreen.V36.Types.Password (p0 |> migrate_Types_Password_Password)

        Evergreen.V33.Types.TryLogin ->
            Evergreen.V36.Types.TryLogin

        Evergreen.V33.Types.AdminMsg p0 ->
            Evergreen.V36.Types.AdminMsg (p0 |> migrate_Types_AdminMsg)

        Evergreen.V33.Types.Resize p0 p1 ->
            Evergreen.V36.Types.Resize p0 p1

        Evergreen.V33.Types.Compact p0 ->
            Evergreen.V36.Types.Compact p0


migrate_Types_InnerAdminModel : Evergreen.V33.Types.InnerAdminModel -> Evergreen.V36.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V33.Types.Listing ->
            Evergreen.V36.Types.Listing

        Evergreen.V33.Types.ListingUsers p0 ->
            Evergreen.V36.Types.ListingUsers (p0 |> Dict.map (\_ -> migrate_Types_User))

        Evergreen.V33.Types.PasswordResetDone p0 p1 ->
            Evergreen.V36.Types.PasswordResetDone p0 p1

        Evergreen.V33.Types.Creating p0 ->
            Evergreen.V36.Types.Creating p0

        Evergreen.V33.Types.Editing p0 p1 p2 p3 ->
            Evergreen.V36.Types.Editing p0 p1 p2 p3

        Evergreen.V33.Types.Deleting p0 ->
            Evergreen.V36.Types.Deleting p0

        Evergreen.V33.Types.Renaming p0 p1 ->
            Evergreen.V36.Types.Renaming p0 p1


migrate_Types_InnerModel : Evergreen.V33.Types.InnerModel -> Evergreen.V36.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V33.Types.Homepage ->
            Evergreen.V36.Types.Homepage

        Evergreen.V33.Types.Loading p0 p1 ->
            Evergreen.V36.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V33.Types.NotFound p0 ->
            Evergreen.V36.Types.NotFound p0

        Evergreen.V33.Types.Login p0 ->
            Evergreen.V36.Types.Login
                { password = p0.password |> migrate_Types_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V33.Types.Admin p0 ->
            Evergreen.V36.Types.Admin (p0 |> migrate_Types_AdminModel)

        Evergreen.V33.Types.Loaded p0 ->
            Evergreen.V36.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , compact = p0.compact
                , data = p0.data |> migrate_Types_CYOA
                }


migrate_Types_Kind : Evergreen.V33.Types.Kind -> Evergreen.V36.Types.Kind
migrate_Types_Kind old =
    case old of
        Evergreen.V33.Types.TieredKind ->
            Evergreen.V36.Types.TieredKind

        Evergreen.V33.Types.SimpleKind ->
            Evergreen.V36.Types.SimpleKind


migrate_Types_Password_Password : Evergreen.V33.Types.Password.Password -> Evergreen.V36.Types.Password.Password
migrate_Types_Password_Password old =
    case old of
        Evergreen.V33.Types.Password.Password p0 ->
            Evergreen.V36.Types.Password.Password p0


migrate_Types_Power : Evergreen.V33.Types.Power -> Evergreen.V36.Types.Power
migrate_Types_Power old =
    { label = old.label
    , id = old.id
    , cost = old.cost
    , replaces = old.replaces
    , requires = old.requires |> List.map migrate_Types_Requirement
    , description = old.description
    }


migrate_Types_Requirement : Evergreen.V33.Types.Requirement -> Evergreen.V36.Types.Requirement
migrate_Types_Requirement old =
    case old of
        Evergreen.V33.Types.Requirement p0 ->
            Evergreen.V36.Types.Requirement p0

        Evergreen.V33.Types.AtLeastXOf p0 p1 ->
            Evergreen.V36.Types.AtLeastXOf p0 (p1 |> List.map migrate_Types_Requirement)


migrate_Types_Section : Evergreen.V33.Types.Section -> Evergreen.V36.Types.Section
migrate_Types_Section old =
    { name = old.name
    , description = old.description
    , powers = old.powers |> List.map migrate_Types_Power
    }


migrate_Types_TBAuthenticated : Evergreen.V33.Types.TBAuthenticated -> Evergreen.V36.Types.TBAuthenticated
migrate_Types_TBAuthenticated old =
    case old of
        Evergreen.V33.Types.TBLogin ->
            Evergreen.V36.Types.TBLogin

        Evergreen.V33.Types.TBCreateCYOA p0 ->
            Evergreen.V36.Types.TBCreateCYOA p0

        Evergreen.V33.Types.TBRenameCYOA p0 p1 ->
            Evergreen.V36.Types.TBRenameCYOA p0 p1

        Evergreen.V33.Types.TBUpdateCYOA p0 p1 ->
            Evergreen.V36.Types.TBUpdateCYOA p0 (p1 |> List.map migrate_Types_Section)

        Evergreen.V33.Types.TBDeleteCYOA p0 ->
            Evergreen.V36.Types.TBDeleteCYOA p0

        Evergreen.V33.Types.TBListUsers ->
            Evergreen.V36.Types.TBListUsers

        Evergreen.V33.Types.TBResetPassword p0 ->
            Evergreen.V36.Types.TBResetPassword p0


migrate_Types_Tier : Evergreen.V33.Types.Tier -> Evergreen.V36.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V33.Types.S ->
            Evergreen.V36.Types.S

        Evergreen.V33.Types.A ->
            Evergreen.V36.Types.A

        Evergreen.V33.Types.B ->
            Evergreen.V36.Types.B

        Evergreen.V33.Types.C ->
            Evergreen.V36.Types.C

        Evergreen.V33.Types.D ->
            Evergreen.V36.Types.D

        Evergreen.V33.Types.F ->
            Evergreen.V36.Types.F


migrate_Types_ToBackend : Evergreen.V33.Types.ToBackend -> Evergreen.V36.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V33.Types.TBGetCYOA p0 ->
            Evergreen.V36.Types.TBGetCYOA p0

        Evergreen.V33.Types.TBAuthenticated p0 p1 ->
            Evergreen.V36.Types.TBAuthenticated (p0 |> migrate_Types_Password_Password)
                (p1 |> migrate_Types_TBAuthenticated)


migrate_Types_ToFrontend : Evergreen.V33.Types.ToFrontend -> Evergreen.V36.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V33.Types.TFGotCYOA p0 p1 ->
            Evergreen.V36.Types.TFGotCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V33.Types.TFRenamedCYOA p0 p1 ->
            Evergreen.V36.Types.TFRenamedCYOA p0 p1

        Evergreen.V33.Types.TFDeletedCYOA p0 ->
            Evergreen.V36.Types.TFDeletedCYOA p0

        Evergreen.V33.Types.TFCYOAMissing p0 ->
            Evergreen.V36.Types.TFCYOAMissing p0

        Evergreen.V33.Types.TFAdmin p0 ->
            Evergreen.V36.Types.TFAdmin (p0 |> Dict.map (\_ -> migrate_Types_CYOA))

        Evergreen.V33.Types.TFUsers p0 ->
            Evergreen.V36.Types.TFUsers (p0 |> Dict.map (\_ -> migrate_Types_User))

        Evergreen.V33.Types.TFResetPassword p0 p1 ->
            Evergreen.V36.Types.TFResetPassword p0 p1


migrate_Types_User : Evergreen.V33.Types.User -> Evergreen.V36.Types.User
migrate_Types_User old =
    { password = old.password |> migrate_Types_Password_Password
    }
