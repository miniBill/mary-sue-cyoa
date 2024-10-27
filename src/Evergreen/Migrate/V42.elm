module Evergreen.Migrate.V42 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Dict
import Evergreen.V41.Types
import Evergreen.V41.Types.Password
import Evergreen.V42.Types
import Evergreen.V42.Types.Password
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V41.Types.FrontendModel -> ModelMigration Evergreen.V42.Types.FrontendModel Evergreen.V42.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V41.Types.BackendModel -> ModelMigration Evergreen.V42.Types.BackendModel Evergreen.V42.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V41.Types.FrontendMsg -> MsgMigration Evergreen.V42.Types.FrontendMsg Evergreen.V42.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V41.Types.ToBackend -> MsgMigration Evergreen.V42.Types.ToBackend Evergreen.V42.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V41.Types.BackendMsg -> MsgMigration Evergreen.V42.Types.BackendMsg Evergreen.V42.Types.BackendMsg
backendMsg _ =
    MsgUnchanged


toFrontend : Evergreen.V41.Types.ToFrontend -> MsgMigration Evergreen.V42.Types.ToFrontend Evergreen.V42.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_BackendModel : Evergreen.V41.Types.BackendModel -> Evergreen.V42.Types.BackendModel
migrate_Types_BackendModel old =
    { cyoas = old.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
    , connections = old.connections
    , users = old.users |> Dict.map (\_ -> migrate_Types_User)
    }


migrate_Types_FrontendModel : Evergreen.V41.Types.FrontendModel -> Evergreen.V42.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    , deviceClass = old.deviceClass
    }


migrate_Types_AdminModel : Evergreen.V41.Types.AdminModel -> Evergreen.V42.Types.AdminModel
migrate_Types_AdminModel old =
    { password = old.password |> migrate_Types_Password_Password
    , cyoas = old.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
    , inner = old.inner |> migrate_Types_InnerAdminModel
    }


migrate_Types_AdminMsg : Evergreen.V41.Types.AdminMsg -> Evergreen.V42.Types.AdminMsg
migrate_Types_AdminMsg old =
    case old of
        Evergreen.V41.Types.CreatePrepare p0 ->
            Evergreen.V42.Types.CreatePrepare p0

        Evergreen.V41.Types.CreateDo p0 ->
            Evergreen.V42.Types.CreateDo p0

        Evergreen.V41.Types.CreateUserPrepare p0 ->
            Evergreen.V42.Types.CreateUserPrepare p0

        Evergreen.V41.Types.CreateUserDo p0 ->
            Evergreen.V42.Types.CreateUserDo p0

        Evergreen.V41.Types.UpdatePrepare p0 p1 p2 p3 ->
            Evergreen.V42.Types.UpdatePrepare p0 p1 p2 p3

        Evergreen.V41.Types.UpdateDo p0 p1 ->
            Evergreen.V42.Types.UpdateDo p0 (p1 |> List.map migrate_Types_Section)

        Evergreen.V41.Types.RenamePrepare p0 p1 ->
            Evergreen.V42.Types.RenamePrepare p0 p1

        Evergreen.V41.Types.RenameDo p0 p1 ->
            Evergreen.V42.Types.RenameDo p0 p1

        Evergreen.V41.Types.TransferPrepare p0 p1 ->
            Evergreen.V42.Types.TransferPrepare p0 p1

        Evergreen.V41.Types.TransferDo p0 p1 ->
            Evergreen.V42.Types.TransferDo p0 p1

        Evergreen.V41.Types.DeletePrepare p0 ->
            Evergreen.V42.Types.DeletePrepare p0

        Evergreen.V41.Types.DeleteDo p0 ->
            Evergreen.V42.Types.DeleteDo p0

        Evergreen.V41.Types.ResetPassword p0 ->
            Evergreen.V42.Types.ResetPassword p0

        Evergreen.V41.Types.List ->
            Evergreen.V42.Types.List

        Evergreen.V41.Types.Users ->
            Evergreen.V42.Types.Users


migrate_Types_CYOA : Evergreen.V41.Types.CYOA -> Evergreen.V42.Types.CYOA
migrate_Types_CYOA old =
    { sections = old.sections |> List.map migrate_Types_Section
    , userId = old.userId
    }


migrate_Types_Choices : Evergreen.V41.Types.Choices -> Evergreen.V42.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V41.Types.Tiered p0 ->
            Evergreen.V42.Types.Tiered (p0 |> Dict.map (\_ -> migrate_Types_Tier))

        Evergreen.V41.Types.Simple p0 ->
            Evergreen.V42.Types.Simple p0


migrate_Types_FrontendMsg : Evergreen.V41.Types.FrontendMsg -> Evergreen.V42.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V41.Types.ChooseTier p0 p1 ->
            Evergreen.V42.Types.ChooseTier p0 (p1 |> Maybe.map migrate_Types_Tier)

        Evergreen.V41.Types.UrlChange p0 ->
            Evergreen.V42.Types.UrlChange p0

        Evergreen.V41.Types.UrlRequest p0 ->
            Evergreen.V42.Types.UrlRequest p0

        Evergreen.V41.Types.ToggleKind p0 ->
            Evergreen.V42.Types.ToggleKind (p0 |> migrate_Types_Kind)

        Evergreen.V41.Types.Password p0 ->
            Evergreen.V42.Types.Password (p0 |> migrate_Types_Password_Password)

        Evergreen.V41.Types.TryLogin ->
            Evergreen.V42.Types.TryLogin

        Evergreen.V41.Types.AdminMsg p0 ->
            Evergreen.V42.Types.AdminMsg (p0 |> migrate_Types_AdminMsg)

        Evergreen.V41.Types.Resize p0 p1 ->
            Evergreen.V42.Types.Resize p0 p1

        Evergreen.V41.Types.Compact p0 ->
            Evergreen.V42.Types.Compact p0


migrate_Types_InnerAdminModel : Evergreen.V41.Types.InnerAdminModel -> Evergreen.V42.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V41.Types.Listing ->
            Evergreen.V42.Types.Listing

        Evergreen.V41.Types.ListingUsers p0 ->
            Evergreen.V42.Types.ListingUsers p0

        Evergreen.V41.Types.PasswordResetDone p0 p1 ->
            Evergreen.V42.Types.PasswordResetDone p0 p1

        Evergreen.V41.Types.Creating p0 ->
            Evergreen.V42.Types.Creating p0

        Evergreen.V41.Types.CreatingUser p0 ->
            Evergreen.V42.Types.CreatingUser p0

        Evergreen.V41.Types.CreateUserDone p0 p1 ->
            Evergreen.V42.Types.CreateUserDone p0 p1

        Evergreen.V41.Types.Editing p0 p1 p2 p3 ->
            Evergreen.V42.Types.Editing p0 p1 p2 p3

        Evergreen.V41.Types.Deleting p0 ->
            Evergreen.V42.Types.Deleting p0

        Evergreen.V41.Types.Renaming p0 p1 ->
            Evergreen.V42.Types.Renaming p0 p1

        Evergreen.V41.Types.Transferring p0 p1 ->
            Evergreen.V42.Types.Transferring p0 p1


migrate_Types_InnerModel : Evergreen.V41.Types.InnerModel -> Evergreen.V42.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V41.Types.Homepage ->
            Evergreen.V42.Types.Homepage

        Evergreen.V41.Types.Loading p0 p1 ->
            Evergreen.V42.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V41.Types.NotFound p0 ->
            Evergreen.V42.Types.NotFound p0

        Evergreen.V41.Types.Login p0 ->
            Evergreen.V42.Types.Login
                { password = p0.password |> migrate_Types_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V41.Types.Admin p0 ->
            Evergreen.V42.Types.Admin (p0 |> migrate_Types_AdminModel)

        Evergreen.V41.Types.Loaded p0 ->
            Evergreen.V42.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , compact = p0.compact
                , data = p0.data |> migrate_Types_CYOA
                }


migrate_Types_Kind : Evergreen.V41.Types.Kind -> Evergreen.V42.Types.Kind
migrate_Types_Kind old =
    case old of
        Evergreen.V41.Types.TieredKind ->
            Evergreen.V42.Types.TieredKind

        Evergreen.V41.Types.SimpleKind ->
            Evergreen.V42.Types.SimpleKind


migrate_Types_Password_Password : Evergreen.V41.Types.Password.Password -> Evergreen.V42.Types.Password.Password
migrate_Types_Password_Password old =
    case old of
        Evergreen.V41.Types.Password.Password p0 ->
            Evergreen.V42.Types.Password.Password p0


migrate_Types_Section : Evergreen.V41.Types.Section -> Evergreen.V42.Types.Section
migrate_Types_Section old =
    { name = old.name
    , description = old.description
    , powers = old.powers |> List.map (migrate_Types_Power >> Just)
    }


migrate_Types_Power : Evergreen.V41.Types.Power -> Evergreen.V42.Types.Power
migrate_Types_Power power =
    { label = power.label
    , id = power.id
    , cost = power.cost
    , replaces = power.replaces
    , description = power.description
    , requires = List.map migrate_Types_Requirement power.requires
    }


migrate_Types_Requirement : Evergreen.V41.Types.Requirement -> Evergreen.V42.Types.Requirement
migrate_Types_Requirement requirement =
    case requirement of
        Evergreen.V41.Types.Requirement id ->
            Evergreen.V42.Types.Requirement id

        Evergreen.V41.Types.AtLeastXOf n children ->
            Evergreen.V42.Types.AtLeastXOf n (List.map migrate_Types_Requirement children)


migrate_Types_TBAuthenticated : Evergreen.V41.Types.TBAuthenticated -> Evergreen.V42.Types.TBAuthenticated
migrate_Types_TBAuthenticated old =
    case old of
        Evergreen.V41.Types.TBLogin ->
            Evergreen.V42.Types.TBLogin

        Evergreen.V41.Types.TBCreateCYOA p0 ->
            Evergreen.V42.Types.TBCreateCYOA p0

        Evergreen.V41.Types.TBRenameCYOA p0 p1 ->
            Evergreen.V42.Types.TBRenameCYOA p0 p1

        Evergreen.V41.Types.TBTransferCYOA p0 p1 ->
            Evergreen.V42.Types.TBTransferCYOA p0 p1

        Evergreen.V41.Types.TBUpdateCYOA p0 p1 ->
            Evergreen.V42.Types.TBUpdateCYOA p0 (p1 |> List.map migrate_Types_Section)

        Evergreen.V41.Types.TBDeleteCYOA p0 ->
            Evergreen.V42.Types.TBDeleteCYOA p0

        Evergreen.V41.Types.TBListUsers ->
            Evergreen.V42.Types.TBListUsers

        Evergreen.V41.Types.TBCreateUser p0 ->
            Evergreen.V42.Types.TBCreateUser p0

        Evergreen.V41.Types.TBResetPassword p0 ->
            Evergreen.V42.Types.TBResetPassword p0


migrate_Types_Tier : Evergreen.V41.Types.Tier -> Evergreen.V42.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V41.Types.S ->
            Evergreen.V42.Types.S

        Evergreen.V41.Types.A ->
            Evergreen.V42.Types.A

        Evergreen.V41.Types.B ->
            Evergreen.V42.Types.B

        Evergreen.V41.Types.C ->
            Evergreen.V42.Types.C

        Evergreen.V41.Types.D ->
            Evergreen.V42.Types.D

        Evergreen.V41.Types.F ->
            Evergreen.V42.Types.F


migrate_Types_ToBackend : Evergreen.V41.Types.ToBackend -> Evergreen.V42.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V41.Types.TBGetCYOA p0 ->
            Evergreen.V42.Types.TBGetCYOA p0

        Evergreen.V41.Types.TBAuthenticated p0 p1 ->
            Evergreen.V42.Types.TBAuthenticated (p0 |> migrate_Types_Password_Password)
                (p1 |> migrate_Types_TBAuthenticated)


migrate_Types_ToFrontend : Evergreen.V41.Types.ToFrontend -> Evergreen.V42.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V41.Types.TFGotCYOA p0 p1 ->
            Evergreen.V42.Types.TFGotCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V41.Types.TFRenamedCYOA p0 p1 ->
            Evergreen.V42.Types.TFRenamedCYOA p0 p1

        Evergreen.V41.Types.TFTransferredCYOA p0 p1 ->
            Evergreen.V42.Types.TFTransferredCYOA p0 p1

        Evergreen.V41.Types.TFDeletedCYOA p0 ->
            Evergreen.V42.Types.TFDeletedCYOA p0

        Evergreen.V41.Types.TFCYOAMissing p0 ->
            Evergreen.V42.Types.TFCYOAMissing p0

        Evergreen.V41.Types.TFAdmin p0 ->
            Evergreen.V42.Types.TFAdmin (p0 |> Dict.map (\_ -> migrate_Types_CYOA))

        Evergreen.V41.Types.TFUsers p0 ->
            Evergreen.V42.Types.TFUsers p0

        Evergreen.V41.Types.TFResetPassword p0 p1 ->
            Evergreen.V42.Types.TFResetPassword p0 p1

        Evergreen.V41.Types.TFCreatedUser p0 p1 ->
            Evergreen.V42.Types.TFCreatedUser p0 p1


migrate_Types_User : Evergreen.V41.Types.User -> Evergreen.V42.Types.User
migrate_Types_User old =
    { password = old.password |> migrate_Types_Password_Password
    }
