module Evergreen.Migrate.V31 exposing (..)

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
import Env
import Evergreen.V27.Types
import Evergreen.V27.Types.Password
import Evergreen.V31.Types
import Evergreen.V31.Types.Password
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V27.Types.FrontendModel -> ModelMigration Evergreen.V31.Types.FrontendModel Evergreen.V31.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V27.Types.BackendModel -> ModelMigration Evergreen.V31.Types.BackendModel Evergreen.V31.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V27.Types.FrontendMsg -> MsgMigration Evergreen.V31.Types.FrontendMsg Evergreen.V31.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V27.Types.ToBackend -> MsgMigration Evergreen.V31.Types.ToBackend Evergreen.V31.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V27.Types.BackendMsg -> MsgMigration Evergreen.V31.Types.BackendMsg Evergreen.V31.Types.BackendMsg
backendMsg old =
    MsgMigrated ( migrate_Types_BackendMsg old, Cmd.none )


toFrontend : Evergreen.V27.Types.ToFrontend -> MsgMigration Evergreen.V31.Types.ToFrontend Evergreen.V31.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_BackendModel : Evergreen.V27.Types.BackendModel -> Evergreen.V31.Types.BackendModel
migrate_Types_BackendModel old =
    { cyoas = old.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
    , connections = old.connections
    , users = Dict.singleton "admin" { password = Evergreen.V31.Types.Password.Password Env.password }
    }


migrate_Types_FrontendModel : Evergreen.V27.Types.FrontendModel -> Evergreen.V31.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    , deviceClass = old.deviceClass
    }


migrate_Types_AdminMsg : Evergreen.V27.Types.AdminMsg -> Evergreen.V31.Types.AdminMsg
migrate_Types_AdminMsg old =
    case old of
        Evergreen.V27.Types.CreatePrepare p0 ->
            Evergreen.V31.Types.CreatePrepare p0

        Evergreen.V27.Types.CreateDo p0 ->
            Evergreen.V31.Types.CreateDo p0

        Evergreen.V27.Types.UpdatePrepare p0 p1 p2 p3 ->
            Evergreen.V31.Types.UpdatePrepare p0 p1 p2 p3

        Evergreen.V27.Types.UpdateDo p0 p1 ->
            Evergreen.V31.Types.UpdateDo p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V27.Types.RenamePrepare p0 p1 ->
            Evergreen.V31.Types.RenamePrepare p0 p1

        Evergreen.V27.Types.RenameDo p0 p1 ->
            Evergreen.V31.Types.RenameDo p0 p1

        Evergreen.V27.Types.DeletePrepare p0 ->
            Evergreen.V31.Types.DeletePrepare p0

        Evergreen.V27.Types.DeleteDo p0 ->
            Evergreen.V31.Types.DeleteDo p0

        Evergreen.V27.Types.List ->
            Evergreen.V31.Types.List


migrate_Types_BackendMsg : Evergreen.V27.Types.BackendMsg -> Evergreen.V31.Types.BackendMsg
migrate_Types_BackendMsg old =
    case old of
        Evergreen.V27.Types.BackendDisconnected p0 ->
            Evergreen.V31.Types.BackendDisconnected p0


migrate_Types_CYOA : Evergreen.V27.Types.CYOA -> Evergreen.V31.Types.CYOA
migrate_Types_CYOA old =
    old |> List.map migrate_Types_Section


migrate_Types_Choices : Evergreen.V27.Types.Choices -> Evergreen.V31.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V27.Types.Tiered p0 ->
            Evergreen.V31.Types.Tiered (p0 |> Dict.map (\_ -> migrate_Types_Tier))

        Evergreen.V27.Types.Simple p0 ->
            Evergreen.V31.Types.Simple p0


migrate_Types_FrontendMsg : Evergreen.V27.Types.FrontendMsg -> Evergreen.V31.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V27.Types.ChooseTier p0 p1 ->
            Evergreen.V31.Types.ChooseTier p0 (p1 |> Maybe.map migrate_Types_Tier)

        Evergreen.V27.Types.UrlChange p0 ->
            Evergreen.V31.Types.UrlChange p0

        Evergreen.V27.Types.UrlRequest p0 ->
            Evergreen.V31.Types.UrlRequest p0

        Evergreen.V27.Types.ToggleKind p0 ->
            Evergreen.V31.Types.ToggleKind (p0 |> migrate_Types_Kind)

        Evergreen.V27.Types.Password p0 ->
            Evergreen.V31.Types.Password (p0 |> migrate_Types_Password_Password)

        Evergreen.V27.Types.TryLogin ->
            Evergreen.V31.Types.TryLogin

        Evergreen.V27.Types.AdminMsg p0 ->
            Evergreen.V31.Types.AdminMsg (p0 |> migrate_Types_AdminMsg)

        Evergreen.V27.Types.Resize p0 p1 ->
            Evergreen.V31.Types.Resize p0 p1

        Evergreen.V27.Types.Compact p0 ->
            Evergreen.V31.Types.Compact p0


migrate_Types_InnerModel : Evergreen.V27.Types.InnerModel -> Evergreen.V31.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V27.Types.Homepage ->
            Evergreen.V31.Types.Homepage

        Evergreen.V27.Types.Loading p0 p1 ->
            Evergreen.V31.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V27.Types.NotFound p0 ->
            Evergreen.V31.Types.NotFound p0

        Evergreen.V27.Types.Login p0 ->
            Evergreen.V31.Types.Login
                { password = p0.password |> migrate_Types_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V27.Types.Admin admin ->
            Evergreen.V31.Types.Admin (migrate_Types_AdminModel admin)

        Evergreen.V27.Types.Loaded p0 ->
            Evergreen.V31.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , compact = p0.compact
                , data = p0.data |> migrate_Types_CYOA
                }


migrate_Types_AdminModel :
    { password : Evergreen.V27.Types.Password.Password, cyoas : Dict.Dict Evergreen.V27.Types.CYOAId Evergreen.V27.Types.CYOA, inner : Evergreen.V27.Types.InnerAdminModel }
    ->
        { password : Evergreen.V31.Types.Password.Password
        , cyoas : Dict.Dict Evergreen.V31.Types.CYOAId Evergreen.V31.Types.CYOA
        , inner : Evergreen.V31.Types.InnerAdminModel
        }
migrate_Types_AdminModel admin =
    { cyoas = Dict.map (\_ -> migrate_Types_CYOA) admin.cyoas
    , password = migrate_Types_Password_Password admin.password
    , inner = migrate_Types_InnerAdminModel admin.inner
    }


migrate_Types_InnerAdminModel : Evergreen.V27.Types.InnerAdminModel -> Evergreen.V31.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V27.Types.Listing ->
            Evergreen.V31.Types.Listing

        Evergreen.V27.Types.Creating p0 ->
            Evergreen.V31.Types.Creating p0

        Evergreen.V27.Types.Editing p0 p1 p2 p3 ->
            Evergreen.V31.Types.Editing p0 p1 p2 p3

        Evergreen.V27.Types.Deleting p0 ->
            Evergreen.V31.Types.Deleting p0

        Evergreen.V27.Types.Renaming p0 p1 ->
            Evergreen.V31.Types.Renaming p0 p1


migrate_Types_Kind : Evergreen.V27.Types.Kind -> Evergreen.V31.Types.Kind
migrate_Types_Kind old =
    case old of
        Evergreen.V27.Types.TieredKind ->
            Evergreen.V31.Types.TieredKind

        Evergreen.V27.Types.SimpleKind ->
            Evergreen.V31.Types.SimpleKind


migrate_Types_Password_Password : Evergreen.V27.Types.Password.Password -> Evergreen.V31.Types.Password.Password
migrate_Types_Password_Password old =
    case old of
        Evergreen.V27.Types.Password.Password p0 ->
            Evergreen.V31.Types.Password.Password p0


migrate_Types_Power : Evergreen.V27.Types.Power -> Evergreen.V31.Types.Power
migrate_Types_Power old =
    { label = old.label
    , id = old.id
    , cost = old.cost
    , replaces = old.replaces
    , requires = old.requires |> List.map migrate_Types_Requirement
    , description = old.description
    }


migrate_Types_Requirement : Evergreen.V27.Types.Requirement -> Evergreen.V31.Types.Requirement
migrate_Types_Requirement old =
    case old of
        Evergreen.V27.Types.Requirement p0 ->
            Evergreen.V31.Types.Requirement p0

        Evergreen.V27.Types.AtLeastXOf p0 p1 ->
            Evergreen.V31.Types.AtLeastXOf p0 (p1 |> List.map migrate_Types_Requirement)


migrate_Types_Section : Evergreen.V27.Types.Section -> Evergreen.V31.Types.Section
migrate_Types_Section old =
    { name = old.name
    , description = old.description
    , powers = old.powers |> List.map migrate_Types_Power
    }


migrate_Types_TBAuthenticated : Evergreen.V27.Types.TBAuthenticated -> Evergreen.V31.Types.TBAuthenticated
migrate_Types_TBAuthenticated old =
    case old of
        Evergreen.V27.Types.TBLogin ->
            Evergreen.V31.Types.TBLogin

        Evergreen.V27.Types.TBCreateCYOA p0 ->
            Evergreen.V31.Types.TBCreateCYOA p0

        Evergreen.V27.Types.TBRenameCYOA p0 p1 ->
            Evergreen.V31.Types.TBRenameCYOA p0 p1

        Evergreen.V27.Types.TBUpdateCYOA p0 p1 ->
            Evergreen.V31.Types.TBUpdateCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V27.Types.TBDeleteCYOA p0 ->
            Evergreen.V31.Types.TBDeleteCYOA p0


migrate_Types_Tier : Evergreen.V27.Types.Tier -> Evergreen.V31.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V27.Types.S ->
            Evergreen.V31.Types.S

        Evergreen.V27.Types.A ->
            Evergreen.V31.Types.A

        Evergreen.V27.Types.B ->
            Evergreen.V31.Types.B

        Evergreen.V27.Types.C ->
            Evergreen.V31.Types.C

        Evergreen.V27.Types.D ->
            Evergreen.V31.Types.D

        Evergreen.V27.Types.F ->
            Evergreen.V31.Types.F


migrate_Types_ToBackend : Evergreen.V27.Types.ToBackend -> Evergreen.V31.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V27.Types.TBGetCYOA p0 ->
            Evergreen.V31.Types.TBGetCYOA p0

        Evergreen.V27.Types.TBAuthenticated p0 p1 ->
            Evergreen.V31.Types.TBAuthenticated (p0 |> migrate_Types_Password_Password)
                (p1 |> migrate_Types_TBAuthenticated)


migrate_Types_ToFrontend : Evergreen.V27.Types.ToFrontend -> Evergreen.V31.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V27.Types.TFGotCYOA p0 p1 ->
            Evergreen.V31.Types.TFGotCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V27.Types.TFRenamedCYOA p0 p1 ->
            Evergreen.V31.Types.TFRenamedCYOA p0 p1

        Evergreen.V27.Types.TFDeletedCYOA p0 ->
            Evergreen.V31.Types.TFDeletedCYOA p0

        Evergreen.V27.Types.TFCYOAMissing p0 ->
            Evergreen.V31.Types.TFCYOAMissing p0

        Evergreen.V27.Types.TFAdmin p0 ->
            Evergreen.V31.Types.TFAdmin (p0 |> Dict.map (\_ -> migrate_Types_CYOA))
