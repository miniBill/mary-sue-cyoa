module Evergreen.Migrate.V6 exposing (..)

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
import Evergreen.V5.Password
import Evergreen.V5.Types
import Evergreen.V6.Password
import Evergreen.V6.Types
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V5.Types.FrontendModel -> ModelMigration Evergreen.V6.Types.FrontendModel Evergreen.V6.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V5.Types.BackendModel -> ModelMigration Evergreen.V6.Types.BackendModel Evergreen.V6.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V5.Types.FrontendMsg -> MsgMigration Evergreen.V6.Types.FrontendMsg Evergreen.V6.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V5.Types.ToBackend -> MsgMigration Evergreen.V6.Types.ToBackend Evergreen.V6.Types.BackendMsg
toBackend old =
    MsgMigrated ( migrate_Types_ToBackend old, Cmd.none )


backendMsg : Evergreen.V5.Types.BackendMsg -> MsgMigration Evergreen.V6.Types.BackendMsg Evergreen.V6.Types.BackendMsg
backendMsg _ =
    MsgUnchanged


toFrontend : Evergreen.V5.Types.ToFrontend -> MsgMigration Evergreen.V6.Types.ToFrontend Evergreen.V6.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_BackendModel : Evergreen.V5.Types.BackendModel -> Evergreen.V6.Types.BackendModel
migrate_Types_BackendModel old =
    { cyoas = old.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
    , connections = old.connections
    }


migrate_Types_FrontendModel : Evergreen.V5.Types.FrontendModel -> Evergreen.V6.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    }


migrate_Password_Password : Evergreen.V5.Password.Password -> Evergreen.V6.Password.Password
migrate_Password_Password old =
    case old of
        Evergreen.V5.Password.Password p0 ->
            Evergreen.V6.Password.Password p0


migrate_Types_AdminMsg : Evergreen.V5.Types.AdminMsg -> Evergreen.V6.Types.AdminMsg
migrate_Types_AdminMsg old =
    case old of
        Evergreen.V5.Types.CreatePrepare p0 ->
            Evergreen.V6.Types.CreatePrepare p0

        Evergreen.V5.Types.CreateDo p0 ->
            Evergreen.V6.Types.CreateDo p0

        Evergreen.V5.Types.UpdatePrepare p0 p1 p2 p3 ->
            Evergreen.V6.Types.UpdatePrepare p0 p1 p2 p3

        Evergreen.V5.Types.UpdateDo p0 p1 ->
            Evergreen.V6.Types.UpdateDo p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V5.Types.RenamePrepare p0 p1 ->
            Evergreen.V6.Types.RenamePrepare p0 p1

        Evergreen.V5.Types.RenameDo p0 p1 ->
            Evergreen.V6.Types.RenameDo p0 p1

        Evergreen.V5.Types.DeletePrepare p0 ->
            Evergreen.V6.Types.DeletePrepare p0

        Evergreen.V5.Types.DeleteDo p0 ->
            Evergreen.V6.Types.DeleteDo p0

        Evergreen.V5.Types.List ->
            Evergreen.V6.Types.List


migrate_Types_CYOA : Evergreen.V5.Types.CYOA -> Evergreen.V6.Types.CYOA
migrate_Types_CYOA old =
    old |> List.map migrate_Types_Section


migrate_Types_Choices : Evergreen.V5.Types.Choices -> Evergreen.V6.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V5.Types.Tiered p0 ->
            Evergreen.V6.Types.Tiered (p0 |> Dict.map (\_ -> migrate_Types_Tier))

        Evergreen.V5.Types.Simple p0 ->
            Evergreen.V6.Types.Simple p0


migrate_Types_FrontendMsg : Evergreen.V5.Types.FrontendMsg -> Evergreen.V6.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V5.Types.ChooseTier p0 p1 ->
            Evergreen.V6.Types.ChooseTier p0 (p1 |> Maybe.map migrate_Types_Tier)

        Evergreen.V5.Types.UrlChange p0 ->
            Evergreen.V6.Types.UrlChange p0

        Evergreen.V5.Types.UrlRequest p0 ->
            Evergreen.V6.Types.UrlRequest p0

        Evergreen.V5.Types.ToggleKind p0 ->
            Evergreen.V6.Types.ToggleKind (p0 |> migrate_Types_Kind)

        Evergreen.V5.Types.Password p0 ->
            Evergreen.V6.Types.Password (p0 |> migrate_Password_Password)

        Evergreen.V5.Types.TryLogin ->
            Evergreen.V6.Types.TryLogin

        Evergreen.V5.Types.AdminMsg p0 ->
            Evergreen.V6.Types.AdminMsg (p0 |> migrate_Types_AdminMsg)


migrate_Types_InnerAdminModel : Evergreen.V5.Types.InnerAdminModel -> Evergreen.V6.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V5.Types.Listing ->
            Evergreen.V6.Types.Listing

        Evergreen.V5.Types.Creating p0 ->
            Evergreen.V6.Types.Creating p0

        Evergreen.V5.Types.Editing p0 p1 p2 p3 ->
            Evergreen.V6.Types.Editing p0 p1 p2 p3

        Evergreen.V5.Types.Deleting p0 ->
            Evergreen.V6.Types.Deleting p0

        Evergreen.V5.Types.Renaming p0 p1 ->
            Evergreen.V6.Types.Renaming p0 p1


migrate_Types_InnerModel : Evergreen.V5.Types.InnerModel -> Evergreen.V6.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V5.Types.Homepage ->
            Evergreen.V6.Types.Homepage

        Evergreen.V5.Types.Loading p0 p1 ->
            Evergreen.V6.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V5.Types.NotFound p0 ->
            Evergreen.V6.Types.NotFound p0

        Evergreen.V5.Types.Login p0 ->
            Evergreen.V6.Types.Login
                { password = p0.password |> migrate_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V5.Types.Admin p0 ->
            Evergreen.V6.Types.Admin
                { password = p0.password |> migrate_Password_Password
                , cyoas = p0.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
                , inner = p0.inner |> migrate_Types_InnerAdminModel
                }

        Evergreen.V5.Types.Loaded p0 ->
            Evergreen.V6.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , data = p0.data |> migrate_Types_CYOA
                }


migrate_Types_Kind : Evergreen.V5.Types.Kind -> Evergreen.V6.Types.Kind
migrate_Types_Kind old =
    case old of
        Evergreen.V5.Types.TieredKind ->
            Evergreen.V6.Types.TieredKind

        Evergreen.V5.Types.SimpleKind ->
            Evergreen.V6.Types.SimpleKind


migrate_Types_Power : Evergreen.V5.Types.Power -> Evergreen.V6.Types.Power
migrate_Types_Power old =
    { label = old.label
    , id = old.id
    , cost = old.cost
    , description = old.description
    , requires = old.requires |> List.map migrate_Types_Requirement
    }


migrate_Types_Requirement : String -> Evergreen.V6.Types.Requirement
migrate_Types_Requirement old =
    Evergreen.V6.Types.Requirement old


migrate_Types_Section : Evergreen.V5.Types.Section -> Evergreen.V6.Types.Section
migrate_Types_Section old =
    { name = old.name
    , description = old.description
    , powers = old.powers |> List.map migrate_Types_Power
    }


migrate_Types_TBAuthenticated : Evergreen.V5.Types.TBAuthenticated -> Evergreen.V6.Types.TBAuthenticated
migrate_Types_TBAuthenticated old =
    case old of
        Evergreen.V5.Types.TBLogin ->
            Evergreen.V6.Types.TBLogin

        Evergreen.V5.Types.TBCreateCYOA p0 ->
            Evergreen.V6.Types.TBCreateCYOA p0

        Evergreen.V5.Types.TBRenameCYOA p0 p1 ->
            Evergreen.V6.Types.TBRenameCYOA p0 p1

        Evergreen.V5.Types.TBUpdateCYOA p0 p1 ->
            Evergreen.V6.Types.TBUpdateCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V5.Types.TBDeleteCYOA p0 ->
            Evergreen.V6.Types.TBDeleteCYOA p0


migrate_Types_Tier : Evergreen.V5.Types.Tier -> Evergreen.V6.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V5.Types.S ->
            Evergreen.V6.Types.S

        Evergreen.V5.Types.A ->
            Evergreen.V6.Types.A

        Evergreen.V5.Types.B ->
            Evergreen.V6.Types.B

        Evergreen.V5.Types.C ->
            Evergreen.V6.Types.C

        Evergreen.V5.Types.D ->
            Evergreen.V6.Types.D

        Evergreen.V5.Types.F ->
            Evergreen.V6.Types.F


migrate_Types_ToBackend : Evergreen.V5.Types.ToBackend -> Evergreen.V6.Types.ToBackend
migrate_Types_ToBackend old =
    case old of
        Evergreen.V5.Types.TBGetCYOA p0 ->
            Evergreen.V6.Types.TBGetCYOA p0

        Evergreen.V5.Types.TBAuthenticated p0 p1 ->
            Evergreen.V6.Types.TBAuthenticated (p0 |> migrate_Password_Password)
                (p1 |> migrate_Types_TBAuthenticated)


migrate_Types_ToFrontend : Evergreen.V5.Types.ToFrontend -> Evergreen.V6.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V5.Types.TFGotCYOA p0 p1 ->
            Evergreen.V6.Types.TFGotCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V5.Types.TFRenamedCYOA p0 p1 ->
            Evergreen.V6.Types.TFRenamedCYOA p0 p1

        Evergreen.V5.Types.TFDeletedCYOA p0 ->
            Evergreen.V6.Types.TFDeletedCYOA p0

        Evergreen.V5.Types.TFCYOAMissing p0 ->
            Evergreen.V6.Types.TFCYOAMissing p0

        Evergreen.V5.Types.TFAdmin p0 ->
            Evergreen.V6.Types.TFAdmin (p0 |> Dict.map (\_ -> migrate_Types_CYOA))
