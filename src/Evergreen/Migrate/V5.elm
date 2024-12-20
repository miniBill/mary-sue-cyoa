module Evergreen.Migrate.V5 exposing (..)

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
import Evergreen.V1.Password
import Evergreen.V1.Types
import Evergreen.V5.Password
import Evergreen.V5.Types
import Lamdera.Migrations exposing (..)


frontendModel : Evergreen.V1.Types.FrontendModel -> ModelMigration Evergreen.V5.Types.FrontendModel Evergreen.V5.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V1.Types.BackendModel -> ModelMigration Evergreen.V5.Types.BackendModel Evergreen.V5.Types.BackendMsg
backendModel _ =
    ModelUnchanged


frontendMsg : Evergreen.V1.Types.FrontendMsg -> MsgMigration Evergreen.V5.Types.FrontendMsg Evergreen.V5.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V1.Types.ToBackend -> MsgMigration Evergreen.V5.Types.ToBackend Evergreen.V5.Types.BackendMsg
toBackend _ =
    MsgUnchanged


backendMsg : Evergreen.V1.Types.BackendMsg -> MsgMigration Evergreen.V5.Types.BackendMsg Evergreen.V5.Types.BackendMsg
backendMsg _ =
    MsgUnchanged


toFrontend : Evergreen.V1.Types.ToFrontend -> MsgMigration Evergreen.V5.Types.ToFrontend Evergreen.V5.Types.FrontendMsg
toFrontend _ =
    MsgUnchanged


migrate_Types_FrontendModel : Evergreen.V1.Types.FrontendModel -> Evergreen.V5.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    }


migrate_Password_Password : Evergreen.V1.Password.Password -> Evergreen.V5.Password.Password
migrate_Password_Password old =
    case old of
        Evergreen.V1.Password.Password p0 ->
            Evergreen.V5.Password.Password p0


migrate_Types_AdminMsg : Evergreen.V1.Types.AdminMsg -> Evergreen.V5.Types.AdminMsg
migrate_Types_AdminMsg old =
    case old of
        Evergreen.V1.Types.CreatePrepare p0 ->
            Evergreen.V5.Types.CreatePrepare p0

        Evergreen.V1.Types.CreateDo p0 ->
            Evergreen.V5.Types.CreateDo p0

        Evergreen.V1.Types.UpdatePrepare p0 p1 p2 ->
            Evergreen.V5.Types.UpdatePrepare p0
                p1
                p2
                False

        Evergreen.V1.Types.UpdateDo p0 p1 ->
            Evergreen.V5.Types.UpdateDo p0 p1

        Evergreen.V1.Types.RenamePrepare p0 p1 ->
            Evergreen.V5.Types.RenamePrepare p0 p1

        Evergreen.V1.Types.RenameDo p0 p1 ->
            Evergreen.V5.Types.RenameDo p0 p1

        Evergreen.V1.Types.DeletePrepare p0 ->
            Evergreen.V5.Types.DeletePrepare p0

        Evergreen.V1.Types.DeleteDo p0 ->
            Evergreen.V5.Types.DeleteDo p0

        Evergreen.V1.Types.List ->
            Evergreen.V5.Types.List


migrate_Types_Choices : Evergreen.V1.Types.Choices -> Evergreen.V5.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V1.Types.Tiered p0 ->
            Evergreen.V5.Types.Tiered (p0 |> Dict.map (\_ -> migrate_Types_Tier))

        Evergreen.V1.Types.Simple p0 ->
            Evergreen.V5.Types.Simple p0


migrate_Types_FrontendMsg : Evergreen.V1.Types.FrontendMsg -> Evergreen.V5.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V1.Types.ChooseTier p0 p1 ->
            Evergreen.V5.Types.ChooseTier p0 (p1 |> Maybe.map migrate_Types_Tier)

        Evergreen.V1.Types.UrlChange p0 ->
            Evergreen.V5.Types.UrlChange p0

        Evergreen.V1.Types.UrlRequest p0 ->
            Evergreen.V5.Types.UrlRequest p0

        Evergreen.V1.Types.ToggleKind p0 ->
            Evergreen.V5.Types.ToggleKind (p0 |> migrate_Types_Kind)

        Evergreen.V1.Types.Password p0 ->
            Evergreen.V5.Types.Password (p0 |> migrate_Password_Password)

        Evergreen.V1.Types.TryLogin ->
            Evergreen.V5.Types.TryLogin

        Evergreen.V1.Types.AdminMsg p0 ->
            Evergreen.V5.Types.AdminMsg (p0 |> migrate_Types_AdminMsg)


migrate_Types_InnerAdminModel : Evergreen.V1.Types.InnerAdminModel -> Evergreen.V5.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V1.Types.Listing ->
            Evergreen.V5.Types.Listing

        Evergreen.V1.Types.Creating p0 ->
            Evergreen.V5.Types.Creating p0

        Evergreen.V1.Types.Editing p0 p1 p2 ->
            Evergreen.V5.Types.Editing p0
                p1
                p2
                False

        Evergreen.V1.Types.Deleting p0 ->
            Evergreen.V5.Types.Deleting p0

        Evergreen.V1.Types.Renaming p0 p1 ->
            Evergreen.V5.Types.Renaming p0 p1


migrate_Types_InnerModel : Evergreen.V1.Types.InnerModel -> Evergreen.V5.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V1.Types.Homepage ->
            Evergreen.V5.Types.Homepage

        Evergreen.V1.Types.Loading p0 p1 ->
            Evergreen.V5.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V1.Types.NotFound p0 ->
            Evergreen.V5.Types.NotFound p0

        Evergreen.V1.Types.Login p0 ->
            Evergreen.V5.Types.Login
                { password = p0.password |> migrate_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V1.Types.Admin p0 ->
            Evergreen.V5.Types.Admin
                { password = p0.password |> migrate_Password_Password
                , cyoas = p0.cyoas
                , inner = p0.inner |> migrate_Types_InnerAdminModel
                }

        Evergreen.V1.Types.Loaded p0 ->
            Evergreen.V5.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , data = p0.data
                }


migrate_Types_Kind : Evergreen.V1.Types.Kind -> Evergreen.V5.Types.Kind
migrate_Types_Kind old =
    case old of
        Evergreen.V1.Types.TieredKind ->
            Evergreen.V5.Types.TieredKind

        Evergreen.V1.Types.SimpleKind ->
            Evergreen.V5.Types.SimpleKind


migrate_Types_Tier : Evergreen.V1.Types.Tier -> Evergreen.V5.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V1.Types.S ->
            Evergreen.V5.Types.S

        Evergreen.V1.Types.A ->
            Evergreen.V5.Types.A

        Evergreen.V1.Types.B ->
            Evergreen.V5.Types.B

        Evergreen.V1.Types.C ->
            Evergreen.V5.Types.C

        Evergreen.V1.Types.D ->
            Evergreen.V5.Types.D

        Evergreen.V1.Types.F ->
            Evergreen.V5.Types.F
