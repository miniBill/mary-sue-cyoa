module Evergreen.Migrate.V41 exposing (..)

import Dict
import Evergreen.V37.Types
import Evergreen.V37.Types.Password
import Evergreen.V41.Types
import Evergreen.V41.Types.Password
import Lamdera.Migrations exposing (..)
import List


frontendModel : Evergreen.V37.Types.FrontendModel -> ModelMigration Evergreen.V41.Types.FrontendModel Evergreen.V41.Types.FrontendMsg
frontendModel old =
    ModelMigrated ( migrate_Types_FrontendModel old, Cmd.none )


backendModel : Evergreen.V37.Types.BackendModel -> ModelMigration Evergreen.V41.Types.BackendModel Evergreen.V41.Types.BackendMsg
backendModel old =
    let
        newModel : Evergreen.V41.Types.BackendModel
        newModel =
            { cyoas =
                old.cyoas
                    |> Dict.map (\_ -> migrate_Types_CYOA)
            , connections = old.connections
            , users =
                old.users
                    |> Dict.map (\_ -> migrate_Types_User)
            }
    in
    ModelMigrated ( newModel, Cmd.none )


migrate_Types_User : Evergreen.V37.Types.User -> Evergreen.V41.Types.User
migrate_Types_User user =
    { password = migrate_Types_Password_Password user.password }


frontendMsg : Evergreen.V37.Types.FrontendMsg -> MsgMigration Evergreen.V41.Types.FrontendMsg Evergreen.V41.Types.FrontendMsg
frontendMsg _ =
    MsgUnchanged


toBackend : Evergreen.V37.Types.ToBackend -> MsgMigration Evergreen.V41.Types.ToBackend Evergreen.V41.Types.BackendMsg
toBackend _ =
    MsgUnchanged


backendMsg : Evergreen.V37.Types.BackendMsg -> MsgMigration Evergreen.V41.Types.BackendMsg Evergreen.V41.Types.BackendMsg
backendMsg _ =
    MsgUnchanged


toFrontend : Evergreen.V37.Types.ToFrontend -> MsgMigration Evergreen.V41.Types.ToFrontend Evergreen.V41.Types.FrontendMsg
toFrontend old =
    MsgMigrated ( migrate_Types_ToFrontend old, Cmd.none )


migrate_Types_FrontendModel : Evergreen.V37.Types.FrontendModel -> Evergreen.V41.Types.FrontendModel
migrate_Types_FrontendModel old =
    { key = old.key
    , inner = old.inner |> migrate_Types_InnerModel
    , deviceClass = old.deviceClass
    }


migrate_Types_AdminModel : Evergreen.V37.Types.AdminModel -> Evergreen.V41.Types.AdminModel
migrate_Types_AdminModel old =
    { password = old.password |> migrate_Types_Password_Password
    , cyoas = old.cyoas |> Dict.map (\_ -> migrate_Types_CYOA)
    , inner = old.inner |> migrate_Types_InnerAdminModel
    }


migrate_Types_CYOA : Evergreen.V37.Types.CYOA -> Evergreen.V41.Types.CYOA
migrate_Types_CYOA old =
    { sections = old.sections |> List.map migrate_Types_Section
    , userId =
        if old.userId == "admin" then
            "kappa"

        else
            old.userId
    }


migrate_Types_Choices : Evergreen.V37.Types.Choices -> Evergreen.V41.Types.Choices
migrate_Types_Choices old =
    case old of
        Evergreen.V37.Types.Tiered p0 ->
            Evergreen.V41.Types.Tiered (p0 |> Dict.map (\_ -> migrate_Types_Tier))

        Evergreen.V37.Types.Simple p0 ->
            Evergreen.V41.Types.Simple p0


migrate_Types_InnerAdminModel : Evergreen.V37.Types.InnerAdminModel -> Evergreen.V41.Types.InnerAdminModel
migrate_Types_InnerAdminModel old =
    case old of
        Evergreen.V37.Types.Listing ->
            Evergreen.V41.Types.Listing

        Evergreen.V37.Types.ListingUsers p0 ->
            Evergreen.V41.Types.ListingUsers (Dict.keys p0)

        Evergreen.V37.Types.PasswordResetDone p0 p1 ->
            Evergreen.V41.Types.PasswordResetDone p0 p1

        Evergreen.V37.Types.Creating p0 ->
            Evergreen.V41.Types.Creating p0

        Evergreen.V37.Types.CreatingUser p0 ->
            Evergreen.V41.Types.CreatingUser p0

        Evergreen.V37.Types.CreateUserDone p0 p1 ->
            Evergreen.V41.Types.CreateUserDone p0 p1

        Evergreen.V37.Types.Editing p0 p1 p2 p3 ->
            Evergreen.V41.Types.Editing p0 p1 p2 p3

        Evergreen.V37.Types.Deleting p0 ->
            Evergreen.V41.Types.Deleting p0

        Evergreen.V37.Types.Renaming p0 p1 ->
            Evergreen.V41.Types.Renaming p0 p1

        Evergreen.V37.Types.Transferring p0 p1 ->
            Evergreen.V41.Types.Transferring p0 p1


migrate_Types_InnerModel : Evergreen.V37.Types.InnerModel -> Evergreen.V41.Types.InnerModel
migrate_Types_InnerModel old =
    case old of
        Evergreen.V37.Types.Homepage ->
            Evergreen.V41.Types.Homepage

        Evergreen.V37.Types.Loading p0 p1 ->
            Evergreen.V41.Types.Loading p0 (p1 |> migrate_Types_Choices)

        Evergreen.V37.Types.NotFound p0 ->
            Evergreen.V41.Types.NotFound p0

        Evergreen.V37.Types.Login p0 ->
            Evergreen.V41.Types.Login
                { password = p0.password |> migrate_Types_Password_Password
                , loggingIn = p0.loggingIn
                }

        Evergreen.V37.Types.Admin p0 ->
            Evergreen.V41.Types.Admin (p0 |> migrate_Types_AdminModel)

        Evergreen.V37.Types.Loaded p0 ->
            Evergreen.V41.Types.Loaded
                { cyoaId = p0.cyoaId
                , choices = p0.choices |> migrate_Types_Choices
                , compact = p0.compact
                , data = p0.data |> migrate_Types_CYOA
                }


migrate_Types_Password_Password : Evergreen.V37.Types.Password.Password -> Evergreen.V41.Types.Password.Password
migrate_Types_Password_Password old =
    case old of
        Evergreen.V37.Types.Password.Password p0 ->
            Evergreen.V41.Types.Password.Password p0


migrate_Types_Power : Evergreen.V37.Types.Power -> Evergreen.V41.Types.Power
migrate_Types_Power old =
    { label = old.label
    , id = old.id
    , cost = old.cost
    , replaces = old.replaces
    , requires = old.requires |> List.map migrate_Types_Requirement
    , description = old.description
    }


migrate_Types_Requirement : Evergreen.V37.Types.Requirement -> Evergreen.V41.Types.Requirement
migrate_Types_Requirement old =
    case old of
        Evergreen.V37.Types.Requirement p0 ->
            Evergreen.V41.Types.Requirement p0

        Evergreen.V37.Types.AtLeastXOf p0 p1 ->
            Evergreen.V41.Types.AtLeastXOf p0 (p1 |> List.map migrate_Types_Requirement)


migrate_Types_Section : Evergreen.V37.Types.Section -> Evergreen.V41.Types.Section
migrate_Types_Section old =
    { name = old.name
    , description = old.description
    , powers = old.powers |> List.map migrate_Types_Power
    }


migrate_Types_Tier : Evergreen.V37.Types.Tier -> Evergreen.V41.Types.Tier
migrate_Types_Tier old =
    case old of
        Evergreen.V37.Types.S ->
            Evergreen.V41.Types.S

        Evergreen.V37.Types.A ->
            Evergreen.V41.Types.A

        Evergreen.V37.Types.B ->
            Evergreen.V41.Types.B

        Evergreen.V37.Types.C ->
            Evergreen.V41.Types.C

        Evergreen.V37.Types.D ->
            Evergreen.V41.Types.D

        Evergreen.V37.Types.F ->
            Evergreen.V41.Types.F


migrate_Types_ToFrontend : Evergreen.V37.Types.ToFrontend -> Evergreen.V41.Types.ToFrontend
migrate_Types_ToFrontend old =
    case old of
        Evergreen.V37.Types.TFGotCYOA p0 p1 ->
            Evergreen.V41.Types.TFGotCYOA p0 (p1 |> migrate_Types_CYOA)

        Evergreen.V37.Types.TFRenamedCYOA p0 p1 ->
            Evergreen.V41.Types.TFRenamedCYOA p0 p1

        Evergreen.V37.Types.TFTransferredCYOA p0 p1 ->
            Evergreen.V41.Types.TFTransferredCYOA p0 p1

        Evergreen.V37.Types.TFDeletedCYOA p0 ->
            Evergreen.V41.Types.TFDeletedCYOA p0

        Evergreen.V37.Types.TFCYOAMissing p0 ->
            Evergreen.V41.Types.TFCYOAMissing p0

        Evergreen.V37.Types.TFAdmin p0 ->
            Evergreen.V41.Types.TFAdmin (p0 |> Dict.map (\_ -> migrate_Types_CYOA))

        Evergreen.V37.Types.TFUsers p0 ->
            Evergreen.V41.Types.TFUsers (Dict.keys p0)

        Evergreen.V37.Types.TFResetPassword p0 p1 ->
            Evergreen.V41.Types.TFResetPassword p0 p1

        Evergreen.V37.Types.TFCreatedUser p0 p1 ->
            Evergreen.V41.Types.TFCreatedUser p0 p1
