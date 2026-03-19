module View.Admin exposing (view)

import CYOA exposing (CYOA, Section)
import CYOAParser
import Element exposing (DeviceClass, Element, alignRight, alignTop, centerY, el, fill, height, inFront, paragraph, rgb, scrollbarX, shrink, spacing, table, text, width)
import Element.Font as Font
import Element.Input as Input
import Html
import Id exposing (Id)
import Parser
import Parser.Extra
import SeqDict exposing (SeqDict)
import SeqSet
import Theme
import Types exposing (AdminMsg(..), Choices(..), InnerAdminModel(..), User)
import Types.Password exposing (Password)
import Url
import Url.Builder
import View.CYOA


view : DeviceClass -> { password : Password, cyoas : SeqDict (Id CYOA) ( Id User, CYOA ), inner : InnerAdminModel } -> Element AdminMsg
view deviceClass admin =
    (case admin.inner of
        Listing ->
            viewAdminList admin.cyoas

        ListingUsers users ->
            viewUserList users

        CreatingUser user ->
            viewCreatingUser user

        Creating cyoaId ->
            viewCreating cyoaId

        Editing cyoaId old current preview ->
            viewEditing deviceClass cyoaId old current preview

        Renaming from to ->
            viewRenaming from to

        Transferring cyoaId userId ->
            viewTransferring cyoaId userId

        Deleting _ ->
            [ Theme.centralMessage "branch 'Deleting _' not implemented" ]

        PasswordResetDone userId password ->
            [ paragraph []
                [ text <| "Password reset successfull: user " ++ Id.toString userId ++ " now has password "
                , el [ Font.family [ Font.monospace ] ] <| text password
                ]
            ]

        CreateUserDone userId password ->
            [ paragraph []
                [ text <| "User successfully created: user " ++ Id.toString userId ++ " with password "
                , el [ Font.family [ Font.monospace ] ] <| text password
                ]
            ]
    )
        |> (::) (topRow admin.inner)
        |> Theme.column
            [ Theme.padding
            , width fill
            , height fill
            ]


viewCreating : Id CYOA -> List (Element AdminMsg)
viewCreating cyoaId =
    [ Theme.input []
        { label = Input.labelAbove [] <| text "Id"
        , text = Id.toString cyoaId
        , placeholder = Just <| Input.placeholder [] <| text "bestest-notebook-ever"
        , onChange = Id.fromString >> CreatePrepare
        }
    , Theme.button []
        { label = text "Create"
        , onPress = Just <| CreateDo cyoaId
        }
    ]


viewCreatingUser : Id User -> List (Element AdminMsg)
viewCreatingUser userId =
    [ Theme.input []
        { label = Input.labelAbove [] <| text "Username"
        , text = Id.toString userId
        , placeholder = Just <| Input.placeholder [] <| text "username"
        , onChange = Id.fromString >> CreateUserPrepare
        }
    , Theme.button []
        { label = text "Create"
        , onPress = Just <| CreateUserDo userId
        }
    ]


viewRenaming : Id CYOA -> Id CYOA -> List (Element AdminMsg)
viewRenaming from to =
    [ Theme.input []
        { label = Input.labelAbove [] <| paragraph [] [ text <| "You are renaming ", el [ Font.family [ Font.monospace ] ] <| text (Id.toString from), text " to" ]
        , text = Id.toString to
        , placeholder = Just <| Input.placeholder [] <| text "bestest-notebook-ever"
        , onChange = Id.fromString >> RenamePrepare from
        }
    , Theme.button []
        { label = text "Rename"
        , onPress = Just <| RenameDo from to
        }
    ]


viewTransferring : Id CYOA -> Id User -> List (Element AdminMsg)
viewTransferring cyoaId userId =
    [ Theme.input []
        { label = Input.labelAbove [] <| paragraph [] [ text <| "You are transferring ", el [ Font.family [ Font.monospace ] ] <| text (Id.toString cyoaId), text " to" ]
        , text = Id.toString userId
        , placeholder = Just <| Input.placeholder [] <| text "admin"
        , onChange = Id.fromString >> TransferPrepare cyoaId
        }
    , Theme.button []
        { label = text "Transfer"
        , onPress = Just <| TransferDo cyoaId userId
        }
    ]


viewEditing : DeviceClass -> Id CYOA -> String -> String -> Bool -> List (Element AdminMsg)
viewEditing deviceClass cyoaId old current preview =
    let
        inputBox : Element AdminMsg
        inputBox =
            Theme.multiline
                [ alignTop
                , width fill
                , height <| Element.maximum 6969 fill
                ]
                { label = Input.labelAbove [] <| text "Content"
                , text = current
                , onChange = \newValue -> UpdatePrepare cyoaId old newValue preview
                , placeholder = Nothing
                , spellcheck = True
                }

        parsed : Result (List Parser.DeadEnd) (List Section)
        parsed =
            Parser.run CYOAParser.mainParser (current ++ "\n\n")

        saveButton : Maybe (List Section) -> Element AdminMsg
        saveButton sections =
            Theme.button []
                { onPress =
                    if old == current then
                        Nothing

                    else
                        Maybe.map (UpdateDo cyoaId) sections
                , label = text "Save"
                }

        errorView : List Parser.DeadEnd -> Element msg
        errorView e =
            el [ Font.color <| rgb 1 0 0 ] <|
                Element.html <|
                    Html.pre []
                        (Parser.Extra.errorToHtml Parser.Extra.parser current e)
    in
    [ Theme.row
        [ width fill
        , inFront <|
            Theme.row [ alignRight ]
                [ case parsed of
                    Ok cyoa ->
                        saveButton (Just cyoa)

                    Err e ->
                        if preview then
                            saveButton Nothing

                        else
                            errorView e
                , Theme.button []
                    { onPress = Just <| UpdatePrepare cyoaId old current (not preview)
                    , label = text "Preview"
                    }
                ]
        ]
        [ el [ Font.bold ] <| text <| "Editing " ++ Id.toString cyoaId
        ]
    , if preview then
        let
            previewBox : Element msg
            previewBox =
                Theme.column
                    [ alignTop
                    , width fill
                    , height fill
                    , spacing 26
                    , scrollbarX
                    ]
                    [ el [] Element.none
                    , case parsed of
                        Ok sections ->
                            View.CYOA.view deviceClass
                                Nothing
                                { choices = Simple SeqSet.empty
                                , data = sections
                                , compact = False
                                }

                        Err e ->
                            errorView e
                    ]
        in
        Theme.row [ width fill, height fill ]
            [ inputBox
            , previewBox
            ]

      else
        inputBox
    ]


viewAdminList : SeqDict (Id CYOA) ( Id User, CYOA ) -> List (Element AdminMsg)
viewAdminList cyoas =
    [ table [ Theme.spacing ]
        { columns =
            [ { header = text "Name"
              , width = shrink
              , view =
                    \( cyoaId, ( _, cyoa ) ) ->
                        let
                            raw : String
                            raw =
                                CYOA.toString cyoa
                        in
                        Theme.button []
                            { label = text (Id.toString cyoaId)
                            , onPress = Just <| UpdatePrepare cyoaId raw raw False
                            }
              }
            , { header = text "Link"
              , width = shrink
              , view =
                    \( cyoaId, _ ) ->
                        let
                            url : String
                            url =
                                Url.Builder.absolute [ Url.percentEncode (Id.toString cyoaId) ] []
                        in
                        Theme.newTabLink [ centerY ]
                            { url = url
                            , label = text <| "https://mary-sue.lamdera.app" ++ url
                            }
              }
            , { header = text "User"
              , width = shrink
              , view = \( _, ( userId, _ ) ) -> el [ centerY ] <| text (Id.toString userId)
              }
            , { header = text "Actions"
              , width = shrink
              , view =
                    \( cyoaId, ( userId, _ ) ) ->
                        Theme.row []
                            [ Theme.button []
                                { label = text "Rename"
                                , onPress = Just <| RenamePrepare cyoaId cyoaId
                                }
                            , Theme.button []
                                { label = text "Transfer"
                                , onPress = Just <| TransferPrepare cyoaId userId
                                }
                            ]
              }
            ]
        , data = SeqDict.toList cyoas
        }
    ]


viewUserList : List (Id User) -> List (Element AdminMsg)
viewUserList users =
    [ table [ Theme.spacing ]
        { columns =
            [ { header = text "Id"
              , width = shrink
              , view = \userId -> el [ centerY ] <| text (Id.toString userId)
              }
            , { header = text "Actions"
              , width = shrink
              , view =
                    \userId ->
                        Theme.button []
                            { label = text "Reset password"
                            , onPress = Just <| ResetPassword userId
                            }
              }
            ]
        , data = users
        }
    , Theme.button []
        { onPress = Just (CreateUserPrepare (Id.fromString ""))
        , label = text "Create"
        }
    ]


topRow : InnerAdminModel -> Element AdminMsg
topRow inner =
    [ Theme.button []
        { label = text "List"
        , onPress =
            case inner of
                Listing ->
                    Nothing

                _ ->
                    Just List
        }
    , Theme.button []
        { label = text "Create"
        , onPress =
            case inner of
                Creating _ ->
                    Nothing

                _ ->
                    Just <| CreatePrepare (Id.fromString "")
        }
    , case inner of
        Editing _ _ _ _ ->
            Theme.button []
                { label = text "Edit"
                , onPress = Nothing
                }

        _ ->
            Element.none
    , case inner of
        Renaming _ _ ->
            Theme.button []
                { label = text "Rename"
                , onPress = Nothing
                }

        _ ->
            Element.none
    , Theme.button [ alignRight ]
        { label = text "Users"
        , onPress =
            case inner of
                ListingUsers _ ->
                    Nothing

                _ ->
                    Just Users
        }
    ]
        |> Theme.row []
