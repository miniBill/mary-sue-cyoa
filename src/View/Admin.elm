module View.Admin exposing (view)

import CYOAParser
import Dict exposing (Dict)
import Element exposing (DeviceClass, Element, alignRight, alignTop, centerY, el, fill, height, inFront, newTabLink, paragraph, rgb, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Parser
import Set
import Theme
import Theme.Colors
import Types exposing (AdminMsg(..), CYOA, CYOAId, Choices(..), InnerAdminModel(..), Power, Section, User, UserId)
import Types.Password exposing (Password)
import Url
import Url.Builder
import View.CYOA


view : DeviceClass -> { password : Password, cyoas : Dict CYOAId CYOA, inner : InnerAdminModel } -> Element AdminMsg
view deviceClass admin =
    (case admin.inner of
        Listing ->
            viewAdminList admin.cyoas

        ListingUsers users ->
            viewUserList users

        Creating cyoaId ->
            viewCreating cyoaId

        Editing cyoaId old current preview ->
            viewEditing deviceClass cyoaId old current preview

        Renaming from to ->
            viewRenaming from to

        Deleting _ ->
            [ Theme.centralMessage "branch 'Deleting _' not implemented" ]

        PasswordResetDone userId password ->
            [ paragraph []
                [ text <| "Password reset successfull: user " ++ userId ++ " now has password "
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


viewCreating : CYOAId -> List (Element AdminMsg)
viewCreating cyoaId =
    [ Input.text
        [ Background.color Theme.Colors.palerViolet
        ]
        { label = Input.labelAbove [] <| text "Id"
        , text = cyoaId
        , placeholder = Just <| Input.placeholder [] <| text "bestest-notebook-ever"
        , onChange = CreatePrepare
        }
    , Theme.button []
        { label = text "Create"
        , onPress = Just <| CreateDo cyoaId
        }
    ]


viewRenaming : CYOAId -> CYOAId -> List (Element AdminMsg)
viewRenaming from to =
    [ Input.text
        [ Background.color Theme.Colors.palerViolet
        ]
        { label = Input.labelAbove [] <| paragraph [] [ text <| "You are renaming ", el [ Font.family [ Font.monospace ] ] <| text from, text " to" ]
        , text = to
        , placeholder = Just <| Input.placeholder [] <| text "bestest-notebook-ever"
        , onChange = RenamePrepare from
        }
    , Theme.button []
        { label = text "Rename"
        , onPress = Just <| RenameDo from to
        }
    ]


viewEditing : DeviceClass -> CYOAId -> String -> String -> Bool -> List (Element AdminMsg)
viewEditing deviceClass cyoaId old current preview =
    let
        inputBox : Element AdminMsg
        inputBox =
            Input.multiline
                [ alignTop
                , width fill
                , height <| Element.maximum 6969 fill
                , Background.color Theme.Colors.palerViolet
                ]
                { label = Input.labelAbove [] <| text "Content"
                , text = current
                , onChange = \newValue -> UpdatePrepare cyoaId old newValue preview
                , placeholder = Nothing
                , spellcheck = True
                }

        parsed : Result (List Parser.DeadEnd) (List Section)
        parsed =
            Parser.run CYOAParser.mainParser (current ++ "\n")

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
                text <|
                    CYOAParser.errorToString e
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
        [ el [ Font.bold ] <| text <| "Editing " ++ cyoaId
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
                    ]
                    [ el [] Element.none
                    , case parsed of
                        Ok sections ->
                            View.CYOA.view deviceClass
                                Nothing
                                { choices = Simple Set.empty
                                , data = { sections = sections }
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


viewAdminList : Dict CYOAId CYOA -> List (Element AdminMsg)
viewAdminList cyoas =
    [ table [ Theme.spacing ]
        { columns =
            [ { header = text "Name"
              , width = shrink
              , view =
                    \( cyoaId, cyoa ) ->
                        let
                            raw : String
                            raw =
                                cyoaToString cyoa
                        in
                        Theme.button []
                            { label = text cyoaId
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
                                Url.Builder.absolute [ Url.percentEncode cyoaId ] []
                        in
                        newTabLink
                            [ Font.underline
                            , Font.color Theme.Colors.violet
                            , centerY
                            ]
                            { url = url
                            , label = text <| "https://mary-sue.lamdera.app" ++ url
                            }
              }
            , { header = text "Actions"
              , width = shrink
              , view =
                    \( cyoaId, _ ) ->
                        Theme.button []
                            { label = text "Rename"
                            , onPress = Just <| RenamePrepare cyoaId cyoaId
                            }
              }
            ]
        , data =
            cyoas
                |> Dict.toList
        }
    ]


viewUserList : Dict UserId User -> List (Element AdminMsg)
viewUserList users =
    [ table [ Theme.spacing ]
        { columns =
            [ { header = text "Id"
              , width = shrink
              , view = \( userId, _ ) -> el [ centerY ] <| text userId
              }
            , { header = text "Actions"
              , width = shrink
              , view =
                    \( userId, _ ) ->
                        Theme.button []
                            { label = text "Reset password"
                            , onPress = Just <| ResetPassword userId
                            }
              }
            ]
        , data = Dict.toList users
        }
    ]


cyoaToString : CYOA -> String
cyoaToString { sections } =
    sections
        |> List.map sectionToString
        |> String.join "\n"


sectionToString : Section -> String
sectionToString section =
    let
        blocks : List String
        blocks =
            section.name :: section.description ++ List.map viewPower section.powers
    in
    String.join "\n\n" blocks ++ "\n"


viewPower : Power -> String
viewPower power =
    let
        costString : String
        costString =
            if power.cost < 0 then
                "Grants: +" ++ String.fromInt -power.cost

            else
                "Cost: " ++ String.fromInt power.cost

        idLine : String
        idLine =
            if power.label == power.id then
                ""

            else
                "Id: "
                    ++ power.id
                    ++ "\n"

        replacesLine : String
        replacesLine =
            case power.replaces of
                Nothing ->
                    ""

                Just replaces ->
                    "(Replaces " ++ replaces ++ ")\n"

        requiresLine : String
        requiresLine =
            if List.isEmpty power.requires then
                ""

            else
                "(Requires "
                    ++ String.join " and "
                        (List.map Types.requirementToString power.requires)
                    ++ ")\n"
    in
    "Name: "
        ++ power.label
        ++ " - "
        ++ costString
        ++ " ☐\n"
        ++ idLine
        ++ replacesLine
        ++ requiresLine
        ++ power.description


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
                    Just <| CreatePrepare ""
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
