module View.Admin exposing (view)

import CYOAParser
import Dict exposing (Dict)
import Element exposing (DeviceClass, Element, alignRight, alignTop, el, fill, height, inFront, newTabLink, rgb, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Parser
import Set
import Theme
import Theme.Colors
import Types exposing (AdminMsg(..), CYOA, CYOAId, Choices(..), InnerAdminModel(..), Power, Section)
import Types.Password exposing (Password)
import Url
import Url.Builder
import View.CYOA


view : DeviceClass -> { password : Password, cyoas : Dict CYOAId CYOA, inner : InnerAdminModel } -> Element AdminMsg
view deviceClass admin =
    (case admin.inner of
        Listing ->
            viewAdminList admin.cyoas

        Creating cyoaId ->
            viewCreating cyoaId

        Editing cyoaId old current preview ->
            viewEditing deviceClass cyoaId old current preview

        Renaming _ _ ->
            [ Theme.centralMessage "branch 'Renaming _ _' not implemented" ]

        Deleting _ ->
            [ Theme.centralMessage "branch 'Deleting _' not implemented" ]
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

        saveButton : Maybe CYOA -> Element AdminMsg
        saveButton cyoa =
            Theme.button []
                { onPress =
                    if old == current then
                        Nothing

                    else
                        Maybe.map (UpdateDo cyoaId) cyoa
                , label = text "Save"
                }

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
                    Ok cyoa ->
                        View.CYOA.view deviceClass
                            Nothing
                            { choices = Simple Set.empty
                            , data = cyoa
                            }

                    Err e ->
                        errorView e
                ]

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
        Theme.row [ width fill, height fill ]
            [ inputBox
            , previewBox
            ]

      else
        inputBox
    ]


viewAdminList : Dict CYOAId CYOA -> List (Element AdminMsg)
viewAdminList cyoas =
    cyoas
        |> Dict.toList
        |> List.map
            (\( cyoaId, cyoa ) ->
                let
                    raw : String
                    raw =
                        cyoaToString cyoa

                    url : String
                    url =
                        Url.Builder.absolute [ Url.percentEncode cyoaId ] []
                in
                Theme.row []
                    [ Theme.button []
                        { label = text cyoaId
                        , onPress = Just <| UpdatePrepare cyoaId raw raw False
                        }
                    , text <| "Link:"
                    , newTabLink
                        [ Font.underline
                        , Font.color Theme.Colors.violet
                        ]
                        { url = url
                        , label = text <| "https://mary-sue.lamdera.app" ++ url
                        }
                    ]
            )


cyoaToString : CYOA -> String
cyoaToString sections =
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
    ]
        |> Theme.row []
