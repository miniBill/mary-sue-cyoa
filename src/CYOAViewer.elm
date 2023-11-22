module CYOAViewer exposing (view)

import Dict
import Element exposing (Attribute, Element, alignRight, alignTop, el, fill, height, paddingEach, paragraph, rgb, scrollbarY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import EnglishNumbers
import Html
import List.Extra
import Markdown.Parser
import Markdown.Renderer
import Set
import Theme
import Types exposing (Choices(..), Power, Requirement(..), Section, Tier(..))


view : Maybe (String -> Maybe Tier -> msg) -> { a | choices : Choices, data : Types.CYOA } -> Element msg
view chooseTier innerModel =
    Theme.column
        [ scrollbarY
        , paddingEach
            { top = 0
            , left = Theme.rhythm
            , right = Theme.rhythm
            , bottom = Theme.rhythm
            }
        , height fill
        ]
        (List.map
            (viewSection chooseTier innerModel.choices)
            innerModel.data
        )


viewSection : Maybe (String -> Maybe Tier -> msg) -> Choices -> Section -> Element msg
viewSection chooseTier choices section =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        ]
        (paragraph [ Font.bold ] [ viewMarkdown section.name ]
            :: List.map (\line -> paragraph [] [ viewMarkdown line ]) section.description
            ++ List.map
                (viewPower chooseTier choices)
                section.powers
        )


viewMarkdown : String -> Element msg
viewMarkdown source =
    case Markdown.Parser.parse source of
        Err _ ->
            text source

        Ok markdown ->
            case
                Markdown.Renderer.render
                    Markdown.Renderer.defaultHtmlRenderer
                    markdown
            of
                Err _ ->
                    text source

                Ok [ html ] ->
                    Element.html html

                Ok html ->
                    Element.html <| Html.span [] html


viewPower : Maybe (String -> Maybe Tier -> msg) -> Choices -> Power -> Element msg
viewPower chooseTier choices power =
    let
        currentTier : Maybe Tier
        currentTier =
            Types.powerTier choices power.id

        viewRequirement : Requirement -> List (Element msg)
        viewRequirement requirement =
            case requirement of
                Requirement req ->
                    [ el
                        [ Font.color <|
                            if isRequirementSatisfied choices requirement then
                                rgb 0.4 0.6 0

                            else if currentTier == Nothing || allRequirementsSatisfied then
                                rgb 0.6 0.4 0

                            else
                                rgb 1 0 0
                        ]
                        (text req)
                    ]

                AtLeastXOf required children ->
                    text
                        ("at least "
                            ++ EnglishNumbers.toString required
                            ++ " of: "
                        )
                        :: List.Extra.intercalate
                            [ text ", " ]
                            (List.map
                                (\child ->
                                    let
                                        s : String
                                        s =
                                            Types.requirementToString child
                                    in
                                    if String.contains "," s then
                                        text "(" :: viewRequirement child ++ [ text ")" ]

                                    else
                                        viewRequirement child
                                )
                                children
                            )

        label : List (Element msg) -> Element msg
        label children =
            Theme.column [ width fill ]
                [ Theme.wrappedRow [ width fill ]
                    [ paragraph [ Font.bold ] [ viewMarkdown power.label ]
                    , el [ alignRight, alignTop ] <|
                        text <|
                            if power.cost >= 0 then
                                "Cost: " ++ String.fromInt power.cost

                            else
                                "Grants: +" ++ String.fromInt -power.cost
                    ]
                , if List.isEmpty power.requires then
                    Element.none

                  else
                    paragraph [ Font.italic ] <|
                        text "Requires: "
                            :: List.Extra.intercalate
                                [ text " and " ]
                                (List.map viewRequirement power.requires)
                , Theme.row [ width fill ]
                    (paragraph [ width fill ]
                        [ viewMarkdown power.description ]
                        :: children
                    )
                ]

        common : List (Attribute msg)
        common =
            [ Theme.padding
            , Border.width 1
            , width fill
            , Background.color backgroundColor
            ]

        toMsg : Maybe Tier -> Maybe msg
        toMsg tier =
            Maybe.map (\t -> t power.id tier) chooseTier

        allRequirementsSatisfied : Bool
        allRequirementsSatisfied =
            List.all (isRequirementSatisfied choices) power.requires

        backgroundColor =
            if currentTier == Nothing then
                if allRequirementsSatisfied then
                    rgb 0.9 0.9 1

                else
                    rgb 0.9 0.9 0.9

            else if allRequirementsSatisfied then
                rgb 0.7 1 0.7

            else
                rgb 1 0.7 0.7
    in
    case choices of
        Tiered _ ->
            el common <|
                label <|
                    List.map
                        (\tier ->
                            let
                                selected : Bool
                                selected =
                                    Just tier == currentTier
                            in
                            Input.button
                                (Theme.tierButtonAttrs selected tier)
                                { onPress =
                                    toMsg <|
                                        if selected then
                                            Nothing

                                        else
                                            Just tier
                                , label = text <| Types.tierToString tier
                                }
                        )
                        [ S, A, B, C, D, F ]

        Simple _ ->
            Input.button common
                { onPress =
                    toMsg <|
                        if currentTier == Nothing then
                            Just S

                        else
                            Nothing
                , label = label []
                }


isRequirementSatisfied : Choices -> Requirement -> Bool
isRequirementSatisfied choices requirement =
    case ( choices, requirement ) of
        ( Simple simple, Requirement name ) ->
            Set.member name simple

        ( Tiered tiered, Requirement name ) ->
            Dict.member name tiered

        ( _, AtLeastXOf required names ) ->
            let
                got : Int
                got =
                    List.Extra.count
                        (isRequirementSatisfied choices)
                        names
            in
            got >= required
