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
                (viewGroup chooseTier choices)
                (Types.groupPowers section.powers)
        )


viewGroup : Maybe (String -> Maybe Tier -> msg) -> Choices -> ( Power, List Power ) -> Element msg
viewGroup chooseTier choices ( power, powers ) =
    case powers of
        [] ->
            viewPower [] chooseTier choices power

        _ ->
            (power :: powers)
                |> List.map (viewPower [] chooseTier choices)
                |> Theme.row
                    [ width fill
                    , Border.width 1

                    -- , Theme.padding
                    -- , Background.color Theme.almostPaleViolet
                    ]


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


viewPower : List (Attribute msg) -> Maybe (String -> Maybe Tier -> msg) -> Choices -> Power -> Element msg
viewPower attrs chooseTier choices power =
    let
        currentTier : Maybe Tier
        currentTier =
            Types.powerTier choices power.id

        label : List (Element msg) -> Element msg
        label children =
            Theme.column [ width fill, alignTop ]
                [ Theme.wrappedRow [ width fill ]
                    [ paragraph [ Font.bold ] [ viewMarkdown power.label ]
                    , el [ alignRight, alignTop ] <|
                        text <|
                            if power.cost >= 0 then
                                "Cost: " ++ String.fromInt power.cost

                            else
                                "Grants: +" ++ String.fromInt -power.cost
                    ]
                , case power.replaces of
                    Nothing ->
                        Element.none

                    Just replaces ->
                        paragraph [ Font.italic ] <|
                            [ text "(Replaces "
                            , text replaces
                            , text ")"
                            ]
                , if List.isEmpty power.requires then
                    Element.none

                  else
                    paragraph [ Font.italic ] <|
                        text "Requires: "
                            :: List.Extra.intercalate
                                [ text " and " ]
                                (List.map (viewRequirement choices currentTier) power.requires)
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
                ++ attrs

        toMsg : Maybe Tier -> Maybe msg
        toMsg tier =
            Maybe.map (\t -> t power.id tier) chooseTier

        allRequirementsSatisfied : Bool
        allRequirementsSatisfied =
            List.all (isRequirementSatisfied choices) power.requires

        backgroundColor : Element.Color
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
            let
                tierButtons : List (Element msg)
                tierButtons =
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
            in
            el common <| label tierButtons

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


viewRequirement : Choices -> Maybe Tier -> Requirement -> List (Element msg)
viewRequirement choices currentTier topLevelRequirement =
    let
        allRequirementsSatisfied : Bool
        allRequirementsSatisfied =
            isRequirementSatisfied choices topLevelRequirement

        go : Requirement -> List (Element msg)
        go requirement =
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
                                        text "(" :: go child ++ [ text ")" ]

                                    else
                                        go child
                                )
                                children
                            )
    in
    go topLevelRequirement


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
