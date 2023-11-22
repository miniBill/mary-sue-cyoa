module CYOAViewer exposing (view)

import Element exposing (Attribute, Element, alignRight, el, fill, height, paddingEach, paragraph, rgb, scrollbarY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Theme
import Types exposing (Choices(..), Power, Section, Tier(..))


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
        (el [ Font.bold ] (text section.name)
            :: List.map (\line -> paragraph [] [ text line ]) section.description
            ++ List.map
                (viewPower chooseTier choices)
                section.powers
        )


viewPower : Maybe (String -> Maybe Tier -> msg) -> Choices -> Power -> Element msg
viewPower chooseTier choices power =
    let
        currentTier : Maybe Tier
        currentTier =
            Types.powerTier choices power.id

        missingPrereq : List String
        missingPrereq =
            List.filter
                (\name -> Types.powerTier choices name == Nothing)
                power.requires

        viewRequirement : String -> Element msg
        viewRequirement requirement =
            el
                [ Font.color <|
                    if List.member requirement missingPrereq then
                        if currentTier == Nothing then
                            rgb 0.6 0.4 0

                        else
                            rgb 1 0 0

                    else
                        rgb 0.4 0.6 0
                ]
                (text requirement)

        label : List (Element msg) -> Element msg
        label children =
            Theme.column [ width fill ]
                [ Theme.row [ width fill ]
                    [ el [ Font.bold ] <| text power.label
                    , el [ alignRight ] <|
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
                            :: List.intersperse
                                (text " and ")
                                (List.map viewRequirement power.requires)
                , Theme.row [ width fill ]
                    (paragraph [ width fill ]
                        [ text power.description ]
                        :: children
                    )
                ]

        common : List (Attribute msg)
        common =
            [ Theme.padding
            , Border.width 1
            , width fill
            ]

        toMsg : Maybe Tier -> Maybe msg
        toMsg tier =
            Maybe.map (\t -> t power.id tier) chooseTier
    in
    case choices of
        Tiered _ ->
            el
                ((Background.color <|
                    if currentTier == Nothing then
                        if List.isEmpty missingPrereq then
                            rgb 0.9 0.9 1

                        else
                            rgb 0.9 0.9 0.9

                    else if List.isEmpty missingPrereq then
                        rgb 0.7 1 0.7

                    else
                        rgb 1 0.7 0.7
                 )
                    :: common
                )
            <|
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
            Input.button
                ((Background.color <|
                    if currentTier == Nothing then
                        if List.isEmpty missingPrereq then
                            rgb 0.9 0.9 1

                        else
                            rgb 0.9 0.9 0.9

                    else if List.isEmpty missingPrereq then
                        rgb 0.7 1 0.7

                    else
                        rgb 1 0.7 0.7
                 )
                    :: common
                )
                { onPress =
                    toMsg <|
                        if currentTier == Nothing then
                            Just S

                        else
                            Nothing
                , label = label []
                }
