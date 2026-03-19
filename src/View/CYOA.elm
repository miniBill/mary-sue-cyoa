module View.CYOA exposing (view)

import CYOA exposing (PowerData, PowerOrBreak(..), Requirement(..), Section, SectionElement(..))
import Color
import Color.Oklch exposing (Oklch)
import Element exposing (Attribute, DeviceClass, Element, alignBottom, alignRight, alignTop, centerX, el, fill, height, paddingEach, paragraph, rgb, scrollbarY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import EnglishNumbers
import Html
import Id exposing (Id)
import List.Extra
import Markdown.Parser
import Markdown.Renderer
import SeqDict exposing (SeqDict)
import SeqSet
import Theme
import Theme.Colors
import Types exposing (Choices(..), Tier(..))


view : DeviceClass -> Maybe (Id PowerOrBreak -> Maybe Tier -> msg) -> { a | choices : Choices, data : List Section, compact : Bool } -> Element msg
view deviceClass chooseTier innerModel =
    let
        alternatives : SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak))
        alternatives =
            Types.getAlternatives innerModel.data
    in
    Theme.column
        [ scrollbarY
        , paddingEach
            { top = 0
            , left = Theme.rhythm
            , right = Theme.rhythm
            , bottom = Theme.rhythm
            }
        , height fill
        , height (Element.minimum 1 fill)
        ]
        (List.map
            (viewSection deviceClass alternatives chooseTier innerModel.choices innerModel.compact)
            innerModel.data
        )


viewSection : DeviceClass -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> Maybe (Id PowerOrBreak -> Maybe Tier -> msg) -> Choices -> Bool -> Section -> Element msg
viewSection deviceClass alternatives chooseTier choices compact section =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width fill
        ]
        (paragraph [ Font.bold ] [ viewMarkdown section.name ]
            :: List.map (\line -> paragraph [] [ viewMarkdown line ]) section.description
            ++ List.map (viewSectionElement deviceClass alternatives chooseTier choices compact) section.elements
        )


viewSectionElement : DeviceClass -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> Maybe (Id PowerOrBreak -> Maybe Tier -> msg) -> Choices -> Bool -> SectionElement -> Element msg
viewSectionElement deviceClass alternatives chooseTier choices compact element =
    case element of
        PowerElement Break ->
            Element.none

        PowerElement (Power power) ->
            viewGroup deviceClass alternatives chooseTier choices compact ( power, [] )

        Subsection subsection ->
            Theme.column
                [ Border.width 1
                , Theme.padding
                , width fill
                ]
                (paragraph [ Font.bold ] [ viewMarkdown subsection.name ]
                    :: List.map (\line -> paragraph [] [ viewMarkdown line ]) subsection.description
                    ++ List.map
                        (viewGroup deviceClass alternatives chooseTier choices compact)
                        (Types.groupPowers subsection.powers)
                )


viewGroup : DeviceClass -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> Maybe (Id PowerOrBreak -> Maybe Tier -> msg) -> Choices -> Bool -> ( PowerData, List PowerData ) -> Element msg
viewGroup deviceClass alternatives chooseTier choices compact ( power, powers ) =
    case powers of
        [] ->
            viewPower deviceClass alternatives [] { tiersBelow = False } chooseTier choices compact power

        _ ->
            (power :: powers)
                |> List.map (viewPower deviceClass alternatives [ height fill ] { tiersBelow = True } chooseTier choices compact)
                |> Theme.wrappedRow [ width fill ]


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


viewPower : DeviceClass -> SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> List (Attribute msg) -> { tiersBelow : Bool } -> Maybe (Id PowerOrBreak -> Maybe Tier -> msg) -> Choices -> Bool -> PowerData -> Element msg
viewPower deviceClass alternatives attrs { tiersBelow } chooseTier choices compact power =
    let
        currentTier : Maybe Tier
        currentTier =
            Types.powerTier choices power.id
    in
    if compact && currentTier == Nothing then
        Element.none

    else
        let
            label : List (Element msg) -> Element msg
            label children =
                Theme.column
                    [ width fill
                    , height fill
                    , alignTop
                    ]
                    [ Theme.wrappedRow
                        [ width fill
                        , if currentTier == Nothing || allRequirementsSatisfied then
                            width fill

                          else
                            Font.color <| rgb 1 0 0
                        ]
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

                                -- TODO: lookup label
                                , text (Id.toString replaces)
                                , text ")"
                                ]
                    , if List.isEmpty power.requires then
                        Element.none

                      else
                        paragraph [ Font.italic ] <|
                            text "Requires: "
                                :: List.Extra.intercalate
                                    [ text " and " ]
                                    (List.map (viewRequirement alternatives choices currentTier) power.requires)
                    , if tiersBelow then
                        descriptionRows
                            ++ [ Theme.row [ centerX, alignBottom ] children ]
                            |> Theme.column [ width fill, height fill ]

                      else if deviceClass == Element.Phone then
                        Theme.column [ width fill ]
                            [ Theme.column [ width fill, height fill ] descriptionRows
                            , Theme.row [ centerX, alignBottom ] children
                            ]

                      else
                        Theme.row [ width fill ] <|
                            Theme.column [ width fill, height fill ] descriptionRows
                                :: children
                    ]

            descriptionRows : List (Element msg)
            descriptionRows =
                power.description
                    |> String.split "\n"
                    |> List.map
                        (\line ->
                            paragraph [ width fill ]
                                [ viewMarkdown line ]
                        )

            common : List (Attribute msg)
            common =
                (if currentTier == Nothing || allRequirementsSatisfied then
                    let
                        backgroundColor : Oklch
                        backgroundColor =
                            case choices of
                                Tiered _ ->
                                    case currentTier of
                                        Just tier ->
                                            if allRequirementsSatisfied then
                                                let
                                                    tierColor : Oklch
                                                    tierColor =
                                                        Theme.Colors.tierToColor tier
                                                in
                                                { tierColor | lightness = 0.85 }

                                            else
                                                Theme.Colors.missingRequisites

                                        Nothing ->
                                            Theme.Colors.unselectedBackground

                                Simple _ ->
                                    if currentTier == Nothing then
                                        Theme.Colors.unselectedBackground

                                    else
                                        Color.rgb 0.7 1 0.7 |> Color.Oklch.fromColor
                    in
                    [ Border.width 1
                    , Theme.Colors.background backgroundColor
                    ]

                 else
                    [ Border.color <| rgb 1 0 0
                    , Border.width 2
                    , case choices of
                        Tiered _ ->
                            Background.color <| rgb 0.6 0.6 0.6

                        Simple _ ->
                            Background.color <| rgb 1 0.6 0.6
                    ]
                )
                    ++ [ Theme.padding
                       , width <| Element.minimum 250 fill
                       ]
                    ++ attrs

            toMsg : Maybe Tier -> Maybe msg
            toMsg tier =
                Maybe.map (\t -> t power.id tier) chooseTier

            allRequirementsSatisfied : Bool
            allRequirementsSatisfied =
                List.all (isRequirementSatisfied alternatives choices) power.requires
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
                el common <|
                    label tierButtons

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


viewRequirement : SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> Choices -> Maybe Tier -> Requirement -> List (Element msg)
viewRequirement alternatives choices currentTier topLevelRequirement =
    let
        allRequirementsSatisfied : Bool
        allRequirementsSatisfied =
            isRequirementSatisfied alternatives choices topLevelRequirement

        go : Requirement -> List (Element msg)
        go requirement =
            case requirement of
                Requirement req ->
                    [ el
                        [ Font.color <|
                            if isRequirementSatisfied alternatives choices requirement then
                                rgb 0.4 0.6 0

                            else if currentTier == Nothing || allRequirementsSatisfied then
                                rgb 0.6 0.4 0

                            else
                                rgb 1 0 0
                        ]
                        (case SeqDict.get req alternatives of
                            Just (first :: rest) ->
                                -- TODO: resolve Ids
                                text <| Id.toString first ++ " (or " ++ String.join ", or " (List.map Id.toString rest) ++ ")"

                            _ ->
                                text (Id.toString req)
                        )
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
                                    if String.contains "," (Id.toString child) then
                                        text "(" :: go (Requirement child) ++ [ text ")" ]

                                    else
                                        go (Requirement child)
                                )
                                children
                            )
    in
    go topLevelRequirement


isRequirementSatisfied : SeqDict (Id PowerOrBreak) (List (Id PowerOrBreak)) -> Choices -> Requirement -> Bool
isRequirementSatisfied alternatives choices requirement =
    let
        findWith : (Id PowerOrBreak -> b -> Bool) -> b -> Id PowerOrBreak -> Bool
        findWith member collection powerId =
            member powerId collection
                || List.any
                    (\power -> member power collection)
                    (Maybe.withDefault [] <| SeqDict.get powerId alternatives)
    in
    case ( choices, requirement ) of
        ( Simple simple, Requirement name ) ->
            findWith SeqSet.member simple name

        ( Tiered tiered, Requirement name ) ->
            findWith SeqDict.member tiered name

        ( Simple simple, AtLeastXOf required names ) ->
            let
                got : Int
                got =
                    List.Extra.count
                        (\name -> findWith SeqSet.member simple name)
                        names
            in
            got >= required

        ( Tiered tiered, AtLeastXOf required names ) ->
            let
                got : Int
                got =
                    List.Extra.count
                        (\name -> findWith SeqDict.member tiered name)
                        names
            in
            got >= required
