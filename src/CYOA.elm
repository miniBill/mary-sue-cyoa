module CYOA exposing (CYOA, PowerData, PowerOrBreak(..), Requirement(..), Section, SectionElement(..), empty, toString)

import EnglishNumbers
import Id exposing (Id)
import Rope exposing (Rope)
import Rope.Extra


type alias CYOA =
    List Section


empty : CYOA
empty =
    []


type alias Section =
    { name : String
    , description : List String
    , elements : List SectionElement
    }


type SectionElement
    = PowerElement PowerOrBreak
    | Subsection
        { name : String
        , description : List String
        , powers : List PowerOrBreak
        }


type PowerOrBreak
    = Power PowerData
    | Break


type alias PowerData =
    { label : String
    , id : Id PowerOrBreak
    , cost : Int
    , replaces : Maybe (Id PowerOrBreak)
    , requires : List Requirement
    , description : String
    }


type Requirement
    = Requirement (Id PowerOrBreak)
    | AtLeastXOf Int (List (Id PowerOrBreak))


toString : CYOA -> String
toString cyoa =
    toRope cyoa
        |> Rope.toList
        |> String.concat


toRope : CYOA -> Rope String
toRope cyoa =
    List.map sectionToRope cyoa
        |> Rope.Extra.join "\n\n"


sectionToRope : Section -> Rope String
sectionToRope section =
    [ Rope.singleton ("## " ++ section.name)
    , section.description
        |> List.map (\l -> Rope.fromList [ "\n", l, "\n" ])
        |> Rope.fromRopeList
    , section.elements
        |> List.map elementToRope
        |> Rope.fromRopeList
    ]
        |> Rope.fromRopeList


elementToRope : SectionElement -> Rope String
elementToRope element =
    case element of
        Subsection subsection ->
            [ Rope.singleton ("### " ++ subsection.name)
            , subsection.description
                |> List.map (\l -> Rope.fromList [ "\n", l, "\n" ])
                |> Rope.fromRopeList
            , subsection.powers
                |> List.map powerOrBreakToRope
                |> Rope.fromRopeList
            ]
                |> Rope.fromRopeList

        PowerElement power ->
            powerOrBreakToRope power


powerOrBreakToRope : PowerOrBreak -> Rope String
powerOrBreakToRope powerOrBreak =
    case powerOrBreak of
        Break ->
            Rope.singleton "<br>"

        Power p ->
            powerToRope p


powerToRope : PowerData -> Rope String
powerToRope power =
    let
        costString : String
        costString =
            if power.cost < 0 then
                "Grants: +" ++ String.fromInt -power.cost

            else
                "Cost: " ++ String.fromInt power.cost

        idLine : String
        idLine =
            if power.label == Id.toString power.id then
                ""

            else
                "Id: "
                    ++ Id.toString power.id
                    ++ "\n"

        replacesLine : String
        replacesLine =
            case power.replaces of
                Nothing ->
                    ""

                Just replaces ->
                    "(Replaces " ++ Id.toString replaces ++ ")\n"

        requiresLine : String
        requiresLine =
            if List.isEmpty power.requires then
                ""

            else
                "(Requires "
                    ++ String.join " and "
                        (List.map requirementToString power.requires)
                    ++ ")\n"
    in
    [ "Name: "
    , power.label
    , " - "
    , costString
    , " ☐\n"
    , idLine
    , replacesLine
    , requiresLine
    , power.description
    ]
        |> Rope.fromList


requirementToString : Requirement -> String
requirementToString requirement =
    case requirement of
        Requirement name ->
            Id.toString name

        AtLeastXOf required alternatives ->
            "at least "
                ++ EnglishNumbers.toString required
                ++ " of: "
                ++ String.join ", "
                    (List.map
                        (\a ->
                            let
                                child : String
                                child =
                                    Id.toString a
                            in
                            if String.contains "," child then
                                "(" ++ child ++ ")"

                            else
                                child
                        )
                        alternatives
                    )
