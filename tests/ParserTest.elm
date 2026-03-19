module ParserTest exposing (atLeastXOf, roundtrips)

import CYOA exposing (PowerData, PowerOrBreak(..), Requirement(..), Section, SectionElement(..))
import CYOAParser
import Expect
import Fuzz exposing (Fuzzer)
import Id exposing (Id)
import Parser
import Test exposing (Test, fuzz, test)


atLeastXOf : Test
atLeastXOf =
    test "Parses 'at least three of: ..." <|
        \_ ->
            Parser.run CYOAParser.requirementParser "at least three of: A Thousand Ships, Angelic Tones, Emerald Orbs, Perfect Hair, Size Difference, Dressing Room, Like Roses, Well Endowed"
                |> Expect.equal
                    (Ok
                        (AtLeastXOf 3
                            [ Id.fromString "A Thousand Ships"
                            , Id.fromString "Angelic Tones"
                            , Id.fromString "Emerald Orbs"
                            , Id.fromString "Perfect Hair"
                            , Id.fromString "Size Difference"
                            , Id.fromString "Dressing Room"
                            , Id.fromString "Like Roses"
                            , Id.fromString "Well Endowed"
                            ]
                        )
                    )


roundtrips : Test
roundtrips =
    fuzz (Fuzz.listOfLengthBetween 0 3 fuzzSection) "Main parser roundtrips" <|
        \sections ->
            sections
                |> CYOA.toString
                |> Parser.run CYOAParser.mainParser
                |> Expect.equal (Ok sections)


fuzzSection : Fuzzer Section
fuzzSection =
    Fuzz.map3 Section
        niceNonemptyString
        (Fuzz.list niceString)
        (Fuzz.listOfLengthBetween 1 3 (Fuzz.map (\data -> data |> Power |> PowerElement) fuzzPower))


fuzzPower : Fuzzer PowerData
fuzzPower =
    Fuzz.map6 PowerData
        niceNonemptyString
        fuzzId
        Fuzz.int
        (Fuzz.maybe fuzzId)
        (Fuzz.listOfLengthBetween 0 3 fuzzRequirement)
        niceString


fuzzId : Fuzzer (Id a)
fuzzId =
    Fuzz.map Id.fromString niceNonemptyString


fuzzRequirement : Fuzzer Requirement
fuzzRequirement =
    Fuzz.oneOf
        [ Fuzz.map Requirement fuzzId
        , Fuzz.map2 AtLeastXOf
            Fuzz.int
            (Fuzz.listOfLengthBetween 1 3 fuzzId)
        ]


niceNonemptyString : Fuzzer String
niceNonemptyString =
    Fuzz.map2 (\h t -> String.fromList (h :: t) |> String.trim)
        (Fuzz.oneOfValues
            (List.map
                Char.fromCode
                (List.range
                    (Char.toCode 'a')
                    (Char.toCode 'z')
                )
            )
        )
        (Fuzz.listOfLengthBetween 1 10 Fuzz.char)


niceString : Fuzzer String
niceString =
    Fuzz.string
