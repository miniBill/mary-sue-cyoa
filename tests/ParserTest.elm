module ParserTest exposing (atLeastXOf, roundtrips)

import CYOAParser
import Expect
import Fuzz exposing (Fuzzer)
import Parser
import Test exposing (Test, fuzz, test)
import Types exposing (CYOAId, Power, Requirement(..), Section)
import View.Admin as Admin


atLeastXOf : Test
atLeastXOf =
    test "Parses 'at least three of: ..." <|
        \_ ->
            Parser.run CYOAParser.requirementParser "at least three of: A Thousand Ships, Angelic Tones, Emerald Orbs, Perfect Hair, Size Difference, Dressing Room, Like Roses, Well Endowed"
                |> Expect.equal
                    (Ok
                        (AtLeastXOf 3
                            [ Requirement "A Thousand Ships"
                            , Requirement "Angelic Tones"
                            , Requirement "Emerald Orbs"
                            , Requirement "Perfect Hair"
                            , Requirement "Size Difference"
                            , Requirement "Dressing Room"
                            , Requirement "Like Roses"
                            , Requirement "Well Endowed"
                            ]
                        )
                    )


roundtrips : Test
roundtrips =
    fuzz (Fuzz.listOfLengthBetween 0 3 fuzzSection) "Main parser roundtrips" <|
        \sections ->
            sections
                |> List.map Admin.sectionToString
                |> String.join "\n"
                |> Parser.run CYOAParser.mainParser
                |> Expect.equal (Ok sections)


fuzzSection : Fuzzer Section
fuzzSection =
    Fuzz.map3 Section
        niceNonemptyString
        (Fuzz.list niceString)
        (Fuzz.listOfLengthBetween 1 3 (Fuzz.map Just fuzzPower))


fuzzPower : Fuzzer Power
fuzzPower =
    Fuzz.map6 Power
        niceNonemptyString
        fuzzCYOAId
        Fuzz.int
        (Fuzz.maybe fuzzCYOAId)
        (Fuzz.listOfLengthBetween 0 3 fuzzRequirement)
        niceString


fuzzCYOAId : Fuzzer CYOAId
fuzzCYOAId =
    niceNonemptyString


fuzzRequirement : Fuzzer Requirement
fuzzRequirement =
    Fuzz.oneOf
        [ Fuzz.map Requirement fuzzCYOAId
        , Fuzz.map2 AtLeastXOf
            Fuzz.int
            (Fuzz.listOfLengthBetween 1
                3
                (Fuzz.map Requirement fuzzCYOAId)
            )
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
