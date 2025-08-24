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
    fuzz (Fuzz.list fuzzSection) "Main parser roundtrips" <|
        \sections ->
            sections
                |> List.map Admin.sectionToString
                |> String.join "\n"
                |> Parser.run CYOAParser.mainParser
                |> Expect.equal (Ok sections)


fuzzSection : Fuzzer Section
fuzzSection =
    Fuzz.map3 Section
        niceString
        (Fuzz.list niceString)
        (Fuzz.list (Fuzz.maybe fuzzPower))


fuzzPower : Fuzzer Power
fuzzPower =
    Fuzz.map6 Power
        niceString
        fuzzCYOAId
        Fuzz.int
        (Fuzz.maybe niceString)
        (Fuzz.list (fuzzRequirement 2))
        niceString


fuzzCYOAId : Fuzzer CYOAId
fuzzCYOAId =
    niceString


fuzzRequirement : Int -> Fuzzer Requirement
fuzzRequirement budget =
    if budget <= 0 then
        Fuzz.map Requirement fuzzCYOAId

    else
        Fuzz.oneOf
            [ Fuzz.map Requirement fuzzCYOAId
            , Fuzz.map2 AtLeastXOf
                Fuzz.int
                (Fuzz.listOfLengthBetween 1
                    3
                    (Fuzz.lazy (\_ -> fuzzRequirement (budget - 1)))
                )
            ]


niceString : Fuzzer String
niceString =
    Fuzz.string
