module ParserTest exposing (atLeastXOf)

import CYOAParser
import Expect
import Parser
import Test exposing (..)
import Types exposing (Requirement(..))


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
