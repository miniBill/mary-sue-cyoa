module EnglishNumbers exposing (fromString, toString)


toString : Int -> String
toString val =
    case val of
        0 ->
            "zero"

        1 ->
            "one"

        2 ->
            "two"

        3 ->
            "three"

        4 ->
            "four"

        5 ->
            "five"

        6 ->
            "six"

        7 ->
            "seven"

        8 ->
            "eight"

        9 ->
            "nine"

        10 ->
            "ten"

        11 ->
            "eleven"

        12 ->
            "twelve"

        13 ->
            "thirteen"

        14 ->
            "fourteen"

        15 ->
            "fifteen"

        16 ->
            "sixteen"

        17 ->
            "seventeen"

        18 ->
            "eighteen"

        19 ->
            "nineteen"

        20 ->
            "twenty"

        _ ->
            String.fromInt val


fromString : String -> Maybe Int
fromString val =
    case val of
        "zero" ->
            Just 0

        "one" ->
            Just 1

        "two" ->
            Just 2

        "three" ->
            Just 3

        "four" ->
            Just 4

        "five" ->
            Just 5

        "six" ->
            Just 6

        "seven" ->
            Just 7

        "eight" ->
            Just 8

        "nine" ->
            Just 9

        "ten" ->
            Just 10

        "eleven" ->
            Just 11

        "twelve" ->
            Just 12

        "thirteen" ->
            Just 13

        "fourteen" ->
            Just 14

        "fifteen" ->
            Just 15

        "sixteen" ->
            Just 16

        "seventeen" ->
            Just 17

        "eighteen" ->
            Just 18

        "nineteen" ->
            Just 19

        "twenty" ->
            Just 20

        _ ->
            Nothing
