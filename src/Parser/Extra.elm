module Parser.Extra exposing (errorToHtml)

import Html exposing (Html)
import Html.Attributes
import Parser.Error


errorToHtml :
    Parser.Error.Extract inner problem
    -> String
    -> List (Parser.Error.DeadEnd inner problem)
    -> List (Html msg)
errorToHtml extract src deadEnds =
    let
        color : String -> Html msg -> Html msg
        color value child =
            Html.span [ Html.Attributes.style "color" value ] [ child ]
    in
    Parser.Error.renderError
        { text = Html.text
        , formatContext = color "cyan"
        , formatCaret = color "red"
        , newline = Html.br [] []
        , linesOfExtraContext = 3
        }
        extract
        src
        deadEnds
