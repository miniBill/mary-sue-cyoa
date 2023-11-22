module Theme exposing (button, centralMessage, column, padding, row, rythm, spacing)

import Element exposing (Attribute, Element, centerX, centerY, el, rgb, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


padding : Attribute msg
padding =
    Element.padding rythm


spacing : Attribute msg
spacing =
    Element.spacing rythm


rythm : number
rythm =
    10


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Element.column (spacing :: attrs)


centralMessage : String -> Element msg
centralMessage message =
    el
        [ centerX
        , centerY
        , Font.size 48
        ]
        (text message)


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs config =
    if config.onPress == Nothing then
        el
            (padding
                :: Border.width 1
                :: Background.color (rgb 0.7 0.7 0.7)
                :: attrs
            )
            config.label

    else
        Input.button
            (padding
                :: Border.width 1
                :: Background.color (rgb 0.8 0.9 0.8)
                :: attrs
            )
            config
