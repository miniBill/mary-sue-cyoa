module Theme exposing (button, centralMessage, column, darkViolet, padding, paleViolet, palerViolet, row, rythm, spacing, violet)

import Element exposing (Attribute, Color, Element, centerX, centerY, el, rgb, rgb255, text)
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
                :: Border.color (rgb 0 0 0)
                :: Background.color palerViolet
                :: Font.color darkViolet
                :: attrs
            )
            config.label

    else
        Input.button
            (padding
                :: Border.width 1
                :: Border.color (rgb 0 0 0)
                :: Background.color violet
                :: Font.color (rgb 1 1 1)
                :: attrs
            )
            config


darkViolet : Color
darkViolet =
    rgb255 0x43 0x01 0x83


violet : Color
violet =
    rgb255 0x80 0x40 0xBF


palerViolet : Color
palerViolet =
    Element.rgb255 0xF6 0xEA 0xFE


paleViolet : Color
paleViolet =
    Element.rgb255 0xF3 0xE3 0xFE
