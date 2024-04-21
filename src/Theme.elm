module Theme exposing (button, centralMessage, column, input, multiline, newTabLink, padding, rhythm, row, spacing, tierButtonAttrs, wrappedRow)

import Element exposing (Attribute, Element, centerX, centerY, el, rgb, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Theme.Colors
import Types exposing (Tier)


padding : Attribute msg
padding =
    Element.padding rhythm


spacing : Attribute msg
spacing =
    Element.spacing rhythm


rhythm : number
rhythm =
    10


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Element.row (spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Element.wrappedRow (spacing :: attrs)


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


input :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
input attrs config =
    Input.text (Background.color Theme.Colors.palerViolet :: attrs) config


multiline :
    List (Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs config =
    Input.multiline (Background.color Theme.Colors.palerViolet :: attrs) config


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
                :: Background.color Theme.Colors.palerViolet
                :: Font.color Theme.Colors.darkViolet
                :: attrs
            )
            config.label

    else
        Input.button
            (padding
                :: Border.width 1
                :: Border.color (rgb 0 0 0)
                :: Background.color Theme.Colors.violet
                :: Font.color (rgb 1 1 1)
                :: attrs
            )
            config


tierButtonAttrs : Bool -> Tier -> List (Attribute msg)
tierButtonAttrs selected tier =
    [ if selected then
        Background.color <| Theme.Colors.colorToColor <| Theme.Colors.tierToColor tier

      else
        Background.color <| Theme.Colors.colorToColor <| Theme.Colors.hslaMap (\hsla -> { hsla | saturation = 0.2 }) <| Theme.Colors.tierToColor tier
    , padding
    , Border.width 1
    ]


newTabLink : List (Attribute msg) -> { url : String, label : Element msg } -> Element msg
newTabLink attrs config =
    Element.newTabLink
        (Font.underline
            :: Font.color Theme.Colors.violet
            :: attrs
        )
        config
