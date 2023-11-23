module Theme exposing (button, centralMessage, column, padding, rhythm, row, spacing, tierButtonAttrs, wrappedRow)

import Color
import Element exposing (Attribute, Color, Element, centerX, centerY, el, rgb, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Theme.Colors
import Types exposing (Tier(..))


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
        Background.color <| colorToColor <| Theme.Colors.tierToColor tier

      else
        Background.color <| colorToColor <| hslaMap (\hsla -> { hsla | saturation = 0.2 }) <| Theme.Colors.tierToColor tier
    , padding
    , Border.width 1
    ]


hslaMap : (Hsla -> Hsla) -> Color.Color -> Color.Color
hslaMap f c =
    c
        |> Color.toHsla
        |> f
        |> Color.fromHsla


type alias Hsla =
    { alpha : Float
    , lightness : Float
    , hue : Float
    , saturation : Float
    }


colorToColor : Color.Color -> Color
colorToColor c =
    let
        rgba : { red : Float, green : Float, blue : Float, alpha : Float }
        rgba =
            Color.toRgba c
    in
    Element.rgba rgba.red rgba.green rgba.blue rgba.alpha
