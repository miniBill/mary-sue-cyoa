module Theme exposing (almostPaleViolet, button, centralMessage, column, darkViolet, padding, paleViolet, palerViolet, rhythm, row, spacing, tierButtonAttrs, violet, wrappedRow)

import Color
import Element exposing (Attribute, Color, Element, centerX, centerY, el, rgb, rgb255, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


almostPaleViolet : Color
almostPaleViolet =
    Element.rgb255 0x8C 0x4B 0xCB


paleViolet : Color
paleViolet =
    Element.rgb255 0xF3 0xE3 0xFE


tierButtonAttrs : Bool -> Tier -> List (Attribute msg)
tierButtonAttrs selected tier =
    [ if selected then
        Background.color <| colorToColor <| tierToColor tier

      else
        Background.color <| colorToColor <| hslaMap (\hsla -> { hsla | saturation = 0.2 }) <| tierToColor tier
    , padding
    , Border.width 1
    ]


hslaMap : (Hsla -> Hsla) -> Color.Color -> Color.Color
hslaMap f color =
    color
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
colorToColor color =
    let
        rgba : { red : Float, green : Float, blue : Float, alpha : Float }
        rgba =
            Color.toRgba color
    in
    Element.rgba rgba.red rgba.green rgba.blue rgba.alpha


tierToColor : Tier -> Color.Color
tierToColor tier =
    case tier of
        S ->
            Color.rgb255 0x02 0xAF 0xF0

        A ->
            Color.rgb255 0x00 0xAE 0x50

        B ->
            Color.rgb255 0x92 0xCF 0x50

        C ->
            Color.rgb255 0xFE 0xD9 0x66

        D ->
            Color.rgb255 0xF7 0x86 0x1C

        F ->
            Color.rgb255 0xAC 0x00 0x00
