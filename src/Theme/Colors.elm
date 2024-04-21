module Theme.Colors exposing (Hsla, colorToColor, darkViolet, hslaMap, missingRequisites, paleViolet, palerViolet, tierToColor, unselectedBackground, violet)

import Color
import Element exposing (Color)
import Types exposing (Tier(..))


missingRequisites : Color
missingRequisites =
    Element.rgb 1 0.7 0.7


darkViolet : Color
darkViolet =
    Element.rgb255 0x43 0x01 0x83


violet : Color.Color
violet =
    Color.rgb255 0x80 0x40 0xBF


palerViolet : Color
palerViolet =
    Element.rgb255 0xF6 0xEA 0xFE


paleViolet : Color
paleViolet =
    Element.rgb255 0xF3 0xE3 0xFE


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


unselectedBackground : Color
unselectedBackground =
    Element.rgb 0.9 0.9 1


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
