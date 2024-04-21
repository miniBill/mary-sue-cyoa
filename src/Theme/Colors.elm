module Theme.Colors exposing (background, darkViolet, font, missingRequisites, paleViolet, palerViolet, tierToColor, unselectedBackground, violet)

import Color
import Color.Oklch as Oklch exposing (Oklch)
import Element exposing (Attribute)
import Element.Background as Background
import Element.Font as Font
import Types exposing (Tier(..))


missingRequisites : Oklch
missingRequisites =
    Color.rgb 1 0.7 0.7
        |> Oklch.fromColor


darkViolet : Oklch
darkViolet =
    { violet | lightness = 0.33, chroma = 0.172 }


violet : Oklch
violet =
    Color.rgb255 0x80 0x40 0xBF
        |> Oklch.fromColor


palerViolet : Oklch
palerViolet =
    { violet | lightness = 0.95, chroma = 0.029 }


paleViolet : Oklch
paleViolet =
    { violet | lightness = 0.92, chroma = 0.04 }


tierToColor : Tier -> Oklch
tierToColor tier =
    (case tier of
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
    )
        |> Oklch.fromColor


unselectedBackground : Oklch
unselectedBackground =
    Color.rgb 0.9 0.9 1
        |> Oklch.fromColor


font : Oklch -> Attribute msg
font color =
    Font.color <| colorToColor <| Oklch.toColor color


background : Oklch -> Attribute msg
background color =
    Background.color <| colorToColor <| Oklch.toColor color


colorToColor : Color.Color -> Element.Color
colorToColor c =
    let
        rgba : { red : Float, green : Float, blue : Float, alpha : Float }
        rgba =
            Color.toRgba c
    in
    Element.rgba rgba.red rgba.green rgba.blue rgba.alpha
