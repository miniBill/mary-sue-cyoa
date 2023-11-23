module Theme.Colors exposing (darkViolet, paleViolet, palerViolet, tierToColor, violet)

import Color
import Element exposing (Color, rgb255)
import Types exposing (Tier(..))


darkViolet : Color
darkViolet =
    rgb255 0x43 0x01 0x83


violet : Color
violet =
    rgb255 0x80 0x40 0xBF


palerViolet : Color
palerViolet =
    rgb255 0xF6 0xEA 0xFE


paleViolet : Color
paleViolet =
    rgb255 0xF3 0xE3 0xFE


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
