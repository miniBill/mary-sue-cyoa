module Theme exposing (button, buttonAttr, centralMessage, column, input, multiline, newTabLink, padding, rhythm, row, spacing, tierButtonAttrs, wrappedRow)

import Color exposing (rgb)
import Color.Oklch exposing (Oklch)
import Theme.Colors
import Types exposing (Tier)
import Ui exposing (Attribute, Element, centerX, centerY, el, text)
import Ui.Font as Font
import Ui.Input as Input


padding : Attribute msg
padding =
    Ui.padding rhythm


spacing : Attribute msg
spacing =
    Ui.spacing rhythm


rhythm : number
rhythm =
    10


row : List (Attribute msg) -> List (Element msg) -> Element msg
row attrs =
    Ui.row (spacing :: attrs)


wrappedRow : List (Attribute msg) -> List (Element msg) -> Element msg
wrappedRow attrs =
    Ui.row (Ui.wrap :: spacing :: attrs)


column : List (Attribute msg) -> List (Element msg) -> Element msg
column attrs =
    Ui.column (spacing :: attrs)


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
        { id : String
        , onChange : String -> msg
        , text : String
        , label : Element msg
        , placeholder : Maybe String
        }
    -> Element msg
input attrs config =
    let
        label : { element : Element msg, id : Input.Label }
        label =
            Input.label config.id [] config.label
    in
    column []
        [ label.element
        , Input.text (Theme.Colors.background Theme.Colors.palerViolet :: attrs)
            { onChange = config.onChange
            , label = label.id
            , text = config.text
            , placeholder = config.placeholder
            }
        ]


multiline :
    List (Attribute msg)
    ->
        { id : String
        , onChange : String -> msg
        , text : String
        , label : Element msg
        , placeholder : Maybe String
        , spellcheck : Bool
        }
    -> Element msg
multiline attrs config =
    let
        label : { element : Element msg, id : Input.Label }
        label =
            Input.label config.id [] config.label
    in
    column []
        [ label.element
        , Input.multiline (Theme.Colors.background Theme.Colors.palerViolet :: attrs)
            { onChange = config.onChange
            , label = label.id
            , text = config.text
            , spellcheck = config.spellcheck
            , placeholder = config.placeholder
            }
        ]


button :
    List (Attribute msg)
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
button attrs config =
    let
        common : List (Attribute msg)
        common =
            padding
                :: Ui.border 1
                :: Ui.borderColor (rgb 0 0 0)
                :: attrs
    in
    case config.onPress of
        Nothing ->
            el
                (Theme.Colors.background Theme.Colors.palerViolet
                    :: Theme.Colors.font Theme.Colors.darkViolet
                    :: common
                )
                config.label

        Just msg ->
            Ui.el
                (Input.button msg
                    :: Theme.Colors.background Theme.Colors.violet
                    :: Font.color (rgb 1 1 1)
                    :: common
                )
                config.label


tierButtonAttrs : Bool -> Tier -> List (Attribute msg)
tierButtonAttrs selected tier =
    let
        tierColor : Oklch
        tierColor =
            Theme.Colors.tierToColor tier
    in
    [ if selected then
        Theme.Colors.background tierColor

      else
        Theme.Colors.background { tierColor | chroma = 0.05 }
    , padding
    , Ui.border 1
    ]


newTabLink : List (Attribute msg) -> { url : String, label : Element msg } -> Element msg
newTabLink attrs config =
    Ui.el
        (Ui.linkNewTab config.url
            :: Font.underline
            :: Theme.Colors.font Theme.Colors.violet
            :: attrs
        )
        config.label


buttonAttr : Maybe msg -> Attribute msg
buttonAttr maybeMsg =
    case maybeMsg of
        Just msg ->
            Input.button msg

        Nothing ->
            Ui.noAttr
