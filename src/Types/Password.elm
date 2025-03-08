module Types.Password exposing (Password, input, isCorrect, password)

import Ui exposing (Element, text)
import Ui.Input as Input


type Password
    = Password String


isCorrect : Password -> Password -> Bool
isCorrect (Password l) (Password r) =
    l == r


password : String -> Password
password =
    Password


input : Password -> Element Password
input (Password pwd) =
    let
        label :
            { element : Element msg
            , id : Input.Label
            }
        label =
            Input.label "password" [] (text "Password")
    in
    Ui.column []
        [ label.element
        , Input.currentPassword []
            { onChange = Password
            , label = label.id
            , placeholder = Nothing
            , show = False
            , text = pwd
            }
        ]
