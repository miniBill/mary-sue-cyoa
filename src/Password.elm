module Password exposing (Password, input, isCorrect, password)

import Element exposing (Element, text)
import Element.Input as Input
import Env


type Password
    = Password String


isCorrect : Password -> Bool
isCorrect (Password pwd) =
    pwd == Env.password


password : String -> Password
password =
    Password


input : Password -> Element Password
input (Password pwd) =
    Input.currentPassword []
        { onChange = Password
        , label = Input.labelAbove [] <| text "Password"
        , placeholder = Nothing
        , show = False
        , text = pwd
        }
