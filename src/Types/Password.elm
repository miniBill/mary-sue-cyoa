module Types.Password exposing (Password, input, isCorrect, password)

import Element exposing (Element, text)
import Element.Input as Input


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
    Input.currentPassword []
        { onChange = Password
        , label = Input.labelAbove [] <| text "Password"
        , placeholder = Nothing
        , show = False
        , text = pwd
        }
