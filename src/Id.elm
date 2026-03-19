module Id exposing (Id, fromString, toString)


type Id a
    = Id String


fromString : String -> Id a
fromString =
    Id


toString : Id a -> String
toString (Id a) =
    a
