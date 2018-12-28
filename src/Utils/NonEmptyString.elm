module Utils.NonEmptyString exposing (NonEmptyString, build, fromString, toString)


type NonEmptyString
    = NonEmptyString String


fromString : String -> Maybe NonEmptyString
fromString raw =
    case String.length raw of
        0 ->
            Nothing

        _ ->
            Just (NonEmptyString raw)


build : Char -> String -> NonEmptyString
build first rest =
    NonEmptyString (String.fromChar first ++ rest)


toString : NonEmptyString -> String
toString (NonEmptyString string) =
    string
