module Utils.NonEmptyString exposing
    ( NonEmptyString
    , build
    , fromString
    , fuzzer
    , toString
    )

import Fuzz exposing (Fuzzer)
import Random
import Random.Char
import Random.Extra as Random
import Random.String
import Shrink


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



-- TESTING


fuzzer : Fuzzer NonEmptyString
fuzzer =
    let
        shrinker =
            Shrink.string |> Shrink.keepIf (\string -> String.length string > 0)
    in
    Fuzz.custom
        nonemptyGenerator
        shrinker
        |> Fuzz.map NonEmptyString


nonemptyGenerator : Random.Generator String
nonemptyGenerator =
    let
        lengthGenerator =
            Random.frequency
                ( 3, Random.int 1 10 )
                [ ( 1, Random.int 11 50 )
                , ( 1, Random.int 50 1000 )
                ]
    in
    lengthGenerator
        |> Random.andThen (\length -> Random.String.string length Random.Char.english)
