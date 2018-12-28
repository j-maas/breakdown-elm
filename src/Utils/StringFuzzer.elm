module Utils.StringFuzzer exposing
    ( minimalLengthStringFuzzer
    , nonblankStringFuzzer
    , whitespaceStringFuzzer
    )

import Fuzz exposing (Fuzzer)
import Random
import Random.Char
import Random.Extra as Random
import Random.String
import Shrink


minimalLengthStringFuzzer : Int -> Fuzzer String
minimalLengthStringFuzzer length =
    Fuzz.custom
        (minimalLengthStringGenerator length)
        (Shrink.string |> Shrink.keepIf (\string -> String.length string > 0))


minimalLengthStringGenerator : Int -> Random.Generator String
minimalLengthStringGenerator minimalLength =
    -- Taken from elm-explorations/test#Fuzz.string
    let
        asciiGenerator : Random.Generator String
        asciiGenerator =
            Random.frequency
                ( 3, Random.int minimalLength (minimalLength + 10) )
                [ ( 1, Random.int (minimalLength + 11) (minimalLength + 50) )
                , ( 1, Random.int (minimalLength + 50) (minimalLength + 1000) )
                ]
                |> Random.andThen (\length -> Random.String.string length Random.Char.ascii)

        minimalLengthWhitespaceGenerator : Random.Generator String
        minimalLengthWhitespaceGenerator =
            Random.int minimalLength (minimalLength + 10)
                |> Random.andThen (\length -> Random.String.string length whitespace)
    in
    Random.frequency
        ( 9, asciiGenerator )
        [ ( 1, minimalLengthWhitespaceGenerator )
        ]


{-| Generates nonempty strings that contain at least one printable (i. e. not whitespace) character.
-}
nonblankStringFuzzer : Fuzzer String
nonblankStringFuzzer =
    Fuzz.custom
        nonblankGenerator
        (Shrink.string
            |> Shrink.keepIf
                (\string ->
                    let
                        trimmed =
                            String.trim string
                    in
                    String.length trimmed > 0
                )
        )


nonblankGenerator : Random.Generator String
nonblankGenerator =
    Random.frequency
        ( 3, Random.int 1 10 )
        [ ( 1, Random.int 11 50 )
        , ( 1, Random.int 50 1000 )
        ]
        |> Random.andThen
            (\length ->
                let
                    partLength =
                        length // 2 - 1

                    ascii =
                        Random.Char.ascii

                    nonblanks =
                        Random.Char.latin

                    build : String -> Char -> String -> String
                    build front nonblank back =
                        front ++ String.fromChar nonblank ++ back
                in
                Random.map3
                    build
                    (Random.String.string partLength ascii)
                    nonblanks
                    (Random.String.string partLength ascii)
            )


{-| Generates strings containing only whitespace.

The empty string is also produced.

-}
whitespaceStringFuzzer : Fuzzer String
whitespaceStringFuzzer =
    Fuzz.custom
        whitespaceGenerator
        Shrink.string


whitespaceGenerator : Random.Generator String
whitespaceGenerator =
    Random.frequency
        ( 3, Random.int 1 10 )
        [ ( 0.2, Random.constant 0 )
        , ( 1, Random.int 11 50 )
        , ( 1, Random.int 50 1000 )
        ]
        |> Random.andThen (\length -> Random.String.string length whitespace)


whitespace : Random.Generator Char
whitespace =
    let
        space =
            32

        tab =
            9
    in
    Random.choices (Random.constant space) [ Random.constant tab ]
        |> Random.map Char.fromCode
