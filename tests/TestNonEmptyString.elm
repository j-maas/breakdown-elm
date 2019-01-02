module TestNonEmptyString exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import StringFuzz exposing (minimalLengthStringFuzzer)
import Test exposing (..)
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "NonEmptyString"
        [ fuzz (minimalLengthStringFuzzer 1) "makes non-empty string" <|
            \nonempty ->
                NonEmptyString.fromString nonempty
                    |> Maybe.map (NonEmptyString.toString >> Expect.equal nonempty)
                    |> Maybe.withDefault (Expect.fail ("Expected input '" ++ nonempty ++ "' to be non-empty, but received `Nothing`."))
        , test "disallows non-empty string" <|
            \_ ->
                NonEmptyString.fromString ""
                    |> Expect.equal Nothing
        ]
