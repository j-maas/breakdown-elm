module TestNonEmptyString exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import StringFuzz exposing (nonemptyStringFuzzer)
import Test exposing (..)
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "NonEmptyString"
        [ fuzz NonEmptyString.fuzzer "makes non-empty string" <|
            \nonempty ->
                NonEmptyString.toString nonempty
                    |> String.length
                    |> Expect.greaterThan 0
        , test "disallows non-empty string" <|
            \_ ->
                NonEmptyString.fromString ""
                    |> Expect.equal Nothing
        , fuzz nonemptyStringFuzzer "returns string it is made from" <|
            \nonempty ->
                NonEmptyString.fromString nonempty
                    |> Maybe.map NonEmptyString.toString
                    |> Maybe.map (Expect.equal nonempty)
                    |> Maybe.withDefault (Expect.fail "Expected fromString to succeed, but received Nothing.")
        ]
