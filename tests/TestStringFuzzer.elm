module TestStringFuzzer exposing (suite)

import Expect
import Fuzz exposing (..)
import Test exposing (..)
import Utils.StringFuzzer as StringFuzzer


suite : Test
suite =
    describe "StringFuzzer"
        [ fuzz StringFuzzer.whitespaceStringFuzzer "whitespaceStringFuzzer trimmed is empty" <|
            \whitespace ->
                String.trim whitespace
                    |> String.isEmpty
                    |> Expect.true "Expected whitespace to be empty after trimming."
        , fuzz StringFuzzer.nonblankStringFuzzer "nonblankStringFuzzer trimmed is not empty" <|
            \nonblank ->
                String.trim nonblank
                    |> String.isEmpty
                    |> Expect.false "Expected nonblank to not empty after trimming."
        ]
