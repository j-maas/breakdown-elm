module TestTodo exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Todo
import Utils.NonEmptyString as NonEmptyString
import Utils.StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)


suite : Test
suite =
    describe "Todo"
        [ test "creates todo" <|
            \_ ->
                Todo.from (NonEmptyString.build 'C' "reate me!")
                    |> Todo.action
                    |> Expect.equal "Create me!"
        ]
