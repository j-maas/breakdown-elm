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
        [ test "creates todo with action" <|
            \_ ->
                let
                    action =
                        NonEmptyString.build 'C' "reate me!"
                in
                Todo.from action
                    |> Todo.action
                    |> Expect.equal action
        , test "readActions returns action as String" <|
            \_ ->
                Todo.from (NonEmptyString.build 'R' "eturn me!")
                    |> Todo.readAction
                    |> Expect.equal "Return me!"
        , test "updates action" <|
            \_ ->
                Todo.from (NonEmptyString.build 'C' "ange me!")
                    |> Todo.setAction (NonEmptyString.build 'C' "hanged.")
                    |> Todo.readAction
                    |> Expect.equal "Changed."
        ]
