module TestTodo exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)
import Test exposing (..)
import Todo


suite : Test
suite =
    describe "Task"
        [ fuzz nonblankStringFuzzer "makes task and trims action" <|
            \validAction ->
                Todo.from validAction
                    |> Maybe.map
                        (Todo.action
                            >> Expect.equal (String.trim validAction)
                        )
                    |> Maybe.withDefault (Expect.fail "Expected action to be valid, but received `Nothing`.")
        , test "does not make task from empty action" <|
            \_ ->
                Todo.from ""
                    |> Expect.equal Nothing
        , fuzz whitespaceStringFuzzer "does not make task from action with only whitespace" <|
            \invalidAction ->
                Todo.from invalidAction
                    |> Expect.equal Nothing
        ]
