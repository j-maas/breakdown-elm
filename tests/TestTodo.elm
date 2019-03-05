module TestTodo exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import Test exposing (..)
import Todo exposing (Todo)
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "Todo"
        [ fuzz NonEmptyString.fuzzer "returns same action it was created with" <|
            \action ->
                Todo.fromAction action
                    |> Todo.action
                    |> Expect.equal action
        , fuzz2 todoFuzzer NonEmptyString.fuzzer "returns updated action" <|
            \todo action ->
                Todo.setAction action todo
                    |> Todo.action
                    |> Expect.equal action
        , fuzz todoFuzzer "encoding and decoding results in same todo" <|
            \todo ->
                Todo.encode todo
                    |> Decode.decodeValue Todo.decoder
                    |> Expect.equal (Ok todo)
        ]


todoFuzzer : Fuzzer Todo
todoFuzzer =
    Fuzz.map Todo.fromAction
        NonEmptyString.fuzzer
