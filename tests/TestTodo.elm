module TestTodo exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import StringFuzz exposing (nonblankStringFuzzer, whitespaceStringFuzzer)
import Test exposing (..)
import Todo
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "Todo"
        [ test "encoding and decoding results in same todo" <|
            \_ ->
                let
                    todo =
                        Todo.fromAction (NonEmptyString.build 'R' "estore me!")
                in
                Todo.encode todo
                    |> Decode.decodeValue Todo.decoder
                    |> Expect.equal (Ok todo)
        ]
