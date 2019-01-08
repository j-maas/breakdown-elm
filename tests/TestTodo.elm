module TestTodo exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as Decode
import SelectCollection
import StringFuzz exposing (nonblankStringFuzzer, whitespaceStringFuzzer)
import Test exposing (..)
import Todo
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "Todo"
        [ test "creates todo with action" <|
            \_ ->
                let
                    action =
                        NonEmptyString.build 'C' "reate me!"
                in
                Todo.fromAction action
                    |> Todo.action
                    |> Expect.equal action
        , test "creates todo with subtodos" <|
            \_ ->
                let
                    child1 =
                        Todo.fromAction (NonEmptyString.build 'I' "'m the first child.")

                    child2 =
                        Todo.fromAction (NonEmptyString.build 'I' "'m the second child.")
                in
                Todo.from (NonEmptyString.build 'I' "'m the parent.") (SelectCollection.init { current = [ child1, child2 ], done = [] })
                    |> Todo.subtodos
                    |> SelectCollection.mapToList SelectCollection.Current (\_ todo -> todo)
                    |> Expect.equal [ child1, child2 ]
        , test "readActions returns action as String" <|
            \_ ->
                Todo.fromAction (NonEmptyString.build 'R' "eturn me!")
                    |> Todo.readAction
                    |> Expect.equal "Return me!"
        , test "updates action" <|
            \_ ->
                Todo.fromAction (NonEmptyString.build 'C' "ange me!")
                    |> Todo.setAction (NonEmptyString.build 'C' "hanged.")
                    |> Todo.readAction
                    |> Expect.equal "Changed."
        , test "updates subtodos" <|
            \_ ->
                let
                    child1 =
                        Todo.fromAction (NonEmptyString.build 'I' "'m the first child.")

                    child2 =
                        Todo.fromAction (NonEmptyString.build 'I' "'m the second child.")
                in
                Todo.from (NonEmptyString.build 'R' "eplace my subtodos!") (SelectCollection.init { current = [ child1, child2 ], done = [] })
                    |> Todo.setSubtodos (SelectCollection.init { current = [ child2 ], done = [] })
                    |> Todo.subtodos
                    |> SelectCollection.mapToList SelectCollection.Current (\_ todo -> todo)
                    |> Expect.equal [ child2 ]
        , test "encoding and decoding results in same todo" <|
            \_ ->
                let
                    todo =
                        Todo.fromAction (NonEmptyString.build 'R' "estore me!")
                in
                Todo.encode todo
                    |> Decode.decodeValue Todo.decoder
                    |> Expect.equal (Ok todo)
        ]
