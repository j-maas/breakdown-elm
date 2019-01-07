module TestTodo exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import IdList
import Json.Decode as Decode
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
                Todo.from (NonEmptyString.build 'I' "'m the parent.") (IdList.fromList [ child1, child2 ])
                    |> Todo.subtodos
                    |> IdList.mapToList (\_ todo -> todo)
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
                Todo.from (NonEmptyString.build 'R' "eplace my subtodos!") (IdList.fromList [ child1, child2 ])
                    |> Todo.setSubtodos (IdList.fromList [ child2 ])
                    |> Todo.subtodos
                    |> IdList.mapToList (\_ todo -> todo)
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
