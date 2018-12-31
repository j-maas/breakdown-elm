module TestTodoList exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Zipper as Zipper
import Test exposing (..)
import Todo
import TodoList
import Utils.NonEmptyString as NonEmptyString
import Utils.StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)


suite : Test
suite =
    describe "TodoList"
        [ test "fromList and mapToList results in same list with ids" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'M' "e, too!")
                in
                TodoList.fromList [ todo1, todo2 ]
                    |> TodoList.mapToList (\_ todo -> todo)
                    |> Expect.equal [ todo1, todo2 ]
        , test "adds todo to existing list" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'M' "e, too!")
                in
                TodoList.fromList [ todo1 ]
                    |> TodoList.insert todo2
                    |> Tuple.second
                    |> TodoList.mapToList (\_ todo -> todo)
                    |> Expect.equal [ todo1, todo2 ]
        , test "finds todo by id" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'M' "e, too!")
                in
                TodoList.fromList [ todo1 ]
                    |> TodoList.insert todo2
                    |> (\( id, list ) ->
                            TodoList.find id list
                       )
                    |> Maybe.map
                        (\zipper ->
                            let
                                current =
                                    Zipper.current zipper
                            in
                            Expect.equal current todo2
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find todo, but received Nothing.")
        ]
