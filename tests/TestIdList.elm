module TestIdList exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import IdList
import List.Zipper as Zipper
import Test exposing (..)
import Todo
import Utils.NonEmptyString as NonEmptyString


suite : Test
suite =
    describe "TodoList"
        [ test "fromList and mapToList results in same list with ids" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'M' "e, too!")
                in
                IdList.fromList [ todo1, todo2 ]
                    |> IdList.mapToList (\_ todo -> todo)
                    |> Expect.equal [ todo1, todo2 ]
        , test "adds todo to existing list" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'M' "e, too!")
                in
                IdList.fromList [ todo1 ]
                    |> IdList.insert todo2
                    |> Tuple.second
                    |> IdList.mapToList (\_ todo -> todo)
                    |> Expect.equal [ todo1, todo2 ]
        , test "finds todo by id" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'M' "e, too!")
                in
                IdList.fromList [ todo1 ]
                    |> IdList.insert todo2
                    |> (\( id, list ) ->
                            IdList.find id list
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
        , test "updates todo, keeping id the same" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'K' "eep me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'C' "hange me!")
                in
                IdList.fromList [ todo1 ]
                    |> IdList.insert todo2
                    |> (\( id, list ) ->
                            IdList.find id list
                       )
                    |> Maybe.map
                        (\zipper ->
                            let
                                newAction =
                                    NonEmptyString.build 'C' "hanged."

                                changedTodo =
                                    Todo.fromAction newAction
                            in
                            IdList.mapTodo (Todo.setAction newAction) zipper
                                |> IdList.mapToList (\_ t -> t)
                                |> Expect.equal [ todo1, changedTodo ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find todo, but received Nothing.")
        , test "removes" <|
            \_ ->
                let
                    todo1 =
                        Todo.fromAction (NonEmptyString.build 'A' "dd me!")

                    todo2 =
                        Todo.fromAction (NonEmptyString.build 'R' "emove me!")
                in
                IdList.fromList [ todo1 ]
                    |> IdList.insert todo2
                    |> (\( id, list ) ->
                            IdList.find id list
                       )
                    |> Maybe.map
                        (\zipper ->
                            IdList.remove zipper
                                |> IdList.mapToList (\_ t -> t)
                                |> Expect.equal [ todo1 ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find todo, but received Nothing.")
        ]
