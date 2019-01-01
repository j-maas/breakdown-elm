module TestTodoCollections exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Zipper as Zipper
import Test exposing (..)
import Todo exposing (Todo)
import TodoCollection exposing (TodoCollection)
import TodoList
import Utils.NonEmptyString as NonEmptyString
import Utils.StringFuzzer exposing (nonblankStringFuzzer, whitespaceStringFuzzer)


suite : Test
suite =
    describe "TodoCollection"
        [ test "puts todos from list into one collection and retrieves them" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'I' "nsert me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo ]
                    |> TodoCollection.mapToList TodoCollection.Current (\_ t -> t)
                    |> Expect.equal [ todo ]
        , test "todos added to one collection are not retrieved with another" <|
            \_ ->
                let
                    todo =
                        Todo.from (NonEmptyString.build 'D' "o not get me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo ]
                    |> TodoCollection.mapToList TodoCollection.Done (\_ t -> t)
                    |> Expect.equal []
        , test "finds todo by id across collections" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.from (NonEmptyString.build 'G' "et me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo1 ]
                    |> TodoCollection.fromList TodoCollection.Done [ todo2 ]
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                       )
                    |> Maybe.map
                        (TodoCollection.current
                            >> Expect.equal todo3
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        , test "updates todo by id, leaving id the same" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.from (NonEmptyString.build 'C' "hange me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo1 ]
                    |> TodoCollection.fromList TodoCollection.Done [ todo2 ]
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                                |> Maybe.map
                                    (\zipper ->
                                        let
                                            newAction =
                                                NonEmptyString.build 'C' "hanged."

                                            changedTodo =
                                                Todo.from newAction
                                        in
                                        TodoCollection.mapTodo (Todo.setAction newAction) zipper
                                            |> getTodos TodoCollection.Current
                                            |> Expect.equal [ todo1, changedTodo ]
                                    )
                       )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        , test "removes from correct collection" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.from (NonEmptyString.build 'R' "emove me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo1 ]
                    |> TodoCollection.fromList TodoCollection.Done [ todo2 ]
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                       )
                    |> Maybe.map
                        (\zipper ->
                            TodoCollection.remove zipper
                                |> Expect.all
                                    [ getTodos TodoCollection.Current >> Expect.equal [ todo1 ]
                                    , getTodos TodoCollection.Done >> Expect.equal [ todo2 ]
                                    ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        , test "moves todo between collections" <|
            \_ ->
                let
                    todo1 =
                        Todo.from (NonEmptyString.build 'L' "eave me!")

                    todo2 =
                        Todo.from (NonEmptyString.build 'L' "eave me, too!")

                    todo3 =
                        Todo.from (NonEmptyString.build 'M' "ove me!")
                in
                TodoCollection.empty
                    |> TodoCollection.fromList TodoCollection.Current [ todo1 ]
                    |> TodoCollection.fromList TodoCollection.Done [ todo2 ]
                    |> TodoCollection.insert TodoCollection.Current todo3
                    |> (\( id, collection ) ->
                            TodoCollection.find id collection
                       )
                    |> Maybe.map
                        (\zipper ->
                            TodoCollection.move zipper
                                |> Expect.all
                                    [ getTodos TodoCollection.Current >> Expect.equal [ todo1 ]
                                    , getTodos TodoCollection.Done >> Expect.equal [ todo2, todo3 ]
                                    ]
                        )
                    |> Maybe.withDefault (Expect.fail "Expected to find id, but received Nothing.")
        ]


getTodos : TodoCollection.Selector -> TodoCollection -> List Todo
getTodos selector collection =
    TodoCollection.mapToList selector (\_ todo -> todo) collection
