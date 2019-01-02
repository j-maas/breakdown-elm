module TodoList exposing
    ( Id
    , TodoList
    , find
    , fromList
    , insert
    , mapToList
    , mapTodo
    , put
    , remove
    )

import List.Zipper as Zipper exposing (Zipper)
import Todo exposing (Todo)
import Utils.ZipperUtils as Zipper


type TodoList
    = TodoList (List Todo)


type Id
    = Id Int


fromList : List Todo -> TodoList
fromList list =
    TodoList list


insert : Todo -> TodoList -> ( Id, TodoList )
insert todo (TodoList todos) =
    let
        newId =
            Id (List.length todos)
    in
    ( newId, TodoList (todos ++ [ todo ]) )


put : Todo -> TodoList -> TodoList
put todo list =
    insert todo list |> Tuple.second


find : Id -> TodoList -> Maybe (Zipper Todo)
find id (TodoList todos) =
    let
        index =
            case id of
                Id i ->
                    i
    in
    Zipper.fromList todos
        |> Maybe.andThen (Zipper.focusIndex index)


mapTodo : (Todo -> Todo) -> Zipper Todo -> TodoList
mapTodo map zipper =
    TodoList (Zipper.mapCurrent map zipper |> Zipper.toList)


mapToList : (Id -> Todo -> a) -> TodoList -> List a
mapToList map (TodoList todos) =
    List.indexedMap (\id todo -> map (Id id) todo) todos


remove : Zipper Todo -> TodoList
remove zipper =
    TodoList (Zipper.remove zipper)
