module TodoList exposing (TodoList, find, fromList, insert, mapToList)

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


mapToList : (Id -> Todo -> a) -> TodoList -> List a
mapToList map (TodoList todos) =
    List.indexedMap (\id todo -> map (Id id) todo) todos
