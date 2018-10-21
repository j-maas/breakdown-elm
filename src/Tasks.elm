module Tasks exposing
    ( Collection, empty
    , Task, TaskId, Action(..), getAction, readAction, toList, getId, idToComparable
    , actionFromString, stringFromAction
    , appendTask, appendAndGetTask, removeTask, moveTask
    , editTask
    )

{-| Tasks and collection of tasks.


# Collections

@docs Collection, empty


# Tasks

@docs Task, TaskId, Action, getAction, readAction, toList, getId, idToComparable


# Actions

@docs actionFromString, stringFromAction


# Modify Collection

@docs appendTask, appendAndGetTask, removeTask, moveTask


# Edit

@docs editTask

-}

import IdCollection exposing (IdCollection)
import List.Extra as List



-- COLLECTIONS


{-| A collection of tasks identified by a `name`.

The name prevents tasks from different `Collection`s to be inadvertently mixed.

-}
type alias Collection name =
    IdCollection name Action


{-| A `Collection` containing no tasks.
-}
empty : name -> Collection name
empty =
    IdCollection.empty



-- BUILD


{-| Add a new Task containing the given action to the end of a collection.
-}
appendTask : Action -> Collection c -> Collection c
appendTask action collection =
    appendAndGetTask action collection |> Tuple.second


{-| Appends a Task to a collection and returns that new Task.
-}
appendAndGetTask : Action -> Collection c -> ( Task c, Collection c )
appendAndGetTask action to =
    IdCollection.appendAndGetEntry action to



-- READ


{-| A task belonging to a specified collection.
-}
type alias Task collection =
    IdCollection.Entry collection Action


{-| A token to uniquely identify a task in the specified collection.
-}
type alias TaskId collection =
    IdCollection.Id collection


{-| A special string which cannot be empty or have leading or trailing spaces.
-}
type Action
    = Action String


{-| Extracts the `Action` from a `Task`.
-}
getAction : Task c -> Action
getAction task =
    task.item


{-| Extracts a `Task`'s `Action` as a `String`.
-}
readAction : Task c -> String
readAction task =
    stringFromAction task.item


{-| Converts a `Collection` to a `List` for further manipulation.
-}
toList : Collection c -> List (Task c)
toList =
    IdCollection.toList



-- ID


{-| Extracts the `TaskId` from a `Task`.
-}
getId : Task c -> TaskId c
getId task =
    task.id


{-| Only for use in tests. Allows for uniqueness checks on IDs.
-}
idToComparable : TaskId c -> Int
idToComparable =
    IdCollection.idToComparable



-- ACTIONS


{-| Returns a sanitized `Action` from a `String`, iff the string is not empty.
-}
actionFromString : String -> Maybe Action
actionFromString rawAction =
    let
        cleaned =
            String.trim rawAction
    in
    if String.isEmpty cleaned then
        Nothing

    else
        Just (Action cleaned)


{-| Extracts the `String` from an `Action`.
-}
stringFromAction : Action -> String
stringFromAction (Action rawAction) =
    rawAction



-- EDIT


{-| Replaces the `Action` of the specified `Task`.

The `Task` with the given `TaskId` in the `Collection` will have its `Action` replaced with the provided one.

-}
editTask : TaskId c -> Action -> Collection c -> Collection c
editTask id action collection =
    IdCollection.update
        (\_ -> action)
        id
        collection


{-| Removes the `Task` with the given `TaskId` from the `Collection`.
-}
removeTask : TaskId c -> Collection c -> Collection c
removeTask =
    IdCollection.remove


{-| Moves a task between different `Collection`s, ensuring unique IDs.

The `Task` with the given `TaskId` from `Collection a` will be moved to `Collection b` and its `TaskId` updated.

-}
moveTask : TaskId a -> Collection a -> Collection b -> ( Collection a, Collection b )
moveTask id from to =
    let
        mayBeTask =
            IdCollection.get id from

        newFrom =
            removeTask id from
    in
    case mayBeTask of
        Just task ->
            ( newFrom, IdCollection.append task to )

        Nothing ->
            ( from, to )
