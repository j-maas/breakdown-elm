module Tasks exposing
    ( Collection(..), empty
    , Task(..), TaskId(..), Action(..), readAction, toList, getId, idToComparable
    , actionFromString, stringFromAction
    , addTask, removeTask, moveTask
    , editTask
    )

{-| Tasks and collection of tasks.


# Collections

@docs Collection, empty


# Tasks

@docs Task, TaskId, Action, readAction, toList, getId, idToComparable


# Actions

@docs actionFromString, stringFromAction


# Modify Collection

@docs addTask, removeTask, moveTask


# Edit

@docs editTask

-}

import List.Extra as List



-- COLLECTIONS


{-| A collection of tasks identified by a `name`.

The name prevents tasks from different `Collection`s to be inadvertently mixed.

-}
type Collection name
    = Collection (List (Task name))


{-| A `Collection` containing no tasks.
-}
empty : name -> Collection name
empty _ =
    Collection []



-- BUILD


{-| Add a new Task containing the given action to a collection.
-}
addTask : Action -> Collection c -> Collection c
addTask action ((Collection list) as to) =
    let
        nextId =
            nextIdForCollection to

        newList =
            list ++ [ Task { id = nextId, action = action } ]
    in
    Collection newList


{-| Calculates a suitable id with which a new task can be added to the collection.
-}
nextIdForCollection : Collection c -> TaskId c
nextIdForCollection (Collection list) =
    let
        highestId =
            let
                getIdNum =
                    \(Task task) ->
                        case task.id of
                            TaskId n ->
                                n
            in
            List.map getIdNum list |> List.maximum |> Maybe.withDefault 0
    in
    TaskId (highestId + 1)



-- READ


{-| A task belonging to a specified collection.
-}
type Task collection
    = Task
        { id : TaskId collection
        , action : Action
        }


{-| A token to uniquely identify a task in the specified collection.
-}
type TaskId collection
    = TaskId Int


{-| A special string which cannot be empty or have leading or trailing spaces.
-}
type Action
    = Action String


{-| Extracts a `Task`'s `Action` as a `String`.
-}
readAction : Task c -> String
readAction (Task task) =
    stringFromAction task.action


{-| Converts a `Collection` to a `List` for further manipulation.
-}
toList : Collection c -> List (Task c)
toList (Collection list) =
    list



-- ID


{-| Extracts the `TaskId` from a `Task`.
-}
getId : Task c -> TaskId c
getId (Task { id }) =
    id


{-| Only for use in tests. Allows for uniqueness checks on IDs.
-}
idToComparable : TaskId c -> Int
idToComparable (TaskId id) =
    id



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
editTask id action (Collection list) =
    List.updateIf (\task -> getId task == id)
        (\(Task task) -> Task { task | action = action })
        list
        |> Collection


{-| Removes the `Task` with the given `TaskId` from the `Collection`.
-}
removeTask : TaskId c -> Collection c -> Collection c
removeTask id (Collection list) =
    List.filter (\task -> getId task /= id) list |> Collection


{-| Moves a task between different `Collection`s, ensuring unique IDs.

The `Task` with the given `TaskId` from `Collection a` will be moved to `Collection b` and its `TaskId` updated.

-}
moveTask : TaskId a -> Collection a -> Collection b -> ( Collection a, Collection b )
moveTask id ((Collection listFrom) as from) to =
    let
        mayBeTask =
            List.find (\candidate -> getId candidate == id) listFrom

        newFrom =
            removeTask id from
    in
    case mayBeTask of
        Just task ->
            ( newFrom, insertTask task to )

        Nothing ->
            ( from, to )


{-| Inserts a possibly foreign `Task` into a `Collection` by manipulating its `ID`.
-}
insertTask : Task a -> Collection b -> Collection b
insertTask (Task task) ((Collection list) as into) =
    let
        newId =
            nextIdForCollection into

        newTask =
            Task
                { id = newId
                , action = task.action
                }
    in
    Collection (list ++ [ newTask ])
