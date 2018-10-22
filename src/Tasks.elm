module Tasks exposing
    ( Collection, empty
    , TaskEntry, Task, TaskId, Action(..), getAction, readAction, toList, getId, idToComparable
    , actionFromString, stringFromAction
    , appendTask, appendAndGetTask, removeTask, moveTask
    , editTask
    , applyEdit, cancelEdit, edit, notEditing, startEdit, taskFromAction
    )

{-| Tasks and collection of tasks.


# Collections

@docs Collection, empty


# Tasks

@docs TaskEntry, Task, TaskId, Action, getAction, readAction, toList, getId, idToComparable


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
    IdCollection name Task


{-| A `Collection` containing no tasks.
-}
empty : name -> Collection name
empty =
    IdCollection.empty



-- BUILD


{-| Add a new TaskEntry containing the given action to the end of a collection.
-}
appendTask : Action -> Collection c -> Collection c
appendTask action collection =
    appendAndGetTask action collection |> Tuple.second


{-| Appends a TaskEntry to a collection and returns that new TaskEntry.
-}
appendAndGetTask : Action -> Collection c -> ( TaskEntry c, Collection c )
appendAndGetTask action to =
    let
        task =
            taskFromAction action
    in
    IdCollection.appendAndGetEntry task to



-- READ


{-| A task belonging to a specified collection.
-}
type alias TaskEntry collection =
    IdCollection.Entry collection Task


type alias Task =
    { action : Action
    , editing : Editing
    }


taskFromAction : Action -> Task
taskFromAction action =
    { action = action, editing = notEditing }


{-| A token to uniquely identify a task in the specified collection.
-}
type alias TaskId collection =
    IdCollection.Id collection


{-| A special string which cannot be empty or have leading or trailing spaces.
-}
type Action
    = Action String


{-| Extracts the `Action` from a `TaskEntry`.
-}
getAction : TaskEntry c -> Action
getAction =
    .item >> .action


{-| Extracts a `TaskEntry`'s `Action` as a `String`.
-}
readAction : TaskEntry c -> String
readAction =
    getAction >> stringFromAction


{-| Converts a `Collection` to a `List` for further manipulation.
-}
toList : Collection c -> List (TaskEntry c)
toList =
    IdCollection.toList



-- ID


{-| Extracts the `TaskId` from a `TaskEntry`.
-}
getId : TaskEntry c -> TaskId c
getId =
    .id


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


{-| Replaces the `Action` of the specified `TaskEntry`.

The `TaskEntry` with the given `TaskId` in the `Collection` will have its `Action` replaced with the provided one.

-}
editTask : TaskId c -> Action -> Collection c -> Collection c
editTask id action collection =
    IdCollection.set
        id
        (taskFromAction action)
        collection


{-| Removes the `TaskEntry` with the given `TaskId` from the `Collection`.
-}
removeTask : TaskId c -> Collection c -> Collection c
removeTask =
    IdCollection.remove


{-| Moves a task between different `Collection`s, ensuring unique IDs.

The `TaskEntry` with the given `TaskId` from `Collection a` will be moved to `Collection b` and its `TaskId` updated.

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



-- EDITING


type Editing
    = Editing (Maybe EditingInfo)


type EditingInfo
    = EditingInfo
        { edited : String
        , previousAction : Action
        }


{-| The Editing state for when the task is not being edited.
-}
notEditing : Editing
notEditing =
    Editing Nothing


{-| Initiates editing.
-}
startEdit : Task -> Task
startEdit task =
    { task
        | editing = Editing (Just (initEditingInfo task))
    }


{-| Helper function to initialize the EditingInfo when starting to edit a task.
-}
initEditingInfo : Task -> EditingInfo
initEditingInfo { action } =
    EditingInfo
        { edited = stringFromAction action
        , previousAction = action
        }


{-| Stores the new action in the current edit.
-}
edit : String -> Task -> Task
edit newAction task =
    let
        (Editing editing) =
            task.editing

        newEditing =
            Editing (Maybe.map (\(EditingInfo info) -> EditingInfo { info | edited = newAction }) editing)
    in
    { task | editing = newEditing }


{-| Cancels the current edit and leaves the Task as is.
-}
cancelEdit : Task -> Task
cancelEdit task =
    { task | editing = notEditing }


{-| Applies the current edit to the task. If the edit is valid, the task will be returned. Otherwise, Nothing is returned.
-}
applyEdit : Task -> Maybe Task
applyEdit task =
    case task.editing of
        Editing editing ->
            Maybe.andThen (\(EditingInfo info) -> actionFromString info.edited) editing
                |> Maybe.map
                    (\newAction ->
                        { task
                            | action = newAction
                            , editing = notEditing
                        }
                    )
