module Tasks exposing
    ( actionFromString, stringFromAction
    , TaskEntry, Task(..), TaskInfo, TaskId, getTaskInfo, Action, taskFromAction, getAction, readAction
    , startEdit, edit, applyEdit, cancelEdit
    , Collection, empty, toList, getId, idToComparable
    , appendTask, appendAndGetTask, removeTask, moveTask, editTask
    )

{-| Tasks and collection of tasks.


# Actions

@docs actionFromString, stringFromAction


# Tasks

@docs TaskEntry, Task, TaskInfo, TaskId, getTaskInfo, Action, taskFromAction, getAction, readAction


# Editing

@docs startEdit, edit, applyEdit, cancelEdit


# Collections

@docs Collection, empty, toList, getId, idToComparable


## Modify Collection

@docs appendTask, appendAndGetTask, removeTask, moveTask, editTask

-}

import IdCollection exposing (IdCollection)
import List.Extra as List



-- READ


{-| A task belonging to a specified collection.
-}
type alias TaskEntry collection =
    IdCollection.Entry collection Task


{-| Information about the task. Either it is a simple task with only `TaskInfo`
or it is being edited with additional information.
-}
type Task
    = Task TaskInfo
    | TaskInEdit TaskEditInfo


{-| The basic information every task possesses.
-}
type alias TaskInfo =
    { action : Action
    }


{-| All information about a task currently being edited.
-}
type alias TaskEditInfo =
    { info : TaskInfo
    , editing : Editing
    }


{-| Minimal constructor for a task.
-}
taskFromAction : Action -> Task
taskFromAction action =
    Task { action = action }


{-| Extracts the information that all tasks have in common.
-}
getTaskInfo : Task -> TaskInfo
getTaskInfo task =
    case task of
        Task info ->
            info

        TaskInEdit editInfo ->
            editInfo.info


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
    .item >> getTaskInfo >> .action


{-| Extracts a `TaskEntry`'s `Action` as a `String`.
-}
readAction : TaskEntry c -> String
readAction =
    getAction >> stringFromAction



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



-- EDITING


{-| Obscure type that holds the information of the current edit.
-}
type Editing
    = Editing EditingInfo


{-| The actual data for the current edit.
-}
type alias EditingInfo =
    { edited : String
    , previousAction : Action
    }


{-| Initiates editing.
-}
startEdit : Task -> TaskEditInfo
startEdit task =
    { info = getTaskInfo task
    , editing = Editing (initEditingInfo task)
    }


{-| Helper function to initialize the EditingInfo when starting to edit a task.
-}
initEditingInfo : Task -> EditingInfo
initEditingInfo task =
    let
        action =
            getTaskInfo task |> .action
    in
    { edited = stringFromAction action
    , previousAction = action
    }


{-| Stores the new action in the current edit.
-}
edit : String -> TaskEditInfo -> TaskEditInfo
edit newAction task =
    let
        (Editing editing) =
            task.editing

        newEditing =
            Editing { editing | edited = newAction }
    in
    { task | editing = newEditing }


{-| Cancels the current edit and leaves the Task as is.
-}
cancelEdit : TaskEditInfo -> Task
cancelEdit task =
    Task task.info


{-| Applies the current edit to the task.
If the edit is valid, the task will be returned.
Otherwise, Nothing is returned.
-}
applyEdit : TaskEditInfo -> Maybe Task
applyEdit task =
    case task.editing of
        Editing editing ->
            actionFromString editing.edited
                |> Maybe.map
                    (\newAction ->
                        let
                            info =
                                task.info

                            newInfo =
                                { info | action = newAction }
                        in
                        Task newInfo
                    )



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


{-| Add a new TaskEntry containing the given action to the end of a collection.
-}
appendTask : Action -> Collection c -> Collection c
appendTask action collection =
    let
        task =
            taskFromAction action
    in
    IdCollection.append task collection


{-| Appends a TaskEntry to a collection and returns that new TaskEntry.
-}
appendAndGetTask : Action -> Collection c -> ( TaskEntry c, Collection c )
appendAndGetTask action to =
    let
        task =
            taskFromAction action
    in
    IdCollection.appendAndGetEntry task to


{-| Builds a collection from a list of tasks.
-}
fromList : tag -> List Task -> Collection tag
fromList tag list =
    IdCollection.fromList tag list


{-| Converts a `Collection` to a `List` for further manipulation.
-}
toList : Collection c -> List (TaskEntry c)
toList =
    IdCollection.toList


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



-- Modify Collections


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
