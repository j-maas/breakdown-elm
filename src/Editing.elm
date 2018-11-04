module Editing exposing
    ( Editing, getEdited, readPrevious, isUnchanged
    , startEdit, edit, applyEdit
    , Collection, inlineEdit
    )

{-| Editing of tasks.


# Editing

@docs Editing, getEdited, readPrevious, isUnchanged


# Editing Tasks

@docs startEdit, edit, applyEdit


# Collections

@docs Collection, inlineEdit

-}

import IdCollection
import Tasks


{-| Obscure type that holds the information of the current edit.
-}
type Editing
    = Editing EditingInfo


{-| The actual data for the current edit.
-}
type alias EditingInfo =
    { edited : String
    , previousAction : Tasks.Action
    }


getEdited : Editing -> String
getEdited (Editing info) =
    info.edited


readPrevious : Editing -> String
readPrevious (Editing info) =
    info.previousAction |> Tasks.stringFromAction


isUnchanged : Editing -> Bool
isUnchanged (Editing info) =
    info.edited == Tasks.stringFromAction info.previousAction


{-| Initiates editing.
-}
startEdit : Tasks.Task -> Editing
startEdit task =
    Editing (initEditingInfo task)


{-| Helper function to initialize the EditingInfo when starting to edit a task.
-}
initEditingInfo : Tasks.Task -> EditingInfo
initEditingInfo task =
    let
        action =
            Tasks.getAction task
    in
    { edited = Tasks.stringFromAction action
    , previousAction = action
    }


{-| Stores the new action in the current edit.
-}
edit : String -> Editing -> Editing
edit newAction (Editing editing) =
    Editing { editing | edited = newAction }


{-| Applies the current edit to the task.
If the edit is valid, the task will be returned.
Otherwise, Nothing is returned.
-}
applyEdit : Editing -> Tasks.Task -> Maybe Tasks.Task
applyEdit (Editing editing) (Tasks.Task task) =
    Tasks.actionFromString editing.edited
        |> Maybe.map
            (\newAction ->
                Tasks.Task { task | action = newAction }
            )



-- TRANSFORM


type alias Collection c =
    IdCollection.IdCollection c { edit : Maybe Editing, task : Tasks.Task }


inlineEdit : Editing -> c -> Tasks.TaskId c -> Tasks.Collection c -> Collection c
inlineEdit editing tag id collection =
    IdCollection.toList collection
        |> List.map
            (\entry ->
                let
                    editInfo : Maybe Editing
                    editInfo =
                        if id == entry.id then
                            Just editing

                        else
                            Nothing

                    task : Tasks.Task
                    task =
                        entry.item
                in
                { edit = editInfo
                , task = task
                }
            )
        |> IdCollection.fromList tag
