module Tasks exposing (addTask, editTask, empty, getId, idToComparable, moveTask, readAction, removeTask, toList, unsafeActionFromString)

import List.Extra as List



-- COLLECTION


type Collection name
    = Collection (List (Task name))


empty : name -> Collection name
empty _ =
    Collection []



-- BUILD


addTask : String -> Collection c -> Collection c
addTask rawAction ((Collection list) as to) =
    let
        nextId =
            nextIdForCollection to

        newAction =
            actionFromString rawAction

        newList =
            case newAction of
                Nothing ->
                    list

                Just action ->
                    list ++ [ Task { id = nextId, action = action } ]
    in
    Collection newList


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


type Task collection
    = Task
        { id : TaskId collection
        , action : Action
        }


type TaskId collection
    = TaskId Int


type Action
    = Action String


readAction : Task c -> String
readAction (Task task) =
    stringFromAction task.action


toList : Collection c -> List (Task c)
toList (Collection list) =
    list



-- ID


getId : Task c -> TaskId c
getId (Task { id }) =
    id


idToComparable : TaskId c -> Int
idToComparable (TaskId id) =
    id



-- ACTIONS


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


{-| Only for use in tests. Prefer [`actionFromString`](#actionFromString).
-}
unsafeActionFromString : String -> Action
unsafeActionFromString rawAction =
    Action rawAction


stringFromAction : Action -> String
stringFromAction (Action rawAction) =
    rawAction



-- EDIT


editTask : TaskId c -> Action -> Collection c -> Collection c
editTask id action (Collection list) =
    List.updateIf (\task -> getId task == id)
        (\(Task task) -> Task { task | action = action })
        list
        |> Collection


removeTask : TaskId c -> Collection c -> Collection c
removeTask id (Collection list) =
    List.filter (\task -> getId task /= id) list |> Collection


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
