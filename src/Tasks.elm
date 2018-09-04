module Tasks exposing (addTask, empty, readAction, toList)


type Collection name
    = Collection (List (Task name))


empty : name -> Collection name
empty _ =
    Collection []


type Task collection
    = Task
        { id : TaskId
        , action : Action
        }


type TaskId
    = TaskId Int


type Action
    = Action String


addTask : String -> Collection c -> Collection c
addTask rawAction (Collection list) =
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

        nextId =
            highestId + 1

        newAction =
            actionFromString rawAction

        newList =
            case newAction of
                Nothing ->
                    list

                Just action ->
                    list ++ [ Task { id = TaskId nextId, action = action } ]
    in
    Collection newList


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


stringFromAction : Action -> String
stringFromAction (Action rawAction) =
    rawAction


readAction : Task c -> String
readAction (Task task) =
    stringFromAction task.action


toList : Collection c -> List (Task c)
toList (Collection list) =
    list
