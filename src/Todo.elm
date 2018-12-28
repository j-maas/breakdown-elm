module Todo exposing (action, from)


type Todo
    = Todo Action


from : String -> Maybe Todo
from raw =
    actionFromString raw
        |> Maybe.map Todo


action : Todo -> String
action (Todo act) =
    stringFromAction act


type Action
    = Action String


actionFromString : String -> Maybe Action
actionFromString raw =
    let
        trimmed =
            String.trim raw
    in
    case String.length trimmed of
        0 ->
            Nothing

        _ ->
            Just (Action trimmed)


stringFromAction : Action -> String
stringFromAction (Action act) =
    act
