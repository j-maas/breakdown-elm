module Todo exposing (Todo, action, from, readAction, setAction)

import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type Todo
    = Todo NonEmptyString


from : NonEmptyString -> Todo
from =
    Todo


action : Todo -> NonEmptyString
action (Todo act) =
    act


readAction : Todo -> String
readAction todo =
    action todo |> NonEmptyString.toString


setAction : NonEmptyString -> Todo -> Todo
setAction newAction _ =
    Todo newAction
