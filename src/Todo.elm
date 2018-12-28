module Todo exposing (Todo, action, from)

import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type Todo
    = Todo NonEmptyString


from : NonEmptyString -> Todo
from =
    Todo


action : Todo -> String
action (Todo act) =
    NonEmptyString.toString act
