module Todo exposing
    ( Todo
    , action
    , decoder
    , encode
    , from
    , fromAction
    , readAction
    , setAction
    )

import Json.Decode as Decode
import Json.Encode as Encode
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)


type Todo
    = Todo
        { action : NonEmptyString
        }



-- BUILD


from : NonEmptyString -> Todo
from act =
    Todo
        { action = act
        }


fromAction : NonEmptyString -> Todo
fromAction act =
    from act



-- READ


action : Todo -> NonEmptyString
action (Todo todo) =
    todo.action


readAction : Todo -> String
readAction todo =
    action todo |> NonEmptyString.toString



-- MODIFY


setAction : NonEmptyString -> Todo -> Todo
setAction newAction (Todo todo) =
    Todo { todo | action = newAction }



-- JSON


actionField : String
actionField =
    "action"


encode : Todo -> Encode.Value
encode todo =
    Encode.object
        [ ( actionField, readAction todo |> Encode.string )
        ]


decoder : Decode.Decoder Todo
decoder =
    let
        decodeAction =
            Decode.string
                |> Decode.andThen
                    (\rawAction ->
                        case NonEmptyString.fromString rawAction of
                            Just act ->
                                Decode.succeed act

                            Nothing ->
                                Decode.fail "Invalid action."
                    )
    in
    Decode.map
        from
        (Decode.field actionField decodeAction)
