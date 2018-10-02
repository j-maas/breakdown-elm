module TestMain exposing (suite)

import Expect
import Fuzz exposing (..)
import Main exposing (Model, Msg(..), initModel, update)
import Test exposing (..)


suite : Test
suite =
    describe "messages"
        [ test "NoOp keeps model the same" <|
            \_ ->
                update NoOp initModel
                    |> Tuple.first
                    |> Expect.equal initModel
        ]
