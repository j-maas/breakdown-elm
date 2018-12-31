module TestZipperUtils exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Zipper as Zipper exposing (Zipper(..))
import Test exposing (..)
import Utils.ZipperUtils as Utils


suite : Test
suite =
    describe "ZipperUtils"
        [ test "build has focus on first element" <|
            \_ ->
                Utils.build 1 [ 2, 3 ]
                    |> Zipper.current
                    |> Expect.equal 1
        , test "buildWithFocus sets focus correctly" <|
            \_ ->
                Utils.buildWithFocus [ 1, 2 ] 3 [ 4, 5 ]
                    |> Zipper.current
                    |> Expect.equal 3
        , test "focusMap increments correctly" <|
            \_ ->
                [ 1, 2, 3 ]
                    |> Utils.focusMap (\selected -> Zipper.current selected + 1)
                    |> Expect.equal [ 2, 3, 4 ]
        , test "remove yields list without element" <|
            \_ ->
                Utils.buildWithFocus [ 1, 2, 3 ] 4 [ 5, 6 ]
                    |> Utils.remove
                    |> Expect.equal [ 1, 2, 3, 5, 6 ]
        , test "moves item from zipper to list" <|
            \_ ->
                let
                    zipper =
                        Utils.buildWithFocus [ 1, 2 ] 3 [ 4 ]

                    list =
                        [ 5, 6 ]
                in
                Utils.move zipper list
                    |> Expect.equal ( [ 1, 2, 4 ], [ 5, 6, 3 ] )
        , describe "focusIndex"
            [ test "goes to specified index" <|
                \_ ->
                    let
                        zipper =
                            Utils.build 1 [ 2, 3, 4 ]
                    in
                    Utils.focusIndex 2 zipper
                        |> Maybe.map
                            (Zipper.current
                                >> Expect.equal 3
                            )
                        |> Maybe.withDefault (Expect.fail "Expected index to be valid, but received Nothing.")
            , test "if index too high, returns Nothing" <|
                \_ ->
                    let
                        zipper =
                            Utils.build 1 [ 2, 3, 4 ]
                    in
                    Utils.focusIndex 4 zipper
                        |> Expect.equal Nothing
            , test "if index negative, returns Nothing" <|
                \_ ->
                    let
                        zipper =
                            Utils.build 1 [ 2, 3, 4 ]
                    in
                    Utils.focusIndex -1 zipper
                        |> Expect.equal Nothing
            ]
        ]
