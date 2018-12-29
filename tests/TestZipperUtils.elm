module TestZipperUtils exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import List.Zipper as Zipper exposing (Zipper(..))
import Test exposing (..)
import Utils.ZipperUtils as Utils


suite : Test
suite =
    describe "ZipperUtils"
        [ test "focusMap increments correctly" <|
            \_ ->
                [ 1, 2, 3 ]
                    |> Utils.focusMap (\selected -> Zipper.current selected + 1)
                    |> Expect.equal [ 2, 3, 4 ]
        , test "remove yields list without element" <|
            \_ ->
                Zipper [ 1, 2 ] 3 [ 4 ]
                    |> Utils.remove
                    |> Expect.equal [ 1, 2, 4 ]
        , test "moves item from zipper to list" <|
            \_ ->
                let
                    zipper =
                        Zipper [ 1, 2 ] 3 [ 4 ]

                    list =
                        [ 5, 6 ]
                in
                Utils.move zipper list
                    |> Expect.equal ( [ 1, 2, 4 ], [ 5, 6, 3 ] )
        ]
