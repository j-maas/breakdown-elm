module Utils.ZipperUtils exposing
    ( focusMap
    , move
    , remove
    )

import List.Zipper as Zipper exposing (Zipper(..))



-- MAP


focusMap : (Zipper a -> b) -> List a -> List b
focusMap map list =
    case list of
        [] ->
            []

        a :: rest ->
            let
                zipper =
                    Zipper [] a rest
            in
            zipperMap map zipper


zipperMap : (Zipper a -> b) -> Zipper a -> List b
zipperMap map zipper =
    let
        zipMapHelper : Zipper a -> List b -> List b
        zipMapHelper zip accumulator =
            let
                item =
                    map zip

                newAccumulator =
                    accumulator ++ [ item ]
            in
            case Zipper.next zip of
                Just next ->
                    zipMapHelper next newAccumulator

                Nothing ->
                    newAccumulator

        firstZip =
            Zipper.first zipper
    in
    zipMapHelper firstZip []



-- TRANSFORM


remove : Zipper a -> List a
remove zipper =
    case zipper of
        Zipper prelist focus postlist ->
            let
                remaining =
                    prelist ++ postlist
            in
            remaining


move : Zipper a -> List a -> ( List a, List a )
move zipper list =
    let
        current =
            Zipper.current zipper

        newZipper =
            remove zipper

        newList =
            list ++ [ current ]
    in
    ( newZipper, newList )
