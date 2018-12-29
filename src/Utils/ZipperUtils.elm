module Utils.ZipperUtils exposing
    ( build
    , buildWithFocus
    , focusMap
    , move
    , remove
    )

import List.Zipper as Zipper exposing (Zipper(..))



-- BUILD


build : a -> List a -> Zipper a
build first rest =
    Zipper [] first rest


buildWithFocus : List a -> a -> List a -> Zipper a
buildWithFocus pre focus post =
    Zipper (List.reverse pre) focus post



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
    let
        prelist =
            Zipper.before zipper

        postlist =
            Zipper.after zipper
    in
    prelist ++ postlist


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
