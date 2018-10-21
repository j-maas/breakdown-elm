module TestIdCollection exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import IdCollection exposing (Entry, IdCollection)
import List.Extra as List
import Test exposing (..)


type CollectionTag
    = Tag


type OtherTag
    = Other


suite : Test
suite =
    let
        emptyCollection =
            IdCollection.empty Tag
    in
    describe "IdCollection"
        [ describe "Build"
            [ fuzz2 string string "appends item" <|
                \first second ->
                    emptyCollection
                        |> IdCollection.append first
                        |> IdCollection.append second
                        |> toListWithoutIds
                        |> Expect.equal [ first, second ]
            , fuzz2 string string "appends and returns item" <|
                \first second ->
                    IdCollection.fromList Tag [ first ]
                        |> IdCollection.appendAndGetEntry second
                        |> Tuple.mapBoth .item toListWithoutIds
                        |> Expect.equal ( second, [ first, second ] )
            , fuzz (list string) "inits from list" <|
                \list ->
                    IdCollection.fromList Tag list
                        |> toListWithoutIds
                        |> Expect.equal list
            ]
        , describe "Query"
            [ fuzz2 (list string) percentage "gets item by id" <|
                \list offset ->
                    testEntryInCollection
                        (\index collection entry ->
                            IdCollection.get entry.id collection
                                |> Expect.equal (Just entry.item)
                        )
                        list
                        offset
            ]
        , describe "Manipulate"
            [ fuzz2 (list string) percentage "removes item by id" <|
                \list offset ->
                    testEntryInCollection
                        (\index collection entry ->
                            IdCollection.remove entry.id collection
                                |> toListWithoutIds
                                |> Expect.equal (List.removeAt index list)
                        )
                        list
                        offset
            , fuzz2 (list string) percentage "removes and gets item by id" <|
                \list offset ->
                    testEntryInCollection
                        (\index collection entry ->
                            IdCollection.removeAndGet entry.id collection
                                |> Tuple.mapSecond toListWithoutIds
                                |> Expect.equal ( Just entry.item, List.removeAt index list )
                        )
                        list
                        offset
            , fuzz3 (list string) string percentage "sets item by id" <|
                \list newItem offset ->
                    testEntryInCollection
                        (\index collection entry ->
                            IdCollection.set newItem entry.id collection
                                |> toListWithoutIds
                                |> Expect.equal (List.setAt index newItem list)
                        )
                        list
                        offset
            , fuzz3 (list string) string percentage "updates item by id" <|
                \list newItem offset ->
                    testEntryInCollection
                        (\index collection entry ->
                            IdCollection.update (\item -> item ++ newItem) entry.id collection
                                |> toListWithoutIds
                                |> Expect.equal (List.updateAt index (\item -> item ++ newItem) list)
                        )
                        list
                        offset
            , fuzz3 (list string) percentage (list string) "moves items between collections" <|
                \from offset to ->
                    testEntryInCollection
                        (\index collection entry ->
                            let
                                toCollection =
                                    IdCollection.fromList Other to
                            in
                            IdCollection.move entry.id collection toCollection
                                |> Tuple.mapBoth toListWithoutIds toListWithoutIds
                                |> Expect.equal ( List.removeAt index from, to ++ [ entry.item ] )
                        )
                        from
                        offset
            ]
        , let
            hasUniqueIds =
                IdCollection.toList >> List.allDifferentBy (.id >> IdCollection.idToComparable)
          in
          describe "IDs"
            [ fuzz (list string) "items have different ids" <|
                \list ->
                    let
                        collection =
                            IdCollection.fromList Tag list
                    in
                    hasUniqueIds collection
                        |> Expect.true ("Detected duplicate ids in:\n" ++ Debug.toString collection)
            , fuzz3 (list string) percentage (list string) "moving items keeps unique ids" <|
                \from offset to ->
                    testEntryInCollection
                        (\index collection entry ->
                            let
                                toCollection =
                                    IdCollection.fromList Other to
                            in
                            IdCollection.move entry.id collection toCollection
                                |> Tuple.mapBoth hasUniqueIds hasUniqueIds
                                |> Expect.equal ( True, True )
                        )
                        from
                        offset
            ]
        ]


toListWithoutIds : IdCollection tag item -> List item
toListWithoutIds =
    IdCollection.toList >> List.map .item


testEntryInCollection : (Int -> IdCollection CollectionTag item -> Entry CollectionTag item -> Expectation) -> List item -> Float -> Expectation
testEntryInCollection test list offset =
    IdCollection.fromList Tag list
        |> (\collection ->
                let
                    index =
                        offsetToIndex list offset

                    collectionList =
                        IdCollection.toList collection
                in
                case List.getAt index collectionList of
                    Just entry ->
                        test index collection entry

                    Nothing ->
                        if List.length list == 0 then
                            -- If the list is empty, there is no entry. This doesn't mean that the function under test failed.
                            Expect.pass

                        else
                            Expect.fail ("Error getting item by offset. Chosen index was:\n" ++ Debug.toString index)
           )


offsetToIndex : List item -> Float -> Int
offsetToIndex list offset =
    toFloat (List.length list - 1)
        * offset
        |> floor
