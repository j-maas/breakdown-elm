module TestChecklist exposing (suite)

import Checklist exposing (Checklist)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, float, int, list, percentage, string)
import List.Extra as List
import Test exposing (..)


suite : Test
suite =
    describe "Checklist"
        [ fuzz2 checklistFuzzer int "inserts and retrieves item by id" <|
            \checklist item ->
                Checklist.insertCurrent item checklist
                    |> (\( id, cl ) ->
                            Checklist.get id cl
                       )
                    |> Expect.equal (Just item)
        , fuzz2 checkTransformOptionsFuzzer int "updates item by id" <|
            \checkTransformOptions newItem ->
                checkTransform checkTransformOptions
                    (\id _ checklist -> Checklist.update (\_ -> newItem) id checklist)
                    (\{ index, list } other ->
                        ( List.setAt index newItem list, other )
                    )
        , fuzz checkTransformOptionsFuzzer "removes item by id" <|
            \checkTransformOptions ->
                checkTransform checkTransformOptions
                    (\id _ checklist -> Checklist.remove id checklist)
                    (\{ index, list } other -> ( List.removeAt index list, other ))
        , fuzz checkTransformOptionsFuzzer "moves item to current" <|
            \checkTransformOptions ->
                let
                    sanitizedOptions =
                        { checkTransformOptions | selector = Done }
                in
                checkTransform sanitizedOptions
                    (\id _ checklist -> Checklist.moveToCurrent id checklist)
                    (\{ index, item, list } other -> ( List.removeAt index list, other ++ [ item ] ))
        , fuzz checkTransformOptionsFuzzer "moves item to done" <|
            \checkTransformOptions ->
                let
                    sanitizedOptions =
                        { checkTransformOptions | selector = Current }
                in
                checkTransform sanitizedOptions
                    (\id _ checklist -> Checklist.moveToDone id checklist)
                    (\{ index, item, list } other -> ( List.removeAt index list, other ++ [ item ] ))
        ]


checklistFuzzer : Fuzzer (Checklist Int)
checklistFuzzer =
    Fuzz.map2
        (\current done ->
            Checklist.fromItems { current = current, done = done }
        )
        (list int)
        (list int)


nonemptyListFuzzer : Fuzzer a -> Fuzzer (List a)
nonemptyListFuzzer itemFuzzer =
    Fuzz.map2 (\item list -> [ item ] ++ list)
        itemFuzzer
        (list itemFuzzer)


type Selector
    = Current
    | Done


type alias CheckTransformOptions =
    { percentage : Float
    , selector : Selector
    , nonEmpty : List Int
    , list : List Int
    }


checkTransformOptionsFuzzer : Fuzzer CheckTransformOptions
checkTransformOptionsFuzzer =
    Fuzz.map4
        (\percentage isCurrent nonEmpty list ->
            let
                selector =
                    if isCurrent then
                        Current

                    else
                        Done
            in
            CheckTransformOptions percentage selector nonEmpty list
        )
        percentage
        bool
        (nonemptyListFuzzer int)
        (list int)


checkTransform :
    CheckTransformOptions
    -> (Checklist.Id -> Int -> Checklist Int -> Maybe (Checklist Int))
    -> ({ index : Int, item : Int, list : List Int } -> List Int -> ( List Int, List Int ))
    -> Expectation
checkTransform options transformChecklist transformList =
    let
        checklistBuilder nonEmpty aList =
            case options.selector of
                Current ->
                    Checklist.fromItems { current = nonEmpty, done = aList }

                Done ->
                    Checklist.fromItems { current = aList, done = nonEmpty }

        checklist =
            checklistBuilder options.nonEmpty options.list

        list =
            case options.selector of
                Current ->
                    Checklist.mapCurrent Tuple.pair checklist

                Done ->
                    Checklist.mapDone Tuple.pair checklist

        check chosenIndex id item =
            let
                newChecklist =
                    transformChecklist id item checklist

                ( newNonEmpty, newList ) =
                    transformList { index = chosenIndex, item = item, list = options.nonEmpty } options.list

                expectedChecklist =
                    checklistBuilder newNonEmpty newList
            in
            Expect.equal (Just expectedChecklist) newChecklist
    in
    case List.length list of
        0 ->
            Expect.fail "getAtIndex: The provided list was empty. Make sure that the list is not empty."

        n ->
            let
                chosenIndex =
                    (toFloat (n - 1) * options.percentage)
                        |> Basics.floor
            in
            List.getAt chosenIndex list
                |> Maybe.map (\( id, item ) -> check chosenIndex id item)
                |> Maybe.withDefault (Expect.fail "getAtIndex: Expected index to be valid, but received Nothing.")
