module TodoTree exposing
    ( Id
    , TodoNode(..)
    , TodoTree
    , empty
    , encodeNode
    , fromItems
    , get
    , insertCurrent
    , insertCurrentAt
    , insertDone
    , insertDoneAt
    , mapCurrent
    , mapDone
    , moveToCurrent
    , moveToDone
    , nodeDecoder
    , remove
    , update
    )

import Checklist exposing (Checklist)
import Json.Decode as Decode
import Json.Encode as Encode
import Todo exposing (Todo)


type TodoTree
    = TodoTree Subtodos


type TodoNode
    = SimpleTodo Todo
    | CompositTodo Todo Subtodos


type alias Subtodos =
    Checklist TodoNode


encodeNode : TodoNode -> Encode.Value
encodeNode node =
    case node of
        SimpleTodo todo ->
            Todo.encode todo

        CompositTodo todo subtodos ->
            Encode.object
                [ ( "todo", Todo.encode todo )
                , ( "current"
                  , Encode.list identity <|
                        Checklist.mapCurrent
                            (\_ subnode -> encodeNode subnode)
                            subtodos
                  )
                , ( "done"
                  , Encode.list identity <|
                        Checklist.mapDone
                            (\_ subnode -> encodeNode subnode)
                            subtodos
                  )
                ]


nodeDecoder : Decode.Decoder TodoNode
nodeDecoder =
    Decode.oneOf
        [ Todo.decoder |> Decode.map SimpleTodo
        , Decode.map3
            (\todo current done ->
                CompositTodo todo (Checklist.fromItems { current = current, done = done })
            )
            (Decode.field "todo" Todo.decoder)
            (Decode.field "current" (Decode.list (Decode.lazy (\_ -> nodeDecoder))))
            (Decode.field "done" (Decode.list (Decode.lazy (\_ -> nodeDecoder))))
        ]


type Id
    = Id Checklist.Id (List Checklist.Id)


appendId : Id -> Checklist.Id -> Id
appendId (Id first following) newId =
    Id first (following ++ [ newId ])


empty : TodoTree
empty =
    TodoTree <| Checklist.fromItems { current = [], done = [] }


fromItems : { current : List TodoNode, done : List TodoNode } -> TodoTree
fromItems items =
    Checklist.fromItems items
        |> TodoTree


insertCurrent : TodoNode -> TodoTree -> ( Id, TodoTree )
insertCurrent node (TodoTree checklist) =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertCurrent node checklist
    in
    ( Id checklistId [], TodoTree newChecklist )


insertDone : TodoNode -> TodoTree -> ( Id, TodoTree )
insertDone node (TodoTree checklist) =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertDone node checklist
    in
    ( Id checklistId [], TodoTree newChecklist )


insertCurrentAt : Id -> TodoNode -> TodoTree -> Maybe ( Id, TodoTree )
insertCurrentAt id node (TodoTree checklist) =
    updateChecklistWithContext
        (\foundId foundChecklist ->
            let
                item =
                    Checklist.get foundId foundChecklist
            in
            case item of
                Just (CompositTodo todo subtodos) ->
                    let
                        ( checklistId, newSubtodos ) =
                            Checklist.insertCurrent node subtodos
                    in
                    Checklist.update
                        (\_ -> CompositTodo todo newSubtodos)
                        foundId
                        foundChecklist
                        |> Maybe.map
                            (\updatedChecklist ->
                                ( updatedChecklist, checklistId )
                            )

                _ ->
                    Nothing
        )
        id
        checklist
        |> Maybe.map
            (\( updatedChecklist, checklistId ) ->
                ( appendId id checklistId, TodoTree updatedChecklist )
            )


insertDoneAt : Id -> TodoNode -> TodoTree -> Maybe ( Id, TodoTree )
insertDoneAt id node (TodoTree checklist) =
    updateChecklistWithContext
        (\foundId foundChecklist ->
            let
                item =
                    Checklist.get foundId foundChecklist
            in
            case item of
                Just (CompositTodo todo subtodos) ->
                    let
                        ( checklistId, newSubtodos ) =
                            Checklist.insertDone node subtodos
                    in
                    Checklist.update
                        (\_ -> CompositTodo todo newSubtodos)
                        foundId
                        foundChecklist
                        |> Maybe.map
                            (\updatedChecklist ->
                                ( updatedChecklist, checklistId )
                            )

                _ ->
                    Nothing
        )
        id
        checklist
        |> Maybe.map
            (\( updatedChecklist, checklistId ) ->
                ( appendId id checklistId, TodoTree updatedChecklist )
            )


mapCurrent : (Id -> TodoNode -> b) -> TodoTree -> List b
mapCurrent mapping (TodoTree checklist) =
    Checklist.mapCurrent
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapDone : (Id -> TodoNode -> b) -> TodoTree -> List b
mapDone mapping (TodoTree checklist) =
    Checklist.mapDone
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapCurrentSubtodos : (Id -> TodoNode -> b) -> Id -> Subtodos -> List b
mapCurrentSubtodos mapping (Id first following) subtodos =
    let
        nextId checklistId =
            Id first (following ++ [ checklistId ])
    in
    Checklist.mapCurrent
        (\checklistId node -> mapping (nextId checklistId) node)
        subtodos


mapDoneSubtodos : (Id -> TodoNode -> b) -> Id -> Subtodos -> List b
mapDoneSubtodos mapping (Id first following) subtodos =
    let
        nextId checklistId =
            Id first (following ++ [ checklistId ])
    in
    Checklist.mapDone
        (\checklistId node -> mapping (nextId checklistId) node)
        subtodos


get : Id -> TodoTree -> Maybe TodoNode
get id (TodoTree checklist) =
    findChecklist id checklist
        |> Maybe.andThen
            (\( foundId, foundChecklist ) ->
                Checklist.get foundId foundChecklist
            )


update : (TodoNode -> TodoNode) -> Id -> TodoTree -> Maybe TodoTree
update mapping id (TodoTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.update mapping foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map TodoTree


remove : Id -> TodoTree -> Maybe TodoTree
remove id (TodoTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.remove foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map TodoTree


moveToCurrent : Id -> TodoTree -> Maybe TodoTree
moveToCurrent id (TodoTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToCurrent foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map TodoTree


moveToDone : Id -> TodoTree -> Maybe TodoTree
moveToDone id (TodoTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToDone foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map TodoTree


findChecklist : Id -> Checklist TodoNode -> Maybe ( Checklist.Id, Checklist TodoNode )
findChecklist (Id first following) checklist =
    case following of
        [] ->
            Just ( first, checklist )

        nextId :: remaining ->
            case Checklist.get first checklist of
                Just (CompositTodo todo subtodos) ->
                    findChecklist (Id nextId remaining) subtodos

                _ ->
                    Nothing


updateChecklistWithContext :
    (Checklist.Id -> Checklist TodoNode -> Maybe ( Checklist TodoNode, a ))
    -> Id
    -> Checklist TodoNode
    -> Maybe ( Checklist TodoNode, a )
updateChecklistWithContext mapping (Id first following) checklist =
    case following of
        [] ->
            mapping first checklist

        nextId :: remaining ->
            case Checklist.get first checklist of
                Just (CompositTodo todo subtodos) ->
                    case updateChecklistWithContext mapping (Id nextId remaining) subtodos of
                        Just ( newSubtodos, a ) ->
                            Checklist.update (\_ -> CompositTodo todo newSubtodos)
                                first
                                checklist
                                |> Maybe.map (\newChecklist -> ( newChecklist, a ))

                        _ ->
                            Nothing

                _ ->
                    Nothing


updateChecklist :
    (Checklist.Id -> Checklist TodoNode -> Maybe (Checklist TodoNode))
    -> Id
    -> Checklist TodoNode
    -> Maybe (Checklist TodoNode)
updateChecklist mapping id checklist =
    updateChecklistWithContext
        (\foundId foundChecklist ->
            mapping foundId foundChecklist
                |> Maybe.map (\newChecklist -> ( newChecklist, () ))
        )
        id
        checklist
        |> Maybe.map (\( updatedChecklist, _ ) -> updatedChecklist)
