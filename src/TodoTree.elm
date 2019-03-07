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
    , mapCurrentSubtodos
    , mapDone
    , mapDoneSubtodos
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


type alias TodoTree =
    Subtodos


type TodoNode
    = SimpleTodo Todo
    | CompositTodo Todo Subtodos


type alias Subtodos =
    Checklist TodoNode


type Id
    = Id Checklist.Id (List Checklist.Id)


appendId : Id -> Checklist.Id -> Id
appendId (Id first following) newId =
    Id first (following ++ [ newId ])



-- BUILD


empty : TodoTree
empty =
    Checklist.fromItems { current = [], done = [] }


fromItems : { current : List TodoNode, done : List TodoNode } -> TodoTree
fromItems items =
    Checklist.fromItems items


insertCurrent : TodoNode -> TodoTree -> ( Id, TodoTree )
insertCurrent node checklist =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertCurrent node checklist
    in
    ( Id checklistId [], newChecklist )


insertDone : TodoNode -> TodoTree -> ( Id, TodoTree )
insertDone node checklist =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertDone node checklist
    in
    ( Id checklistId [], newChecklist )


insertCurrentAt : Id -> TodoNode -> TodoTree -> Maybe ( Id, TodoTree )
insertCurrentAt =
    insertAt Checklist.insertCurrent


insertDoneAt : Id -> TodoNode -> TodoTree -> Maybe ( Id, TodoTree )
insertDoneAt =
    insertAt Checklist.insertDone


insertAt : (TodoNode -> Checklist TodoNode -> ( Checklist.Id, Checklist TodoNode )) -> Id -> TodoNode -> TodoTree -> Maybe ( Id, TodoTree )
insertAt insert id node checklist =
    updateChecklistWithContext
        (\foundId foundChecklist ->
            Checklist.get foundId foundChecklist
                |> Maybe.map
                    (\foundNode ->
                        case foundNode of
                            CompositTodo todo subtodos ->
                                let
                                    ( checklistId, newChecklist ) =
                                        insert node subtodos
                                in
                                ( todo, checklistId, newChecklist )

                            SimpleTodo todo ->
                                let
                                    ( checklistId, newChecklist ) =
                                        insert node Checklist.empty
                                in
                                ( todo, checklistId, newChecklist )
                    )
                |> Maybe.andThen
                    (\( todo, checklistId, newSubtodos ) ->
                        Checklist.update
                            (\_ -> CompositTodo todo newSubtodos)
                            foundId
                            foundChecklist
                            |> Maybe.map
                                (\updatedChecklist ->
                                    ( updatedChecklist, checklistId )
                                )
                    )
        )
        id
        checklist
        |> Maybe.map
            (\( updatedChecklist, checklistId ) ->
                ( appendId id checklistId, updatedChecklist )
            )



-- READ


mapCurrent : (Id -> TodoNode -> b) -> TodoTree -> List b
mapCurrent mapping checklist =
    Checklist.mapCurrent
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapDone : (Id -> TodoNode -> b) -> TodoTree -> List b
mapDone mapping checklist =
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
get id checklist =
    findChecklist id checklist
        |> Maybe.andThen
            (\( foundId, foundChecklist ) ->
                Checklist.get foundId foundChecklist
            )



-- MODIFY


update : (TodoNode -> TodoNode) -> Id -> TodoTree -> Maybe TodoTree
update mapping id checklist =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.update mapping foundId foundChecklist
        )
        id
        checklist


remove : Id -> TodoTree -> Maybe TodoTree
remove id checklist =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.remove foundId foundChecklist
        )
        id
        checklist


moveToCurrent : Id -> TodoTree -> Maybe TodoTree
moveToCurrent id checklist =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToCurrent foundId foundChecklist
        )
        id
        checklist


moveToDone : Id -> TodoTree -> Maybe TodoTree
moveToDone id checklist =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToDone foundId foundChecklist
        )
        id
        checklist



-- HELPERS


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



-- JSON


todoField =
    "todo"


currentField =
    "current"


doneField =
    "done"


encodeNode : TodoNode -> Encode.Value
encodeNode node =
    case node of
        SimpleTodo todo ->
            Todo.encode todo

        CompositTodo todo subtodos ->
            Encode.object
                [ ( todoField, Todo.encode todo )
                , ( currentField
                  , Encode.list identity <|
                        Checklist.mapCurrent
                            (\_ subnode -> encodeNode subnode)
                            subtodos
                  )
                , ( doneField
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
            (Decode.field todoField Todo.decoder)
            (Decode.field currentField (Decode.list (Decode.lazy (\_ -> nodeDecoder))))
            (Decode.field doneField (Decode.list (Decode.lazy (\_ -> nodeDecoder))))
        ]
