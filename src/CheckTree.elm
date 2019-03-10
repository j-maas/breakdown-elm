module CheckTree exposing
    ( CheckTree
    , Id
    , Node(..)
    , Subnodes
    , empty
    , emptySubnodes
    , encodeNode
    , fromItems
    , get
    , insertCurrent
    , insertCurrentAt
    , insertDone
    , insertDoneAt
    , makeCompositNode
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


type CheckTree a
    = CheckTree (Entries a)


type alias Entries a =
    Checklist (Node a)


type Node a
    = SimpleNode a
    | CompositNode a (Subnodes a)


makeCompositNode : a -> Entries a -> Node a
makeCompositNode todo entries =
    CompositNode todo (Subnodes entries)


{-| Separate type from CheckTree to ensure ID safety.
-}
type Subnodes a
    = Subnodes (Entries a)


emptySubnodes : Subnodes a
emptySubnodes =
    Subnodes Checklist.empty


type Id
    = Id Checklist.Id (List Checklist.Id)


appendId : Id -> Checklist.Id -> Id
appendId (Id first following) newId =
    Id first (following ++ [ newId ])



-- BUILD


empty : CheckTree a
empty =
    Checklist.fromItems { current = [], done = [] }
        |> CheckTree


fromItems : { current : List (Node a), done : List (Node a) } -> CheckTree a
fromItems items =
    Checklist.fromItems items
        |> CheckTree


insertCurrent : Node a -> CheckTree a -> ( Id, CheckTree a )
insertCurrent node (CheckTree checklist) =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertCurrent node checklist
    in
    ( Id checklistId [], CheckTree newChecklist )


insertDone : Node a -> CheckTree a -> ( Id, CheckTree a )
insertDone node (CheckTree checklist) =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertDone node checklist
    in
    ( Id checklistId [], CheckTree newChecklist )


insertCurrentAt : Id -> Node a -> CheckTree a -> Maybe ( Id, CheckTree a )
insertCurrentAt =
    insertAt Checklist.insertCurrent


insertDoneAt : Id -> Node a -> CheckTree a -> Maybe ( Id, CheckTree a )
insertDoneAt =
    insertAt Checklist.insertDone


insertAt :
    (Node a -> Entries a -> ( Checklist.Id, Entries a ))
    -> Id
    -> Node a
    -> CheckTree a
    -> Maybe ( Id, CheckTree a )
insertAt insert id node (CheckTree checklist) =
    updateChecklistWithContext
        (\foundId foundChecklist ->
            Checklist.get foundId foundChecklist
                |> Maybe.map
                    (\foundNode ->
                        case foundNode of
                            CompositNode todo (Subnodes subtodos) ->
                                let
                                    ( checklistId, newChecklist ) =
                                        insert node subtodos
                                in
                                ( todo, checklistId, Subnodes newChecklist )

                            SimpleNode todo ->
                                let
                                    ( checklistId, newChecklist ) =
                                        insert node Checklist.empty
                                in
                                ( todo, checklistId, Subnodes newChecklist )
                    )
                |> Maybe.andThen
                    (\( todo, checklistId, newSubtodos ) ->
                        Checklist.update
                            (\_ -> CompositNode todo newSubtodos)
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
                ( appendId id checklistId, CheckTree updatedChecklist )
            )



-- READ


mapCurrent : (Id -> Node a -> b) -> CheckTree a -> List b
mapCurrent mapping (CheckTree checklist) =
    Checklist.mapCurrent
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapDone : (Id -> Node a -> b) -> CheckTree a -> List b
mapDone mapping (CheckTree checklist) =
    Checklist.mapDone
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapCurrentSubtodos : (Id -> Node a -> b) -> Id -> Subnodes a -> List b
mapCurrentSubtodos mapping (Id first following) (Subnodes subtodos) =
    let
        nextId checklistId =
            Id first (following ++ [ checklistId ])
    in
    Checklist.mapCurrent
        (\checklistId node -> mapping (nextId checklistId) node)
        subtodos


mapDoneSubtodos : (Id -> Node a -> b) -> Id -> Subnodes a -> List b
mapDoneSubtodos mapping (Id first following) (Subnodes subtodos) =
    let
        nextId checklistId =
            Id first (following ++ [ checklistId ])
    in
    Checklist.mapDone
        (\checklistId node -> mapping (nextId checklistId) node)
        subtodos


get : Id -> CheckTree a -> Maybe (Node a)
get id (CheckTree checklist) =
    findChecklist id checklist
        |> Maybe.andThen
            (\( foundId, foundChecklist ) ->
                Checklist.get foundId foundChecklist
            )



-- MODIFY


update : (Node a -> Node a) -> Id -> CheckTree a -> Maybe (CheckTree a)
update mapping id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.update mapping foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree


remove : Id -> CheckTree a -> Maybe (CheckTree a)
remove id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.remove foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree


moveToCurrent : Id -> CheckTree a -> Maybe (CheckTree a)
moveToCurrent id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToCurrent foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree


moveToDone : Id -> CheckTree a -> Maybe (CheckTree a)
moveToDone id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToDone foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree



-- HELPERS


findChecklist : Id -> Entries a -> Maybe ( Checklist.Id, Entries a )
findChecklist (Id first following) checklist =
    case following of
        [] ->
            Just ( first, checklist )

        nextId :: remaining ->
            case Checklist.get first checklist of
                Just (CompositNode todo (Subnodes subtodos)) ->
                    findChecklist (Id nextId remaining) subtodos

                _ ->
                    Nothing


updateChecklistWithContext :
    (Checklist.Id -> Entries a -> Maybe ( Entries a, b ))
    -> Id
    -> Entries a
    -> Maybe ( Entries a, b )
updateChecklistWithContext mapping (Id first following) checklist =
    case following of
        [] ->
            mapping first checklist

        nextId :: remaining ->
            case Checklist.get first checklist of
                Just (CompositNode todo (Subnodes subtodos)) ->
                    case updateChecklistWithContext mapping (Id nextId remaining) subtodos of
                        Just ( newSubtodos, a ) ->
                            Checklist.update (\_ -> CompositNode todo (Subnodes newSubtodos))
                                first
                                checklist
                                |> Maybe.map (\newChecklist -> ( newChecklist, a ))

                        _ ->
                            Nothing

                _ ->
                    Nothing


updateChecklist :
    (Checklist.Id -> Entries a -> Maybe (Entries a))
    -> Id
    -> Entries a
    -> Maybe (Entries a)
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


contentField =
    "content"


currentField =
    "current"


doneField =
    "done"


encodeNode : (a -> Encode.Value) -> Node a -> Encode.Value
encodeNode encodeContent node =
    case node of
        SimpleNode content ->
            encodeContent content

        CompositNode content (Subnodes subtodos) ->
            Encode.object
                [ ( contentField, encodeContent content )
                , ( currentField
                  , Encode.list identity <|
                        Checklist.mapCurrent
                            (\_ subnode -> encodeNode encodeContent subnode)
                            subtodos
                  )
                , ( doneField
                  , Encode.list identity <|
                        Checklist.mapDone
                            (\_ subnode -> encodeNode encodeContent subnode)
                            subtodos
                  )
                ]


nodeDecoder : Decode.Decoder a -> Decode.Decoder (Node a)
nodeDecoder contentDecoder =
    Decode.oneOf
        [ contentDecoder |> Decode.map SimpleNode
        , Decode.map3
            (\todo current done ->
                CompositNode todo (Subnodes <| Checklist.fromItems { current = current, done = done })
            )
            (Decode.field contentField contentDecoder)
            (Decode.field currentField (Decode.list (Decode.lazy (\_ -> nodeDecoder contentDecoder))))
            (Decode.field doneField (Decode.list (Decode.lazy (\_ -> nodeDecoder contentDecoder))))
        ]
