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


type CheckTree a b
    = CheckTree (Entries a b)


type alias Entries a b =
    Checklist (Node a b)


type Node a b
    = SimpleNode a
    | CompositNode b (Subnodes a b)


makeCompositNode : b -> Entries a b -> Node a b
makeCompositNode item entries =
    CompositNode item (Subnodes entries)


{-| Separate type from CheckTree to ensure ID safety.
-}
type Subnodes a b
    = Subnodes (Entries a b)


emptySubnodes : Subnodes a b
emptySubnodes =
    Subnodes Checklist.empty


type Id
    = Id Checklist.Id (List Checklist.Id)


appendId : Id -> Checklist.Id -> Id
appendId (Id first following) newId =
    Id first (following ++ [ newId ])



-- BUILD


empty : CheckTree a b
empty =
    Checklist.fromItems { current = [], done = [] }
        |> CheckTree


fromItems : { current : List (Node a b), done : List (Node a b) } -> CheckTree a b
fromItems items =
    Checklist.fromItems items
        |> CheckTree


insertCurrent : Node a b -> CheckTree a b -> ( Id, CheckTree a b )
insertCurrent node (CheckTree checklist) =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertCurrent node checklist
    in
    ( Id checklistId [], CheckTree newChecklist )


insertDone : Node a b -> CheckTree a b -> ( Id, CheckTree a b )
insertDone node (CheckTree checklist) =
    let
        ( checklistId, newChecklist ) =
            Checklist.insertDone node checklist
    in
    ( Id checklistId [], CheckTree newChecklist )


insertCurrentAt : Id -> Node a b -> (a -> b) -> CheckTree a b -> Maybe ( Id, CheckTree a b )
insertCurrentAt =
    insertAt Checklist.insertCurrent


insertDoneAt : Id -> Node a b -> (a -> b) -> CheckTree a b -> Maybe ( Id, CheckTree a b )
insertDoneAt =
    insertAt Checklist.insertDone


insertAt :
    (Node a b -> Entries a b -> ( Checklist.Id, Entries a b ))
    -> Id
    -> Node a b
    -> (a -> b)
    -> CheckTree a b
    -> Maybe ( Id, CheckTree a b )
insertAt insert id node makeComposit (CheckTree checklist) =
    updateChecklistWithContext
        (\foundId foundChecklist ->
            Checklist.get foundId foundChecklist
                |> Maybe.map
                    (\foundNode ->
                        case foundNode of
                            CompositNode item (Subnodes subtodos) ->
                                let
                                    ( checklistId, newChecklist ) =
                                        insert node subtodos
                                in
                                ( item, checklistId, Subnodes newChecklist )

                            SimpleNode item ->
                                let
                                    ( checklistId, newChecklist ) =
                                        insert node Checklist.empty
                                in
                                ( makeComposit item, checklistId, Subnodes newChecklist )
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


mapCurrent : (Id -> Node a b -> c) -> CheckTree a b -> List c
mapCurrent mapping (CheckTree checklist) =
    Checklist.mapCurrent
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapDone : (Id -> Node a b -> c) -> CheckTree a b -> List c
mapDone mapping (CheckTree checklist) =
    Checklist.mapDone
        (\checklistId node -> mapping (Id checklistId []) node)
        checklist


mapCurrentSubtodos : (Id -> Node a b -> c) -> Id -> Subnodes a b -> List c
mapCurrentSubtodos mapping (Id first following) (Subnodes subtodos) =
    let
        nextId checklistId =
            Id first (following ++ [ checklistId ])
    in
    Checklist.mapCurrent
        (\checklistId node -> mapping (nextId checklistId) node)
        subtodos


mapDoneSubtodos : (Id -> Node a b -> c) -> Id -> Subnodes a b -> List c
mapDoneSubtodos mapping (Id first following) (Subnodes subtodos) =
    let
        nextId checklistId =
            Id first (following ++ [ checklistId ])
    in
    Checklist.mapDone
        (\checklistId node -> mapping (nextId checklistId) node)
        subtodos


get : Id -> CheckTree a b -> Maybe (Node a b)
get id (CheckTree checklist) =
    findChecklist id checklist
        |> Maybe.andThen
            (\( foundId, foundChecklist ) ->
                Checklist.get foundId foundChecklist
            )



-- MODIFY


update : (Node a b -> Node a b) -> Id -> CheckTree a b -> Maybe (CheckTree a b)
update mapping id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.update mapping foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree


remove : Id -> CheckTree a b -> Maybe (CheckTree a b)
remove id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.remove foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree


moveToCurrent : Id -> CheckTree a b -> Maybe (CheckTree a b)
moveToCurrent id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToCurrent foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree


moveToDone : Id -> CheckTree a b -> Maybe (CheckTree a b)
moveToDone id (CheckTree checklist) =
    updateChecklist
        (\foundId foundChecklist ->
            Checklist.moveToDone foundId foundChecklist
        )
        id
        checklist
        |> Maybe.map CheckTree



-- HELPERS


findChecklist : Id -> Entries a b -> Maybe ( Checklist.Id, Entries a b )
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
    (Checklist.Id -> Entries a b -> Maybe ( Entries a b, c ))
    -> Id
    -> Entries a b
    -> Maybe ( Entries a b, c )
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
    (Checklist.Id -> Entries a b -> Maybe (Entries a b))
    -> Id
    -> Entries a b
    -> Maybe (Entries a b)
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


encodeNode : (a -> Encode.Value) -> (b -> Encode.Value) -> Node a b -> Encode.Value
encodeNode encodeSimple encodeComposit node =
    case node of
        SimpleNode content ->
            encodeSimple content

        CompositNode content (Subnodes subtodos) ->
            Encode.object
                [ ( contentField, encodeComposit content )
                , ( currentField
                  , Encode.list identity <|
                        Checklist.mapCurrent
                            (\_ subnode -> encodeNode encodeSimple encodeComposit subnode)
                            subtodos
                  )
                , ( doneField
                  , Encode.list identity <|
                        Checklist.mapDone
                            (\_ subnode -> encodeNode encodeSimple encodeComposit subnode)
                            subtodos
                  )
                ]


nodeDecoder : Decode.Decoder a -> Decode.Decoder b -> Decode.Decoder (Node a b)
nodeDecoder simpleContentDecoder compositContentDecoder =
    Decode.oneOf
        [ simpleContentDecoder |> Decode.map SimpleNode
        , Decode.map3
            (\todo current done ->
                CompositNode todo (Subnodes <| Checklist.fromItems { current = current, done = done })
            )
            (Decode.field contentField compositContentDecoder)
            (Decode.field currentField (Decode.list (Decode.lazy (\_ -> nodeDecoder simpleContentDecoder compositContentDecoder))))
            (Decode.field doneField (Decode.list (Decode.lazy (\_ -> nodeDecoder simpleContentDecoder compositContentDecoder))))
        ]
