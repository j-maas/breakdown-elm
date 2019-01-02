port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global as Global
import Html.Styled as Html
    exposing
        ( Html
        , button
        , div
        , label
        , li
        , span
        , text
        , ul
        )
import Html.Styled.Attributes exposing (attribute, css, title, type_, value)
import Html.Styled.Events exposing (onInput, onSubmit, stopPropagationOn)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Zipper as Zipper exposing (Zipper)
import Todo exposing (Todo)
import TodoCollection exposing (TodoCollection)
import Url exposing (Url)
import Utils.NonEmptyString as NonEmptyString exposing (NonEmptyString)
import Utils.ZipperUtils as Zipper


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }



-- INIT


type alias Model =
    { key : Nav.Key
    , newTodoInput : String
    , todos : TodoCollection
    , editing : Maybe EditingInfo
    }


type alias EditingInfo =
    { todoId : TodoCollection.Id
    , rawNewAction : String
    , oldAction : NonEmptyString
    }


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ key =
    let
        todos =
            decodeFlags flags
                |> Maybe.withDefault TodoCollection.empty
    in
    ( { key = key
      , newTodoInput = ""
      , todos = todos
      , editing = Nothing
      }
    , Cmd.none
    )


decodeFlags : Decode.Value -> Maybe TodoCollection
decodeFlags flags =
    let
        currentTodos =
            decodeTodos "currentTodos" flags

        doneTodos =
            decodeTodos "doneTodos" flags

        decodeTodos field value =
            Decode.decodeValue
                (Decode.field field
                    (Decode.list Todo.decoder)
                )
                value
    in
    Result.map2
        (\current done ->
            TodoCollection.init { current = current, done = done }
        )
        currentTodos
        doneTodos
        |> Result.toMaybe



-- UPDATE


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo
    | Move TodoCollection.Id
    | Remove TodoCollection.Id
    | StartEdit TodoCollection.Id
    | UpdateEdit TodoCollection.Id NonEmptyString String
    | ApplyEdit
    | CancelEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateNewTodoInput newTodo ->
            let
                newModel =
                    { model | newTodoInput = newTodo }
            in
            ( newModel, Cmd.none )

        AddNewTodo ->
            let
                maybeAction =
                    NonEmptyString.fromString model.newTodoInput
            in
            case maybeAction of
                Just action ->
                    let
                        todo =
                            Todo.from action
                    in
                    ( { model
                        | newTodoInput = ""
                        , todos = TodoCollection.put TodoCollection.Current todo model.todos
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Move id ->
            ( invalidateTodoWithId id
                model
                TodoCollection.move
            , Cmd.none
            )

        Remove id ->
            ( invalidateTodoWithId id
                model
                TodoCollection.remove
            , Cmd.none
            )

        StartEdit id ->
            case TodoCollection.find id model.todos of
                Just zipper ->
                    let
                        todo =
                            TodoCollection.current zipper
                    in
                    ( { model
                        | editing =
                            Just
                                { todoId = id
                                , rawNewAction = Todo.readAction todo
                                , oldAction = Todo.action todo
                                }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        UpdateEdit id oldAction rawNewAction ->
            let
                editInfo : EditingInfo
                editInfo =
                    { todoId = id
                    , rawNewAction = rawNewAction
                    , oldAction = oldAction
                    }

                newTodos =
                    NonEmptyString.fromString editInfo.rawNewAction
                        |> Maybe.andThen
                            (\newAction ->
                                TodoCollection.find editInfo.todoId model.todos
                                    |> Maybe.map
                                        (\zipper ->
                                            TodoCollection.mapTodo (Todo.setAction newAction) zipper
                                        )
                            )
                        |> Maybe.withDefault model.todos
            in
            ( { model | todos = newTodos, editing = Just editInfo }, Cmd.none )

        ApplyEdit ->
            ( { model | editing = Nothing }, Cmd.none )

        CancelEdit ->
            let
                newTodos =
                    model.editing
                        |> Maybe.andThen
                            (\editInfo ->
                                TodoCollection.find editInfo.todoId model.todos
                                    |> Maybe.map
                                        (\zipper ->
                                            TodoCollection.mapTodo (Todo.setAction editInfo.oldAction) zipper
                                        )
                            )
                        |> Maybe.withDefault model.todos
            in
            ( { model | todos = newTodos, editing = Nothing }, Cmd.none )
    )
        |> (\( mdl, cmd ) ->
                ( mdl, Cmd.batch [ cmd, save mdl ] )
           )


invalidateTodoWithId : TodoCollection.Id -> Model -> (TodoCollection.Zipper -> TodoCollection) -> Model
invalidateTodoWithId id model doUpdate =
    let
        newEditing =
            Maybe.andThen
                (\editInfo ->
                    if editInfo.todoId == id then
                        Nothing

                    else
                        model.editing
                )
                model.editing

        newTodos =
            case TodoCollection.find id model.todos of
                Just zipper ->
                    doUpdate zipper

                Nothing ->
                    model.todos
    in
    { model | editing = newEditing, todos = newTodos }


{-| Command that saves the tasks collections persistently.
-}
save : Model -> Cmd msg
save model =
    let
        getTodos selector =
            TodoCollection.mapToList selector (\_ todo -> todo)
    in
    Encode.object
        [ ( "currentTodos", Encode.list Todo.encode <| getTodos TodoCollection.Current model.todos )
        , ( "doneTodos", Encode.list Todo.encode <| getTodos TodoCollection.Done model.todos )
        ]
        |> saveRaw


{-| Port for saving an encoded model to localStorage.
-}
port saveRaw : Encode.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Breakdown"
    , body =
        List.map Html.toUnstyled
            [ Global.global
                [ Global.body
                    [ maxWidth (em 26)
                    , margin2 (em 1) auto
                    , fontFamily sansSerif
                    ]
                ]
            , newTodoInput model.newTodoInput
            , viewCurrentTodos model.todos model.editing
            , viewDoneTodos model.todos model.editing
            ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentNewTodoInput =
    Html.form
        [ onSubmit AddNewTodo
        , css
            [ inputContainerStyle
            ]
        ]
        [ input
            [ type_ "text"
            , onInput UpdateNewTodoInput
            , value currentNewTodoInput
            , css
                [ width (pct 100)
                , boxSizing borderBox
                , padding (em 0.75)
                , fontSize (pct 100)
                , height (em 3)
                , marginRight (em 0.5)
                ]
            ]
            []
        , inputSubmit "Add new todo" "add"
        ]


viewCurrentTodos : TodoCollection -> Maybe EditingInfo -> Html Msg
viewCurrentTodos todos editing =
    ul [ css [ todoListStyle ] ]
        (todos
            |> TodoCollection.mapToList TodoCollection.Current
                (\id todo ->
                    let
                        currentEdit =
                            Maybe.andThen
                                (\editInfo ->
                                    if editInfo.todoId == id then
                                        Just editInfo

                                    else
                                        Nothing
                                )
                                editing
                    in
                    li [ css [ todoListEntryStyle ] ]
                        [ case currentEdit of
                            Just editInfo ->
                                viewEditTodo id todo editInfo

                            Nothing ->
                                viewTodo id todo
                        ]
                )
        )


viewDoneTodos : TodoCollection -> Maybe EditingInfo -> Html Msg
viewDoneTodos todos editing =
    ul [ css [ textDecoration lineThrough, todoListStyle ] ]
        (todos
            |> TodoCollection.mapToList TodoCollection.Done
                (\id todo ->
                    let
                        currentEdit =
                            Maybe.andThen
                                (\editInfo ->
                                    if editInfo.todoId == id then
                                        Just editInfo

                                    else
                                        Nothing
                                )
                                editing
                    in
                    li
                        [ css
                            [ hover [ opacity (num 1) ]
                            , opacity (num 0.6)
                            , todoListEntryStyle
                            ]
                        ]
                        [ case currentEdit of
                            Just editInfo ->
                                viewEditTodo id todo editInfo

                            Nothing ->
                                viewTodo id todo
                        ]
                )
        )


viewTodo : TodoCollection.Id -> Todo -> Html Msg
viewTodo id todo =
    let
        ( iconName, moveText ) =
            case TodoCollection.selectorFromId id of
                TodoCollection.Current ->
                    ( "done", "Mark as done" )

                TodoCollection.Done ->
                    ( "refresh", "Mark as to do" )
    in
    div [ css [ inputContainerStyle ], onClick (StartEdit id) ]
        [ text (Todo.readAction todo)
        , div []
            [ button moveText iconName (Move id)
            ]
        ]


viewEditTodo : TodoCollection.Id -> Todo -> EditingInfo -> Html Msg
viewEditTodo id todo editInfo =
    div [ css [ inputContainerStyle ], onClick ApplyEdit ]
        [ Html.form [ onSubmit ApplyEdit ]
            [ input
                [ type_ "text"
                , onInput (UpdateEdit id editInfo.oldAction)
                , value editInfo.rawNewAction
                , css [ width (pct 100), boxSizing borderBox ]
                ]
                []
            ]
        , div []
            [ button "Undo changes" "undo" CancelEdit
            , button "Remove" "delete" (Remove id)
            ]
        ]



-- COMPONENTS


button : String -> String -> Msg -> Html Msg
button description iconName action =
    Html.button
        [ onClick action, css [ buttonStyle, icon iconName ], title description ]
        [ span [ css [ visuallyHidden ] ] [ text description ] ]


inputSubmit : String -> String -> Html Msg
inputSubmit description iconName =
    label []
        [ span [ css [ visuallyHidden ] ] [ text description ]
        , input
            [ type_ "submit"
            , css [ buttonStyle, icon iconName, color transparent ]
            , title description
            ]
            []
        ]



-- STYLES


buttonStyle : Css.Style
buttonStyle =
    Css.batch
        [ borderRadius (em 0.5)
        , backgroundColor (hsl 0.0 0.0 0.9)
        , border zero
        , padding (em 0.5)
        , margin (em 0.1)
        , textAlign center
        , hover [ backgroundColor (hsl 0.0 0.0 0.92) ]
        , active [ backgroundColor (hsl 0.0 0.0 0.88) ]
        ]


icon : String -> Css.Style
icon iconName =
    Css.batch
        [ backgroundImage (url <| "./icons/" ++ iconName ++ ".svg")
        , backgroundRepeat noRepeat
        , backgroundPosition center
        , backgroundSize (pct 75)
        , width (em 3)
        , height (em 3)
        ]


todoListStyle : Css.Style
todoListStyle =
    Css.batch
        [ listStyle none
        , padding zero
        ]


todoListEntryStyle : Css.Style
todoListEntryStyle =
    Css.batch
        [ borderBottom3 (px 1) solid (hsla 0.0 0.0 0.0 0.1)
        , hover
            [ backgroundColor (hsla 0.0 0.0 0.0 0.02)
            ]
        ]


inputContainerStyle : Css.Style
inputContainerStyle =
    Css.batch
        [ property "display" "grid"
        , property "grid-template-columns" "1fr auto"
        , property "grid-gap" "0.5em"
        , alignItems center
        , padding (em 0.5)
        ]


{-| Hides an element visually, but keeps it discoverable to assistive technologies.
See <https://www.w3.org/WAI/tutorials/forms/labels/#note-on-hiding-elements> for further information.
-}
visuallyHidden : Css.Style
visuallyHidden =
    Css.batch
        [ border zero
        , property "clip" "rect(0 0 0 0)"
        , height (px 1)
        , margin (px -1)
        , overflow hidden
        , padding zero
        , position absolute
        , whiteSpace noWrap
        , width (px 1)
        ]



-- EVENTS


input : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
input attributes children =
    Html.input (attributes ++ [ stopPropagation ]) children


stopPropagation : Html.Attribute Msg
stopPropagation =
    stopPropagationOn "click" (Decode.succeed ( NoOp, True ))


onClick : Msg -> Html.Attribute Msg
onClick msg =
    stopPropagationOn "click" (Decode.succeed ( msg, True ))
