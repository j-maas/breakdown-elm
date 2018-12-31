module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (..)
import Css.Global as Global
import Html.Styled as Html
    exposing
        ( Html
        , button
        , div
        , input
        , label
        , li
        , span
        , text
        , ul
        )
import Html.Styled.Attributes exposing (attribute, css, title, type_, value)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import List.Zipper as Zipper exposing (Zipper)
import Todo exposing (Todo)
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


type alias Model =
    { key : Nav.Key
    , newTodoInput : String
    , currentTodos : List (TodoEntry Current)
    , doneTodos : List (TodoEntry Done)
    , editing : Maybe EditingInfo
    }


type TodoEntry a
    = TodoEntry Todo


todoFromEntry : TodoEntry a -> Todo
todoFromEntry (TodoEntry todo) =
    todo


type alias EditingInfo =
    { todo : TodoZipper
    , oldAction : NonEmptyString
    }


type TodoZipper
    = CurrentZipper (Zipper (TodoEntry Current))
    | DoneZipper (Zipper (TodoEntry Done))


zipperFromTodoZipper : TodoZipper -> Zipper Todo
zipperFromTodoZipper todoZipper =
    case todoZipper of
        CurrentZipper zipper ->
            Zipper.map todoFromEntry zipper

        DoneZipper zipper ->
            Zipper.map todoFromEntry zipper


mapTodoZipper : (Todo -> Todo) -> TodoZipper -> TodoZipper
mapTodoZipper map todoZipper =
    case todoZipper of
        CurrentZipper zipper ->
            CurrentZipper (Zipper.mapCurrent (todoFromEntry >> map >> TodoEntry) zipper)

        DoneZipper zipper ->
            DoneZipper (Zipper.mapCurrent (todoFromEntry >> map >> TodoEntry) zipper)


type Current
    = Current


type Done
    = Done


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , newTodoInput = ""
      , currentTodos =
            [ TodoEntry <| Todo.from (NonEmptyString.build 'H' "ello")
            , TodoEntry <| Todo.from (NonEmptyString.build 't' "here")
            ]
      , doneTodos = [ TodoEntry <| Todo.from (NonEmptyString.build 'H' "ow's it going?") ]
      , editing = Nothing
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo
    | Move TodoZipper
    | Remove TodoZipper
    | StartEdit TodoZipper
    | UpdateEdit TodoZipper NonEmptyString String
    | ApplyEdit
    | CancelEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

                        newCurrentTodos =
                            model.currentTodos ++ [ TodoEntry todo ]
                    in
                    ( { model
                        | newTodoInput = ""
                        , currentTodos = newCurrentTodos
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Move todoZipper ->
            let
                ( newCurrent, newDone ) =
                    case todoZipper of
                        CurrentZipper zipper ->
                            let
                                currentZipper =
                                    Zipper.map todoFromEntry zipper

                                done =
                                    List.map todoFromEntry model.doneTodos

                                ( rawNewCurrent, rawNewDone ) =
                                    Zipper.move currentZipper done
                            in
                            ( List.map TodoEntry rawNewCurrent, List.map TodoEntry rawNewDone )

                        DoneZipper zipper ->
                            let
                                doneZipper =
                                    Zipper.map todoFromEntry zipper

                                current =
                                    List.map todoFromEntry model.currentTodos

                                ( rawNewDone, rawNewCurrent ) =
                                    Zipper.move doneZipper current
                            in
                            ( List.map TodoEntry rawNewCurrent, List.map TodoEntry rawNewDone )
            in
            ( { model | currentTodos = newCurrent, doneTodos = newDone }, Cmd.none )

        Remove todoZipper ->
            case todoZipper of
                CurrentZipper zipper ->
                    ( { model | currentTodos = Zipper.remove zipper }, Cmd.none )

                DoneZipper zipper ->
                    ( { model | doneTodos = Zipper.remove zipper }, Cmd.none )

        StartEdit todoZipper ->
            let
                zipper =
                    zipperFromTodoZipper todoZipper

                todo =
                    Zipper.current zipper
            in
            ( { model
                | editing =
                    Just
                        { todo = todoZipper
                        , oldAction = Todo.action todo
                        }
              }
            , Cmd.none
            )

        UpdateEdit todoZipper oldAction rawNewAction ->
            let
                newTodoZipper =
                    case NonEmptyString.fromString rawNewAction of
                        Just newAction ->
                            mapTodoZipper (Todo.setAction newAction) todoZipper

                        Nothing ->
                            todoZipper
            in
            ( { model
                | editing =
                    Just
                        { todo = newTodoZipper
                        , oldAction = oldAction
                        }
              }
            , Cmd.none
            )

        ApplyEdit ->
            case model.editing of
                Just editInfo ->
                    case editInfo.todo of
                        CurrentZipper zipper ->
                            ( { model | currentTodos = Zipper.toList zipper, editing = Nothing }, Cmd.none )

                        DoneZipper zipper ->
                            ( { model | doneTodos = Zipper.toList zipper, editing = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CancelEdit ->
            case model.editing of
                Just editInfo ->
                    let
                        restoredZipper =
                            mapTodoZipper (Todo.setAction editInfo.oldAction) editInfo.todo
                    in
                    case restoredZipper of
                        CurrentZipper zipper ->
                            ( { model | currentTodos = Zipper.toList zipper, editing = Nothing }, Cmd.none )

                        DoneZipper zipper ->
                            ( { model | doneTodos = Zipper.toList zipper, editing = Nothing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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
            , viewCurrentTodos model.currentTodos model.editing
            , viewDoneTodos model.doneTodos model.editing
            ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentNewTodoInput =
    Html.form [ onSubmit AddNewTodo ]
        [ input [ type_ "text", onInput UpdateNewTodoInput, value currentNewTodoInput ] []
        , inputSubmit "Add new todo" "add"
        ]


viewCurrentTodos : List (TodoEntry Current) -> Maybe EditingInfo -> Html Msg
viewCurrentTodos todos editing =
    ul [ css [ todoListStyle ] ]
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    let
                        currentEdit =
                            Maybe.andThen
                                (\editInfo ->
                                    if editInfo.todo == CurrentZipper todoZipper then
                                        Just editInfo

                                    else
                                        Nothing
                                )
                                editing
                    in
                    li [ css [ todoListEntryStyle ] ]
                        [ case currentEdit of
                            Just editInfo ->
                                viewEditTodo (CurrentZipper todoZipper) editInfo

                            Nothing ->
                                viewTodo (CurrentZipper todoZipper)
                        ]
                )
        )


viewDoneTodos : List (TodoEntry Done) -> Maybe EditingInfo -> Html Msg
viewDoneTodos todos editing =
    ul [ css [ textDecoration lineThrough, todoListStyle ] ]
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    let
                        currentEdit =
                            Maybe.andThen
                                (\editInfo ->
                                    if editInfo.todo == DoneZipper todoZipper then
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
                                viewEditTodo (DoneZipper todoZipper) editInfo

                            Nothing ->
                                viewTodo (DoneZipper todoZipper)
                        ]
                )
        )


viewTodo : TodoZipper -> Html Msg
viewTodo todoZipper =
    let
        todo =
            Zipper.current (zipperFromTodoZipper todoZipper)

        ( iconName, moveText ) =
            case todoZipper of
                CurrentZipper _ ->
                    ( "done", "Mark as done" )

                DoneZipper _ ->
                    ( "refresh", "Mark as to do" )
    in
    div [ css [ containerStyle ], onClick (StartEdit todoZipper) ]
        [ text (Todo.readAction todo)
        , div []
            [ button moveText iconName (Move todoZipper)
            ]
        ]


viewEditTodo : TodoZipper -> EditingInfo -> Html Msg
viewEditTodo todoZipper editInfo =
    let
        todo =
            Zipper.current (zipperFromTodoZipper todoZipper)

        ( iconName, moveText ) =
            case todoZipper of
                CurrentZipper _ ->
                    ( "done", "Mark as done" )

                DoneZipper _ ->
                    ( "refresh", "Mark as to do" )
    in
    div [ css [ containerStyle ], onClick ApplyEdit ]
        [ input [ type_ "text", onInput (UpdateEdit todoZipper editInfo.oldAction), value (Todo.readAction todo) ] []
        , div []
            [ button moveText iconName (Move todoZipper)
            , button "Undo changes" "undo" CancelEdit
            , button "Remove" "delete" (Remove todoZipper)
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
            , css [ buttonStyle, icon iconName ]
            , title description
            , value ""
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
        , padding (em 0.5)
        , hover
            [ backgroundColor (hsla 0.0 0.0 0.0 0.02)
            ]
        ]


containerStyle : Css.Style
containerStyle =
    Css.batch
        [ displayFlex
        , flexDirection row
        , justifyContent spaceBetween
        , alignItems center
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
