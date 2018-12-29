module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import List.Zipper as Zipper exposing (Zipper)
import Todo exposing (Todo)
import Url exposing (Url)
import Utils.NonEmptyString as NonEmptyString
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
    , currentTodos : TodoList Current
    , doneTodos : TodoList Done
    }


type TodoList a
    = TodoList (List Todo)


type TodoZipper
    = CurrentZipper (Zipper Todo)
    | DoneZipper (Zipper Todo)


zipperFromTodoZipper : TodoZipper -> Zipper Todo
zipperFromTodoZipper todoZipper =
    case todoZipper of
        CurrentZipper zipper ->
            zipper

        DoneZipper zipper ->
            zipper


type Current
    = Current


type Done
    = Done


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , newTodoInput = ""
      , currentTodos =
            TodoList
                [ Todo.from (NonEmptyString.build 'H' "ello")
                , Todo.from (NonEmptyString.build 't' "here")
                ]
      , doneTodos = TodoList [ Todo.from (NonEmptyString.build 'H' "ow's it going?") ]
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo
    | Remove TodoZipper


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
                            case model.currentTodos of
                                TodoList list ->
                                    TodoList (list ++ [ todo ])
                    in
                    ( { model
                        | newTodoInput = ""
                        , currentTodos = newCurrentTodos
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Remove todoZipper ->
            case todoZipper of
                CurrentZipper zipper ->
                    ( { model | currentTodos = TodoList (Zipper.remove zipper) }, Cmd.none )

                DoneZipper zipper ->
                    ( { model | doneTodos = TodoList (Zipper.remove zipper) }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Breakdown"
    , body =
        [ newTodoInput model.newTodoInput
        , viewCurrentTodos model.currentTodos
        , viewDoneTodos model.doneTodos
        ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentNewTodoInput =
    Html.form [ onSubmit AddNewTodo ]
        [ input [ type_ "text", onInput UpdateNewTodoInput, value currentNewTodoInput ] []
        , input [ type_ "submit", value "+" ] []
        ]


viewCurrentTodos : TodoList Current -> Html Msg
viewCurrentTodos (TodoList todos) =
    ul []
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    li [] [ viewTodo (CurrentZipper todoZipper) ]
                )
        )


viewDoneTodos : TodoList Done -> Html Msg
viewDoneTodos (TodoList todos) =
    ul []
        (todos
            |> Zipper.focusMap
                (\todoZipper ->
                    li [] [ viewTodo (DoneZipper todoZipper) ]
                )
        )


viewTodo : TodoZipper -> Html Msg
viewTodo todoZipper =
    let
        todo =
            Zipper.current (zipperFromTodoZipper todoZipper)
    in
    div []
        [ text (Todo.action todo)
        , button [ onClick (Remove todoZipper) ] [ text "Remove" ]
        ]
