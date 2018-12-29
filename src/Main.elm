module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Todo exposing (Todo)
import Url exposing (Url)
import Utils.NonEmptyString as NonEmptyString


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
    , breakdown : Breakdown.Model
    , newTodoInput : String
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , breakdown =
            Breakdown.empty
                |> Breakdown.put (Todo.from (NonEmptyString.build 'H' "ello"))
                |> Breakdown.insert (Todo.from (NonEmptyString.build 't' "here"))
                |> (\( id, model ) ->
                        Breakdown.moveToDone id model
                   )
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Breakdown.empty
      , newTodoInput = ""
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | UpdateNewTodoInput String
    | AddNewTodo


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

                        newBreakdown =
                            Breakdown.put todo model.breakdown
                    in
                    ( { model | breakdown = newBreakdown, newTodoInput = "" }, Cmd.none )

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
        [ newTodoInput model.newTodoInput
        , viewCurrentTodos (Breakdown.currentTodos model.breakdown)
        , viewDoneTodos (Breakdown.doneTodos model.breakdown)
        ]
    }


newTodoInput : String -> Html Msg
newTodoInput currentNewTodoInput =
    Html.form [ onSubmit AddNewTodo ]
        [ input [ type_ "text", onInput UpdateNewTodoInput, value currentNewTodoInput ] []
        , input [ type_ "submit", value "+" ] []
        ]


type BreakdownId
    = CurrentId (Breakdown.Id Breakdown.Current)
    | DoneId (Breakdown.Id Breakdown.Done)


viewCurrentTodos : List ( Breakdown.Id Breakdown.Current, Todo ) -> Html Msg
viewCurrentTodos todos =
    ul []
        (todos
            |> List.map
                (\( id, todo ) ->
                    li [] [ viewTodo (CurrentId id) todo ]
                )
        )


viewDoneTodos : List ( Breakdown.Id Breakdown.Done, Todo ) -> Html Msg
viewDoneTodos todos =
    ul []
        (todos
            |> List.map
                (\( id, todo ) ->
                    li [] [ viewTodo (DoneId id) todo ]
                )
        )


viewTodo : BreakdownId -> Todo -> Html Msg
viewTodo id todo =
    text (Todo.action todo)
