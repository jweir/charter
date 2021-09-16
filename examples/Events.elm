module Events exposing (main)

import Browser
import Charter exposing (Box, Layer(..), Point, Size, chart, dot, labels, sparkline)
import Charter.Events as Charter
import Charter.Extras as Charter
import Html as Html
import Html.Attributes as Html
import Html.Events as Html
import Random
import Svg
import Svg.Attributes as Svg
import Time


type alias Model =
    { listener : Charter.Listener
    , constraint : Charter.Constraint
    , data : List Point
    , userData : List Point
    , selectedData : List Point
    , selectedUserData : List Point
    }


type Msg
    = Select Charter.Listener
    | SetContraint Charter.Constraint
    | AddPoint Charter.Listener


sample : List Point
sample =
    let
        seed =
            Random.initialSeed 42

        point =
            Random.pair (Random.float -100 100) (Random.float -100 100)
    in
    List.range 1 1000
        |> List.map
            (\n ->
                Random.step point (Random.initialSeed n)
                    |> Tuple.first
            )


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { listener = Charter.listener
                  , constraint = Charter.XY
                  , data = sample
                  , userData = []
                  , selectedData = []
                  , selectedUserData = []
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Charter.subscribe model.listener Select
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        filter model_ =
            { model_
                | selectedData = Charter.filter model_.data model_.constraint model.listener
                , selectedUserData = Charter.filter model_.userData model_.constraint model.listener
            }
    in
    case msg of
        AddPoint listener ->
            case Charter.clicked listener of
                Just p ->
                    ( { model | userData = p :: model.userData } |> filter, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetContraint con ->
            ( { model | constraint = con } |> filter, Cmd.none )

        Select listener ->
            ( { model | listener = listener } |> filter
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    let
        button con =
            Html.input [ Html.type_ "radio", Html.onClick (SetContraint con), Html.checked (con == model.constraint) ] []
    in
    Html.div [ Html.style "margin" "2em" ]
        [ Html.h2 []
            [ Html.text "Events and Selections"
            ]
        , Html.p []
            [ chart (Size 420 420)
                [ Layer (Box 420 420 0 0)
                    [ Charter.line [] [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( 0, 0 ) ] ]
                , Layer (Box 400 400 10 10)
                    [ Charter.highlight [ Svg.fill "rgba(0,0,0,0.1)" ] model.constraint model.listener
                    , Charter.zeroLine []
                    , dot [ Svg.r "2", Svg.fill "gray" ] model.data
                    , dot [ Svg.r "5", Svg.fill "red" ] model.userData
                    , Charter.onSelect model.listener Select
                    , Charter.onClick model.listener AddPoint
                    ]
                ]
            , chart (Size 420 420)
                [ Layer (Box 420 420 0 0)
                    [ Charter.line [] [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( 0, 0 ) ] ]
                , Layer (Box 400 400 10 10)
                    [ Charter.zeroLine []
                    , dot [ Svg.r "4" ] model.selectedData
                    , dot [ Svg.r "8", Svg.fill "red" ] model.selectedUserData
                    ]
                ]
            ]
        , Html.p []
            [ Html.text "Make a selection with the mouse then change the selection constraint to "
            , button Charter.XY
            , Html.text "X & Y"
            , Html.text " "
            , button Charter.OnlyX
            , Html.text "X"
            , Html.text " "
            , button Charter.OnlyY
            , Html.text "Y"
            ]
        , Html.p [] [ Html.text "Click anywhere in the chart to add a new point." ]
        ]
