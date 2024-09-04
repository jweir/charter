module Test exposing (main)

import Browser
import Charter exposing (Box, layer, Point, Size, chart, sparkline)
import Charter.Extras as Charter
import Html as Html
import Html.Attributes as HA
import Svg
import Svg.Attributes as Svg
import Time


type alias Model =
    {}


type Msg
    = Noop


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( {}
                , Cmd.none
                )
        , view = \model -> view model
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


filter : Charter.DataSet -> Charter.Listener -> Charter.DataSet
filter data listener =
    case Charter.selection listener of
        Nothing ->
            []

        Just ( ( x0, _ ), ( x1, _ ) ) ->
            data
                |> List.filter (\( x, _ ) -> x >= x0 && x <= x1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )

c = "rgba(100, 0, 0, 0.8)"
c1 = "rgba(200, 0, 0, 0.8)"
c2 = "rgba(100, 150, 0, 0.8)"

view : Model -> Html.Html Msg
view model =
    Html.div []
        [ chart (Size 620 120)
            [ layer
                (Box 600 100 10 10)
                [ Charter.area [ Svg.fill "#EEE", Svg.stroke "none" ] data0
                , Charter.area [ Svg.fill "#CCC", Svg.stroke "none" ] data1
                , Charter.area [ Svg.fill "rgba(1,1,0.2,0.2)", Svg.stroke "none" ] data2
                , Charter.zeroLine []
                ]
            ]
        , chart (Size 620 120)
            [ layer
                (Box 600 100 10 10)
                [ Charter.stack [
                  ([ Svg.fill c, Svg.stroke "none" ], [(0,0), (10,0)])
                  , ([ Svg.fill c, Svg.stroke "none" ], [(0,2),(5,4), (10,2)])
                  ,([ Svg.fill c1, Svg.stroke "none" ], [(0,2), (6, 5), (10,2)])
                  ,([ Svg.fill c2, Svg.stroke "none" ], [(0,2), (10,2)])
                  ,([ Svg.fill c, Svg.stroke "none" ], [(5,12), (10,2)])
                  ]
                ]
            ]
        ]


data0 =
    [ ( 1, 2 ), ( 2, 4 ), ( 3, 3 ), ( 4, 6 ) ]


data1 =
    [ ( 0, -2 ), ( 1, -1 ), ( 2, 0 ), ( 3, 2 ) ]


data2 =
    [ ( 0, 0 ), ( 1, -1 ), ( 2, -2 ), ( 3, 0 ), ( 4, 1 ) ]
