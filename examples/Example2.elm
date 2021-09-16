module Example2 exposing (main)

import Browser
import Charter exposing (Box, Layer(..), Point, Size, bar, chart, labels, sparkline)
import Charter.Events as Charter
import Charter.Extras as Charter
import Html as Html
import Html.Attributes as HA
import Html.Events as HE
import Svg
import Svg.Attributes as Svg
import Time


type alias Model =
    { listener : Charter.Listener
    , listener2 : Charter.Listener
    , clicked : Maybe Point
    , hover : Maybe Point
    , width : Size
    }


type Size
    = Large
    | Small


type Msg
    = Select Charter.Listener
    | Select2 Charter.Listener
    | Click Charter.Listener
    | Hover Charter.Listener
    | ChangeSize Size


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { listener = Charter.listener
                  , listener2 = Charter.listener
                  , clicked = Nothing
                  , hover = Nothing
                  , width = Large
                  }
                , Cmd.none
                )
        , view = \model -> view model
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Charter.subscribe model.listener Select
        , Charter.subscribe model.listener2 Select2
        ]


filter : List Charter.Point -> Charter.Listener -> List Charter.Point
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


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.p []
            [ chart (Size 400 200)
                [ Layer (Box 400 50 0 0)
                    [ Charter.zeroLine []
                    , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                    , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                    ]
                , Layer (Box 400 50 0 50)
                    [ Charter.zeroLine []
                    , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                    , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                    ]
                , Layer (Box 400 50 0 100)
                    [ Charter.zeroLine []
                    , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                    , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                    ]
                , Layer (Box 400 50 0 150)
                    [ Charter.zeroLine []
                    , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                    , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                    ]
                ]
            ]
        ]
