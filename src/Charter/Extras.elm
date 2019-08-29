module Charter.Extras exposing (minMax, Axes(..), step, Step(..))

{-| Additional functions to help in generating the graphs.


# Definition

@docs minMax, Axes, step, Step

-}

import Tuple


y : ( Float, Float ) -> Float
y ( _, y_ ) =
    y_


x : ( Float, Float ) -> Float
x ( x_, _ ) =
    x_


{-| Descibes which axes on the graph or from the data to use.
-}
type Axes
    = X
    | Y


{-| Returns the min and max value for the given Axis.
An empty list will return (0,0)
-}
minMax : Axes -> List ( Float, Float ) -> ( Float, Float )
minMax axes data =
    let
        acc =
            case axes of
                X ->
                    x

                Y ->
                    y
    in
    data
        |> List.map acc
        |> (\list -> ( List.minimum list, List.maximum list ))
        |> (\( min, max ) -> ( Maybe.withDefault 0 min, Maybe.withDefault 0 max ))


{-| Defines where the step will occur
-}
type Step
    = Before
    | After


{-| step modifies the input data so it will be drawn with a step on the Y axis.
-}
step : Step -> List ( Float, Float ) -> List ( Float, Float )
step msg points =
    let
        stepFn : Maybe ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float )
        stepFn prev current =
            case prev of
                Nothing ->
                    [ current ]

                Just ( px, py ) ->
                    case msg of
                        Before ->
                            [ ( px, y current ), current ]

                        After ->
                            [ ( x current, py ), current ]
    in
    points
        |> List.foldl
            (\current ( prev, steppedPoints ) ->
                ( Just current
                , steppedPoints ++ stepFn prev current
                )
            )
            ( Nothing, [] )
        |> Tuple.second
