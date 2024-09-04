module Charter.Extras exposing (minMax, Axes(..), step, Step(..), intersect)

{-| Additional functions to help in generating the graphs.


# Definition

@docs minMax, Axes, step, Step, intersect

-}

import Tuple


y : ( Float, Float ) -> Float
y ( _, y_ ) =
    y_


x : ( Float, Float ) -> Float
x ( x_, _ ) =
    x_


{-| Describes which axes on the graph or from the data to use.
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


type Intersection
    = None
    | Left ( Float, Float )
    | Exact ( Float, Float )
    | Between ( Float, Float ) ( Float, Float )


findIntersect : Axes -> Float -> List ( Float, Float ) -> Intersection
findIntersect axes value points =
    let
        comp ( ax, ay ) =
            if axes == X then
                compare value ax

            else
                compare value ay

        find p inter =
            case ( comp p, inter ) of
                ( _, Between a b ) ->
                    Between a b

                ( _, Exact a ) ->
                    Exact a

                ( EQ, _ ) ->
                    Exact p

                ( GT, _ ) ->
                    Left p

                ( LT, Left a ) ->
                    Between a p

                ( LT, None ) ->
                    None
    in
    points
        |> List.foldl find None


{-| Attempt to find a point where a given value intersects with a dataset
along either the X or Y axis.

This function is not optimized.

-}
intersect : Axes -> Float -> List ( Float, Float ) -> Maybe ( Float, Float )
intersect axes value points =
    case ( axes, findIntersect axes value points ) of
        ( _, Exact p ) ->
            Just p

        ( X, Between ( lx, ly ) ( rx, ry ) ) ->
            let
                m =
                    (ry - ly) / (rx - lx)

                yIntercept =
                    ry - (m * rx)
            in
            Just ( value, (m * value) + yIntercept )

        ( Y, Between ( lx, ly ) ( rx, ry ) ) ->
            let
                m =
                    (ry - ly) / (rx - lx)

                xIntercept =
                    rx - (m * ry)
            in
            Just ( (m * value) + xIntercept, value )

        _ ->
            Nothing
