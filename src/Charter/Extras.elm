module Charter.Extras exposing
    ( Axes(..)
    , minMax
    )

{-| Additional functions to help in generating the graphs.


# Definition

@docs Axes


# Functions

@docs minMax

-}


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
                    Tuple.first

                Y ->
                    Tuple.second
    in
    data
        |> List.map acc
        |> (\list -> ( List.minimum list, List.maximum list ))
        |> (\( min, max ) -> ( Maybe.withDefault 0 min, Maybe.withDefault 0 max ))


{-| returns the min and max Point on either the X or Y axes - not used currently
-}
extent : Axes -> List ( Float, Float ) -> List ( Float, Float )
extent axes data =
    let
        getter =
            case axes of
                X ->
                    x

                Y ->
                    y
    in
    case List.head data of
        Nothing ->
            []

        Just h ->
            let
                ( minv, maxv ) =
                    List.foldr
                        (\point ( min, max ) ->
                            ( if getter point < getter min then
                                point

                              else
                                min
                            , if getter point > getter max then
                                point

                              else
                                max
                            )
                        )
                        ( h, h )
                        data
            in
            [ minv, maxv ]
