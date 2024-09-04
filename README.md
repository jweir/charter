# Charter

A package for quickly line charts and a few other charts.
Supports eventing for selecting data within a chart.

See `examples/Example.elm` for more detailed example with events.

## Getting Started

```
elm install jweir/charter
```

The below code will render just a line graph and a Y axis.

```
import Charter
import Svg.Attributes as Svg


points : Charter.DataSet
points =
    [ ( 0, 20 ), ( 1, 50 ), ( 2, 56 ), ( 3, 55 ), ( 4, 45 ), ( 5, 44 ), ( 6, 56 ) ]


main =
    Charter.chart
        -- The width and height of the created SVG element
        { width = 330, height = 70 }
        -- The chart then takes a list of `Layer` elements
        ([ Charter.Layer
            {- Each layer has its own width, height
               and x/y position. Here the layer is moved down
               and over 10 pixels to give a little padding.
            -}
            { width = 300, height = 50, x = 10, y = 10 }
            {- The layer then takes a list of commands to
               render data.  Here we draw a line and a red
               dot for each point.
            -}
            [ Charter.line [] points
            , Charter.dot [ Svg.fill "red" ] points
            ]

         -- This layer will represent a Y axis.
         , Charter.Layer
            {- Note this layer is a bit wider the other layer
               to give room for the labels.
            -}
            { width = 310, height = 50, x = 10, y = 10 }
            {- This list of values will be used for the ticks.
               NOTE the min and max values are the
               same as the point data used to draw the line.
               This defines the domain, see below later.
            -}
            ([ 0, 28, 56 ]
                |> List.concatMap
                    (\y ->
                        [ Charter.line
                            [ Svg.stroke "lightgray" ]
                            {- Note that for this Layer x has
                               has a range of just 0 to 1.
                               0 will be left most, 1 rightmost.
                            -}
                            [ ( 0, y ), ( 1, y ) ]

                        {- The label takes a list of Points,
                           list of Svg.Atributes, and a String.
                        -}
                        , Charter.label
                            []
                            [ ( ( 1, y )
                              , [ Svg.fontSize "10px" ]
                              , String.fromFloat y
                              )
                            ]
                        ]
                    )
            )
         ]
            {- Layers are rendered in the order received:
               first bottom, last top. We want the Y Axis on
               the bottom, so this list is reversed.
            -}
            |> List.reverse
        )
```

## TODO

- Layers and Domains
- Interaction
