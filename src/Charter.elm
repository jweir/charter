module Charter exposing
    ( chart, Size, Layer(..), Box, Element, Chart, sparkline
    , Point, Label
    , line, area, dot, bar, labels, zeroLine
    , insertDomain
    )

{-| Charter is a library and set of functions for rendering charts and graphs.
It has support for text labels events.

  - TODO Layer to layer
  - TODO chart to draw


# Definition

@docs chart, Size, Layer, Box, Element, Chart, sparkline


# Data

@docs Point, Label


# Elements

All elements in Charter are styled using Svg.Attribute.

@docs line, area, dot, bar, labels, zeroLine


# Options

@docs insertDomain

-}

import Array
import Charter.Extras exposing (Axes(..), minMax)
import Charter.Internals exposing (..)
import Svg
    exposing
        ( Svg
        , circle
        , rect
        )
import Svg.Attributes as A
    exposing
        ( cx
        , cy
        , d
        , fill
        , height
        , r
        , stroke
        , strokeWidth
        , width
        , x
        , y
        )
import Svg.Lazy as Svg


{-| Defines both the size and positional offset for a Layer.
-}
type alias Box =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    }


{-| -}
type alias Size =
    { width : Float
    , height : Float
    }


{-| All data in Charter is made up of 2D Points (x,y).
-}
type alias Point =
    ( Float, Float )


{-| The data and text to use for labeling
-}
type alias Label msg =
    --FIXE me maybe this and layer should become funcs?
    ( List (Svg.Attribute msg), Point, String )


{-| The SVG representation of the chart.
-}
type alias Chart msg =
    Svg msg


{-| A Chart can have multiple Layers. Each Layer has its own Box which is used
to position the layer within the Chart.

Each Layer has its own data domain. This means each Layer will have its own
scale.

Layers can be used for things like creating X or Y Axis, charts that contain
multiple graphs and more.

Here is an example with a simple X Axis.

    chart (Size 400 220)
        [ Layer (Box 400 200 0 0) [ bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ] ]
        , Layer (Box 400 200 0 0) [ labels [] [ ( [], ( 0, 0 ), "0" ), ( [], ( 5, 0 ), "5" ) ] ]
        ]

-}
type Layer msg
    = Layer Box (List (Element msg))


{-| Use chart to draw graphics with layers. Layers can be positioned and overlayed, allowing for charts with margins and different regions.

    chart (Size 620 120)
        [ Layer
            (Box 600 50 10 10)
            [ line [] [ ( 0, 0 ), ( 10, 5 ) ]
            , zeroLine []
            ]
        , Layer (Box 600 20 10 60)
            [ area [ Svg.stroke "none", Svg.fill "rgb(150,150,255)" ] [ ( 1, 4 ), ( 10, 2 ) ] ]
        ]

-}
chart : Size -> List (Layer msg) -> Chart msg
chart size layers =
    -- FIXME rename view? or render? or print?
    layers
        |> List.map (\(Layer box elements) -> Svg.lazy2 convert box elements)
        |> frame size


{-| Draws a simple chart without layers or padding.

          -- data is tuples with (x,y) values as floats
          data = [(0,0),(5,10),(10,12)]

          sparkline
              (Size 100 10)    -- the width and height
              [ line [] data ] -- type of element to draw with SVG attributes and the data

Multiple elements can be included to have muliple graphs. Each will have its data
scaled relatively to one another. The graphs are drawn in the order they are
given. So last graph will be drawn on top.

          sparkline
              (100,10)
              [ line [] data
              , line [Svg.stroke "red" ] [(0,10), (10,0)]
              ]

-}
sparkline : Size -> List (Element msg) -> Chart msg
sparkline size elements =
    frame size [ Svg.lazy2 convert (Box size.width size.height 0 0) elements ]


convert : Box -> List (Element msg) -> Chart msg
convert box sets =
    let
        ( cmdEls, eventELs ) =
            sets
                |> List.foldr
                    (\set ( cmds, evnts ) ->
                        case set of
                            Command s ->
                                ( s :: cmds, evnts )

                            Event e ->
                                ( cmds, e :: evnts )
                    )
                    ( [], [] )

        scalar =
            createScalar box
                (cmdEls |> List.concatMap (\token -> [ token.data ]))

        collector : CommandSet msg -> List (Chart msg)
        collector token =
            token.method token.data token.attributes scalar

        events =
            eventConvert eventELs box scalar
    in
    cmdEls
        |> List.concatMap collector
        -- add the events to the end, so it is on top of the other elements
        |> (\list -> list ++ [ eventArea scalar events ])
        |> xlayer box


createScalar : Box -> List (List Point) -> Scalar
createScalar box points =
    let
        domain_ =
            createDomain points
    in
    { domain = domain_
    , range = range box domain_
    , inverter = invert box domain_
    , box = box
    }


{-| The graphical building block for the chart.
-}
type alias Element msg =
    Charter.Internals.Element msg



-- DRAWING


{-| Draws an open line element.
-}
line : List (Svg.Attribute msg) -> List Point -> Element msg
line attr data =
    CommandSet lineCmd data attr |> Command


{-| Draws a closed area. The smallest Y value will be used to define the floor of the area.
-}
area : List (Svg.Attribute msg) -> List Point -> Element msg
area attr data =
    CommandSet areaCmd data attr |> Command


{-| Dot draws a dot at each point. Set the radius of the dot by styling it `[Svg.r "3"]`
-}
dot : List (Svg.Attribute msg) -> List Point -> Element msg
dot attr data =
    CommandSet dotCmd data attr |> Command


{-| Bar draws a bar graph of a given width. All bars start at 0. They can be positive or negative.

This is just a bar and does not support stacking.

-}
bar : List (Svg.Attribute msg) -> Float -> List Point -> Element msg
bar attr width data =
    let
        -- bars all start at 0
        floor =
            ( minMax X data |> Tuple.first, 0 )
    in
    CommandSet (barCmd width) (floor :: data) attr |> Command


barCmd : Float -> Method a
barCmd w data attr scalar =
    let
        ( ( x0, _ ), ( x1, _ ) ) =
            scalar.domain

        ( mx, my ) =
            scalar.range
    in
    data
        |> List.map
            (\( x, y ) ->
                let
                    -- this positions the bar correctly within the frame based
                    -- on the width of the bar
                    p =
                        w * ((x - x0) / (x1 - x0))

                    ( s0, sy ) =
                        ( my 0, my y )

                    ( y_, h ) =
                        if y < 0 then
                            ( sy - (sy - s0), sy - s0 )

                        else
                            ( sy, s0 - sy )
                in
                rect
                    (setAttr A.x (mx x - p)
                        :: setAttr A.y y_
                        :: setAttr width w
                        :: setAttr height h
                        :: attr
                    )
                    []
            )


{-| Draw plotted text.
-}
labels : List (Svg.Attribute msg) -> List (Label msg) -> Element msg
labels attr labelSet =
    let
        -- map out just the points to use as the underlying data
        data =
            List.map (\( _, p, _ ) -> p) labelSet
    in
    CommandSet (labelCmd labelSet) data attr |> Command



-- OPTIONS


{-| Use insertDomain to include data without rendering it.

For example if you wanted to display 12 months of annual data but only had data for March and April
you could plot it like:

    Layer []
        [ line [] [ ( 3, 2 ), ( 4, 10 ) ]

        -- ensure the first and last month have a position
        , insertDomain [ ( 1, 0 ), ( 12, 0 ) ]
        ]

This would plot the first 4 and leave room for the next 8.

Domain can be called multiple times and with multiple points.

-}
insertDomain : List Point -> Element msg
insertDomain data =
    -- TODO rename addToDomain? and create a forceDomain
    CommandSet noop data [] |> Command


{-| Draw a styled line on the 0 Y axis.
-}
zeroLine : List (Svg.Attribute msg) -> Element msg
zeroLine attr =
    CommandSet zeroLineCmd [] attr |> Command



-- MISC


frame : Size -> List (Chart msg) -> Chart msg
frame size children =
    Svg.svg
        [ setAttr A.width size.width
        , setAttr A.height size.height
        , joinAttr A.viewBox [ 0, 0, size.width, size.height ]
        ]
        children


xlayer : Box -> List (Chart msg) -> Chart msg
xlayer box children =
    Svg.lazy3 lazyLayer box.x box.y children


lazyLayer : Float -> Float -> List (Chart msg) -> Chart msg
lazyLayer x y children =
    Svg.g
        [ A.transform ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")
        ]
        children


zeroLineCmd : Method a
zeroLineCmd _ attr scalar =
    let
        ( ( x1, _ ), ( x2, _ ) ) =
            scalar.domain
    in
    lineCmd
        [ ( x1, 0 ), ( x2, 0 ) ]
        attr
        scalar


noop : Method a
noop _ _ _ =
    []


lineCmd : Method a
lineCmd data attr scalar =
    [ Svg.path
        ([ fill "none"
         , stroke "#000"
         , setAttr strokeWidth 1
         , d (path scalar.range data)
         ]
            ++ attr
        )
        []
    ]


areaCmd : Method a
areaCmd data attr scalar =
    let
        ( ( _, miny ), _ ) =
            scalar.domain

        ( minx, maxx ) =
            Charter.Extras.minMax Charter.Extras.X data

        ( dyMin, _ ) =
            Charter.Extras.minMax Charter.Extras.Y data

        y =
            max (min dyMin 0) miny

        p0 =
            ( minx, y )

        p1 =
            ( maxx, y )

        cappedData =
            p0 :: data ++ [ p1 ]
    in
    lineCmd cappedData attr scalar


dotCmd : Method a
dotCmd data attr scalar =
    data
        |> scale scalar.range
        |> List.map
            (\( x, y ) ->
                circle
                    ([ setAttr cx x
                     , setAttr cy y
                     , setAttr r 2
                     ]
                        ++ attr
                    )
                    []
            )


labelCmd : List (Label a) -> Method a
labelCmd labelList data styled scalar =
    let
        indexed =
            labelList |> Array.fromList
    in
    data
        |> scale scalar.range
        |> Array.fromList
        |> Array.toIndexedList
        |> List.concatMap
            (\( index, ( x, y ) ) ->
                case Array.get index indexed of
                    Nothing ->
                        []

                    Just ( attr, _, label_ ) ->
                        [ Svg.text_
                            ([ setAttr A.x x
                             , setAttr A.y y
                             ]
                                ++ styled
                                ++ attr
                            )
                            [ Svg.text label_ ]
                        ]
            )


collect : Point -> String -> String
collect ( x, y ) pathStr =
    let
        command =
            if pathStr == "" then
                "M"

            else
                "L"
    in
    pathStr
        ++ command
        ++ String.fromFloat x
        ++ " "
        ++ String.fromFloat y


path : Scale -> List Point -> String
path r data =
    data
        |> scale r
        |> List.foldr collect ""


createDomain : List (List Point) -> Domain
createDomain points =
    let
        allPoints =
            points |> List.concatMap identity

        seed =
            allPoints
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
    in
    allPoints
        |> List.foldr
            (\( x, y ) ( ( xlo, ylo ), ( xhi, yhi ) ) ->
                ( ( Basics.min xlo x, Basics.min ylo y )
                , ( Basics.max xhi x, Basics.max yhi y )
                )
            )
            ( seed, seed )
        |> ensure


{-| Ensure the domain along y is not identical.
If it is then insert a 0.
-}
ensure : Domain -> Domain
ensure ( ( x0, y0 ), ( x1, y1 ) ) =
    if y0 == y1 then
        ( ( x0, min 0 y0 ), ( x1, y1 ) )

    else
        ( ( x0, y0 ), ( x1, y1 ) )


{-| creates x and y scale functions
-}
range : Box -> Domain -> Scale
range box ( ( x0, y0 ), ( x1, y1 ) ) =
    ( \x ->
        (x - x0) * (box.width / (x1 - x0)) |> noNan
    , \y ->
        (y1 - y) * (box.height / (y1 - y0)) |> noNan
    )


{-| creates x and y scale inversion functions
-}
invert : Box -> Domain -> Scale
invert box ( ( x0, y0 ), ( x1, y1 ) ) =
    ( \x ->
        (x / box.width) * (x1 - x0) + x0 |> noNan
    , \y ->
        (y / box.height) * (y1 - y0) + y0 |> noNan
    )


{-| remove NaN's with a 0
-}
noNan : Float -> Float
noNan f =
    if isNaN f then
        0

    else
        f


scale : Scale -> List Point -> List Point
scale ( mx, my ) data =
    data
        |> List.map (\( x, y ) -> ( mx x, my y ))


joinAttr : (String -> a) -> List Float -> a
joinAttr fun n =
    List.map String.fromFloat n
        |> String.join " "
        |> fun
