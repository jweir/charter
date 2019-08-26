module Charter exposing
    ( sparkline, Size, Element(..), chart, Layer(..), Box, Constraint(..)
    , Point, DataSet, LabelSet
    , Listener, listener, subscribe, selection, clicked, hover
    )

{-| This library is for generating inline graphs, called sparklines.


# Definition

@docs sparkline, Size, Element, chart, Layer, Box, Constraint


# Data types

@docs Point, DataSet, LabelSet


# Events

@docs Listener, listener, subscribe, selection, clicked, hover

-}

import Array
import Browser.Events as Browser
import Json.Decode as Json
import Svg as Svg
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
import Svg.Events as E


{-| Drawing a chart or sparkline occurs by passing elements

          -- data is tuples with (x,y) values as floats
          data = [(0,0),(5,10),(10,12)]
          data2 = [(0,-5),(7,2),(12,14)]

          sparkline
              (Size 100 10)    -- the width and height
              [ Line [] data ] -- type of element to draw with SVG attributes and the data

Multiple elements can be included to have muliple graphs. Each will have its data
scaled relatively to one another. The graphs are drawn in the order they are
given. So last graph will be drawn on top.

          sparkline
              (100,10)
              [ Line [] data
              , Line [Svg.stroke "red" ] data2
              ]

The three type of graphical elements are

  - **Area** creates a graph meant to be filled
  - **Bar** <BarWidth> Draws a bar graph. This requires defining what the width of the bar.
  - **Dot** draws a dot at each point. Set the radius of the dot by styling it `[Svg.r "3"]`
  - **Label** plots text on the graph
  - **Line** creates a line graph

There are also some options which can be applied to each graph:

  - **Domain** includes the given data into the graph's domain.
    This is useful when creating multiple graphs that need to have the same scale.
  - **ZeroLine** param which will draw a line at the 0 y axis

The charts support events as well:

  - **OnSelect** Event for when a selection is made.
  - **OnClick** Tracks click events.
  - **OnHover** Tracks the mouse moving over the chart.

**Examples**

See Example.elm for more examples, including eventing.

        -- style a ZeroLine to be very light
        sparkline
            ( 200, 5, 5, 5 )
            [ ZeroLine [ Svg.strokeWidth "0.5", Svg.stroke "rgba(0,0,0,0.3)" ]
            , Bar 20 [ ( 0, 2 ) , ( 10, 30 ) , ( 20, 10 ) ]
            ]

        -- graph the datasets to not be relative to one another.  The elements can be piped.
        sparkline
            ( 200, 10, 5, 5 )
            [ Line data
            , Line data2
            ]

-}
type Element a
    = Area (List (Svg.Attribute a)) DataSet
    | Bar (List (Svg.Attribute a)) Float DataSet
    | Dot (List (Svg.Attribute a)) DataSet
    | Label (List (Svg.Attribute a)) (LabelSet a)
    | Line (List (Svg.Attribute a)) DataSet
      -- options
    | Domain DataSet
    | Highlight (List (Svg.Attribute a)) Constraint Listener
    | ZeroLine (List (Svg.Attribute a))
      -- events
    | OnSelect Listener (Listener -> a)
    | OnClick Listener (Listener -> a)
    | OnHover Listener (Listener -> a)


{-| When highlighting a selected region the application can have the selection contrainted to just the X axis or be free.

You would most likely use OnlyX when selecting a timeseries.

-}
type Constraint
    = FreeForm
    | OnlyX


{-| Defines the size and position of chart elements
-}
type alias Box =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    }


{-| Defines the size of the chart
-}
type alias Size =
    { width : Float
    , height : Float
    }


type alias Text =
    String


{-| Tuple of (x,y) value
-}
type alias Point =
    ( Float, Float )


{-| The data to be rendered.
-}
type alias DataSet =
    List Point


{-| The data and text to use for labeling
-}
type alias LabelSet a =
    List ( Point, List (Svg.Attribute a), Text )


type alias Range =
    ( Float -> Float, Float -> Float )


type alias Domain =
    ( Point, Point )


type alias Method a =
    DataSet
    -> List (Svg.Attribute a)
    -> Scalar
    -> List (Svg a)


type alias Scalar =
    { domain : Domain

    -- range scales from the orginal data to the coordinates on the sized canvas
    , range : Range

    -- invert does the opposite of range
    , inverter : Range
    , box : Box
    }


{-| Layers group the chart elements. They have a size and x, y offsets

Layers are drawn in the order of the list. If you want something drawn over another layer, place it after that layer.

-}
type Layer a
    = Layer Box (List (Element a))


{-| Use chart to draw graphics with layers. Layers can be positioned and overlayed, allowing for charts with margins and different regions.

    chart (Size 620 120)
        [ Layer
            (Box 600 50 10 10)
            [ Line [] data0
            , ZeroLine []
            ]
        , Layer (Box 600 20 10 60)
            [ Area [ Svg.stroke "none", Svg.fill "rgb(150,150,255)" ] data1 ]
        ]

-}
chart : Size -> List (Layer a) -> Svg a
chart size layers =
    layers
        |> List.map (\(Layer box elements) -> convert box elements)
        |> frame size


{-| The entry point to create a graph. See Element.
-}
sparkline : Size -> List (Element a) -> Svg a
sparkline size elements =
    frame size [ convert (Box size.width size.height 0 0) elements ]


{-| The entry point to create a graph. See Element.
-}
convert : Box -> List (Element a) -> Svg a
convert box elements =
    let
        sets : List (LayerSet a)
        sets =
            List.map toLayerSet elements

        commands =
            sets
                |> List.filterMap
                    (\set ->
                        case set of
                            Command s ->
                                Just s

                            Event _ ->
                                Nothing
                    )

        domain_ : Domain
        domain_ =
            commands
                |> List.concatMap (\token -> [ token.data ])
                |> domain

        -- calc the range in the method, bar needs the size before calcing the range
        range_ : Range
        range_ =
            range box domain_

        scalar =
            { domain = domain_
            , range = range_
            , inverter = invert box domain_
            , box = box
            }

        collector : CommandSet a -> List (Svg a)
        collector token =
            token.method token.data token.attributes scalar

        events =
            sets
                |> List.filterMap
                    (\set ->
                        case set of
                            Event e ->
                                Just (listen box e scalar)

                            Command s ->
                                Nothing
                    )
                |> List.concatMap identity
    in
    commands
        |> List.concatMap collector
        -- add the events to the end, so it is on top of the other elements
        |> (\list -> list ++ [ eventArea scalar events ])
        |> layer box


type LayerSet a
    = Command (CommandSet a)
    | Event (EventSet a)


type alias CommandSet a =
    { method : Method a
    , data : DataSet
    , attributes : List (Svg.Attribute a)
    }


toLayerSet : Element a -> LayerSet a
toLayerSet msg =
    case msg of
        OnSelect listener_ eventMsg ->
            EventSelect listener_ eventMsg |> Event

        OnClick listener_ eventMsg ->
            EventClick listener_ eventMsg |> Event

        OnHover listener_ eventMsg ->
            EventHover listener_ eventMsg |> Event

        Bar style width data ->
            CommandSet (bar width) data style |> Command

        Dot style data ->
            CommandSet dot data style |> Command

        Line style data ->
            CommandSet line data style |> Command

        Area style data ->
            CommandSet area data style |> Command

        Domain data ->
            CommandSet noop data [] |> Command

        Label style labelSet ->
            let
                -- map out just the points to use as the underlying data
                data =
                    List.map (\( p, _, _ ) -> p) labelSet
            in
            CommandSet (label labelSet) data style |> Command

        -- options
        ZeroLine style ->
            CommandSet zeroLine [] style |> Command

        Highlight style constraint sel ->
            CommandSet (highlight style constraint sel) [] style |> Command


frame : Size -> List (Svg a) -> Svg a
frame size children =
    Svg.svg
        [ setAttr A.width size.width
        , setAttr A.height size.height
        , joinAttr A.viewBox [ 0, 0, size.width, size.height ]
        ]
        children


layer : Box -> List (Svg a) -> Svg a
layer box children =
    Svg.g
        [ A.transform ("translate(" ++ String.fromFloat box.x ++ "," ++ String.fromFloat box.y ++ ")")
        ]
        children


zeroLine : Method a
zeroLine _ attr scalar =
    let
        ( ( x1, _ ), ( x2, _ ) ) =
            scalar.domain
    in
    line
        [ ( x1, 0 ), ( x2, 0 ) ]
        attr
        scalar


noop : Method a
noop data attr _ =
    []


line : Method a
line data attr scalar =
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


area : Method a
area data attr scalar =
    let
        ( ( minx, miny ), ( maxx, maxy ) ) =
            scalar.domain

        p0 =
            ( minx, miny )

        p1 =
            ( maxx, miny )

        cappedData =
            [ p0 ] ++ data ++ [ p1 ]
    in
    line cappedData attr scalar


dot : Method a
dot data attr scalar =
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


bar : Float -> Method a
bar w data attr scalar =
    let
        ( ( x0, y0 ), ( x1, y1 ) ) =
            scalar.domain

        ( mx, my ) =
            scalar.range
    in
    data
        |> List.map
            (\( x, y ) ->
                let
                    -- this positions the bar correctly within the frame based on the width of the bar
                    -- same should happen for dots
                    p =
                        w * ((x - x0) / (x1 - x0))

                    ( y_, h ) =
                        if y < 0 then
                            ( my y - (my y - my 0), my y - my 0 )

                        else
                            ( my y, my 0 - my y )
                in
                rect
                    ([ setAttr A.x (mx x - p)
                     , setAttr A.y y_
                     , setAttr width w
                     , setAttr height h
                     ]
                        ++ attr
                    )
                    []
            )


label : LabelSet a -> Method a
label labels data styled scalar =
    let
        indexed =
            labels |> Array.fromList
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

                    Just ( p, attr, label_ ) ->
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


path : Range -> DataSet -> String
path r data =
    data
        |> scale r
        |> List.foldr collect ""



-- FIXME BUG when domain min and max are equal


domain : List DataSet -> Domain
domain dataset =
    let
        flatData =
            dataset |> List.concatMap (\s -> s)

        seed =
            flatData
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
    in
    flatData
        |> List.foldr
            (\( x, y ) ( ( xlo, ylo ), ( xhi, yhi ) ) ->
                ( ( Basics.min xlo x, Basics.min ylo y )
                , ( Basics.max xhi x, Basics.max yhi y )
                )
            )
            ( seed, seed )
        |> ensure


{-| esures the domain along y is not identical
-}
ensure : Domain -> Domain
ensure ( ( x0, y0 ), ( x1, y1 ) ) =
    if y0 == y1 then
        ( ( x0, min 0 y0 ), ( x1, y1 ) )

    else
        ( ( x0, y0 ), ( x1, y1 ) )


{-| creates x and y scale functions
-}
range : Box -> Domain -> Range
range box ( ( x0, y0 ), ( x1, y1 ) ) =
    ( \x ->
        (x - x0) * (box.width / (x1 - x0)) |> noNan
    , \y ->
        (y1 - y) * (box.height / (y1 - y0)) |> noNan
    )


{-| creates x and y scale inversion functions
-}
invert : Box -> Domain -> Range
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
    case isNaN f of
        True ->
            0

        False ->
            f


scale : Range -> DataSet -> DataSet
scale ( mx, my ) data =
    data
        |> List.map (\( x, y ) -> ( mx x, my y ))


setAttr : (String -> a) -> Float -> a
setAttr fun n =
    fun (String.fromFloat n)


joinAttr : (String -> a) -> List Float -> a
joinAttr fun n =
    List.map String.fromFloat n
        |> String.join " "
        |> fun



-- EVENTS


type EventSet a
    = EventSelect Listener (Listener -> a)
    | EventClick Listener (Listener -> a)
    | EventHover Listener (Listener -> a)


{-| A listener to maintain the state of events (selection, hover and clicks). A
listener can be shared across charts with the same scale.
-}
type Listener
    = Listener EventRecord


type alias EventRecord =
    { mouseDown : Bool
    , box : Box
    , scalar : Maybe Scalar

    -- the starting point on the page
    , offset : Maybe Point
    , start : Maybe Point

    -- the bounding box in absolute size to the SVG graph
    , current : Maybe Point

    -- last clicked position
    , clicked : Maybe Point
    , hover : Maybe Point
    }


{-| Create a new event listener.
-}
listener : Listener
listener =
    Listener (EventRecord False (Box 0 0 0 0) Nothing Nothing Nothing Nothing Nothing Nothing)


{-| When tracking `OnSelect` a subscription will be required. The mouse events are tracked outside of the chart's SVG element.

        type alias Model =
            { listener : Listener }

        type Msg
            = Select Listener

        subscriptions =
            Sub.batch [ subscribe model.listener Select ]

-}
subscribe : Listener -> (Listener -> a) -> Sub a
subscribe listener_ eventMsg =
    let
        offsetPosition : EventRecord -> (Listener -> a) -> Json.Decoder a
        offsetPosition sel msg =
            case sel.offset of
                Nothing ->
                    Json.succeed (msg (Listener sel))

                Just ( x0, y0 ) ->
                    Json.map2
                        (\x y ->
                            let
                                sel_ =
                                    { sel | current = Just ( (x |> toFloat) - x0, (y |> toFloat) - y0 ) }
                            in
                            Listener sel_ |> msg
                        )
                        (Json.field "pageX" Json.int)
                        (Json.field "pageY" Json.int)
    in
    Sub.batch <|
        case listener_ of
            Listener sel ->
                if sel.mouseDown == True then
                    [ Browser.onMouseMove (offsetPosition sel eventMsg)
                    , Browser.onMouseUp (offsetPosition { sel | clicked = Nothing, mouseDown = False } eventMsg)
                    ]

                else
                    []


{-| Clicked returns a point from a click event.
-}
clicked : Listener -> Maybe Point
clicked (Listener sel) =
    case sel.scalar of
        Nothing ->
            Nothing

        Just scalar ->
            case sel.clicked of
                Just ( x, y ) ->
                    let
                        ( ix, iy ) =
                            scalar.inverter
                    in
                    Just
                        ( ix x, iy (y - sel.box.height |> abs) )

                _ ->
                    Nothing


{-| Hover returns a point from a hover event.
-}
hover : Listener -> Maybe Point
hover (Listener sel) =
    case ( sel.scalar, sel.hover ) of
        ( Just scalar, Just ( x, y ) ) ->
            let
                ( ix, iy ) =
                    scalar.inverter
            in
            Just
                ( ix x, iy (y - sel.box.height |> abs) )

        _ ->
            Nothing


{-| Selection returns a box with the selected boundaries of the data.

Use this selection to filter the applications data into a subset.

    filter : DataSet -> Listener -> DataSet
    filter data listener =
        case selection listener of
            Nothing ->
                []

            Just ( ( x0, _ ), ( x1, _ ) ) ->
                data
                    |> List.filter (\( x, _ ) -> x >= x0 && x <= x1)

-}
selection : Listener -> Maybe ( Point, Point )
selection l =
    let
        sel =
            case l of
                Listener sel_ ->
                    sel_
    in
    case sel.scalar of
        Nothing ->
            Nothing

        Just scalar ->
            case ( sel.start, sel.current ) of
                ( Just ( dx0, dy0 ), Just ( dx1, dy1 ) ) ->
                    let
                        ( ix, iy ) =
                            scalar.inverter

                        ( bx0, by0 ) =
                            ( ix dx0, iy (dy0 - sel.box.height |> abs) )

                        ( bx1, by1 ) =
                            ( ix dx1, iy (dy1 - sel.box.height |> abs) )
                    in
                    Just
                        ( ( min bx0 bx1, min by0 by1 )
                        , ( max bx0 bx1, max by0 by1 )
                        )

                _ ->
                    Nothing


{-| TODO support mousemove when outside of the region, this will probably require a subscription
-}
listen : Box -> EventSet a -> Scalar -> List (Svg.Attribute a)
listen box eventSet scalar =
    case eventSet of
        EventSelect listener_ msg ->
            listenSelection box listener_ msg scalar

        EventClick listener_ msg ->
            listenClick box listener_ msg scalar

        EventHover listener_ msg ->
            listenHover box listener_ msg scalar


listenSelection : Box -> Listener -> (Listener -> a) -> Scalar -> List (Svg.Attribute a)
listenSelection box selected msg scalar =
    let
        offsetStart sel m =
            Json.map4
                (\oX oY x y ->
                    let
                        sel_ =
                            { sel
                                | scalar = Just scalar
                                , mouseDown = True
                                , box = box
                                , offset =
                                    Just
                                        ( ((oX - x) |> toFloat) + box.x, ((oY - y) |> toFloat) + box.y )
                                , start =
                                    Just
                                        ( (x |> toFloat) - box.x, (y |> toFloat) - box.y )
                                , current = Nothing
                            }
                    in
                    msg (Listener sel_)
                )
                (Json.field "pageX" Json.int)
                (Json.field "pageY" Json.int)
                (Json.field "offsetX" Json.int)
                (Json.field "offsetY" Json.int)
    in
    case selected of
        Listener s ->
            if s.mouseDown == True then
                []

            else
                [ E.on "mousedown" (offsetStart s msg)
                ]


listenClick : Box -> Listener -> (Listener -> a) -> Scalar -> List (Svg.Attribute a)
listenClick box selected msg scalar =
    let
        click sel m =
            Json.map2
                (\x y ->
                    let
                        sel_ =
                            { sel
                                | scalar = Just scalar
                                , mouseDown = False
                                , box = box
                                , clicked =
                                    if sel.current == Nothing then
                                        Just ( (x |> toFloat) - box.x, (y |> toFloat) - box.y )

                                    else
                                        Nothing
                            }
                    in
                    msg (Listener sel_)
                )
                (Json.field "offsetX" Json.int)
                (Json.field "offsetY" Json.int)
    in
    case selected of
        Listener s ->
            [ E.on "click" (click s msg) ]


listenHover : Box -> Listener -> (Listener -> a) -> Scalar -> List (Svg.Attribute a)
listenHover box selected msg scalar =
    let
        click sel m =
            Json.map2
                (\x y ->
                    let
                        sel_ =
                            { sel
                                | scalar = Just scalar
                                , box = box
                                , hover = Just ( (x |> toFloat) - box.x, (y |> toFloat) - box.y )
                            }
                    in
                    msg (Listener sel_)
                )
                (Json.field "offsetX" Json.int)
                (Json.field "offsetY" Json.int)
    in
    case selected of
        Listener s ->
            [ E.on "mousemove" (click s msg) ]


eventArea : Scalar -> List (Svg.Attribute a) -> Svg a
eventArea scalar events =
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            scalar.domain

        ( mx, my ) =
            scalar.range

        ( ix, iy ) =
            scalar.inverter
    in
    rect
        ([ setAttr A.x (mx x1)
         , setAttr A.y (my y2)
         , setAttr width (mx x2)
         , setAttr height (my y1)
         , fill "rgba(0,0,0,0.0)"
         ]
            ++ events
        )
        []


highlight : List (Svg.Attribute a) -> Constraint -> Listener -> Method a
highlight style constraint selected _ _ scalar =
    case selected of
        Listener sel ->
            case ( sel.start, sel.current ) of
                ( Just ( ax1, ay1 ), Just ( bx1, by1 ) ) ->
                    let
                        box =
                            case constraint of
                                FreeForm ->
                                    Box
                                        (ax1 - bx1 |> abs)
                                        (ay1 - by1 |> abs)
                                        (min ax1 bx1)
                                        (min ay1 by1)

                                OnlyX ->
                                    Box
                                        (ax1 - bx1 |> abs)
                                        scalar.box.height
                                        (min ax1 bx1)
                                        0
                    in
                    [ rect
                        ([ setAttr A.x box.x
                         , setAttr A.y box.y
                         , setAttr width box.width
                         , setAttr height box.height
                         , fill "rgba(255,0,0,0.5)"
                         ]
                            ++ style
                        )
                        []
                    ]

                _ ->
                    []
