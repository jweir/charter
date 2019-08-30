module Charter exposing
    ( sparkline, Size, Element, chart, Layer(..), Box
    , Point, DataSet, LabelSet
    , line, area, dot, bar, label
    , domain, zeroLine, highlight, Constraint(..)
    , Listener, listener, subscribe, onSelect, onClick, onHover
    , selection, clicked, hover
    )

{-| This library is for generating inline graphs, called sparklines.


# Definition

@docs sparkline, Size, Element, chart, Layer, Box


# Data

@docs Point, DataSet, LabelSet


# Drawing

@docs line, area, dot, bar, label


# Options

@docs domain, zeroLine, highlight, Constraint


# Events

    type alias Model =
        { listener : Charter.Listener
        }

    type Msg
        = Select Charter.Listener
        | Click Charter.Listener
        | Hover Charter.Listener

    chart (Size 620 120)
        [ Layer
            (Box 600 70 10 10)
            [ highlight [ Svg.fill "rgba(255,255,0,0.4)" ]
                OnlyX
                model.listener
            ]
        , Layer
            (Box 600 50 10 10)
            [ Charter.onClick model.listener Click
            , Charter.onSelect model.listener Select
            , Charter.onHover model.listener Hover
            , Charter.line [ Svg.stroke "red" ] data0
            ]
        ]

@docs Listener, listener, subscribe, onSelect, onClick, onHover


# Event Data

Use the below functions to extract the data from events. The Point values
returned are scaled to the input data, not the mouse events.

@docs selection, clicked, hover

-}

import Array
import Browser.Events as Browser
import Charter.Extras
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
import Svg.Lazy


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
    List ( Point, List (Svg.Attribute a), String )


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
            [ line [] data0
            , zeroLine []
            ]
        , Layer (Box 600 20 10 60)
            [ area [ Svg.stroke "none", Svg.fill "rgb(150,150,255)" ] data1 ]
        ]

-}
chart : Size -> List (Layer a) -> Svg a
chart size layers =
    layers
        |> List.map (\(Layer box elements) -> convert box elements)
        |> frame size


{-| Draws a simple chart.

          -- data is tuples with (x,y) values as floats
          data = [(0,0),(5,10),(10,12)]
          data2 = [(0,-5),(7,2),(12,14)]

          sparkline
              (Size 100 10)    -- the width and height
              [ line [] data ] -- type of element to draw with SVG attributes and the data

Multiple elements can be included to have muliple graphs. Each will have its data
scaled relatively to one another. The graphs are drawn in the order they are
given. So last graph will be drawn on top.

          sparkline
              (100,10)
              [ line [] data
              , line [Svg.stroke "red" ] data2
              ]

-}
sparkline : Size -> List (Element a) -> Svg a
sparkline size elements =
    frame size [ convert (Box size.width size.height 0 0) elements ]


convert : Box -> List (Element a) -> Svg a
convert box sets =
    let
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
                |> domainCmd

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


{-| -}
type Element a
    = Command (CommandSet a)
    | Event (EventSet a)


type alias CommandSet a =
    { method : Method a
    , data : DataSet
    , attributes : List (Svg.Attribute a)
    }



-- DRAWING


{-| Line creates a line chart
-}
line : List (Svg.Attribute a) -> DataSet -> Element a
line attr data =
    CommandSet lineCmd data attr |> Command


{-| Area creates a graph meant to be filled
-}
area : List (Svg.Attribute a) -> DataSet -> Element a
area attr data =
    CommandSet areaCmd data attr |> Command


{-| Dot draws a dot at each point. Set the radius of the dot by styling it `[Svg.r "3"]`
-}
dot : List (Svg.Attribute a) -> DataSet -> Element a
dot attr data =
    CommandSet dotCmd data attr |> Command


{-| Bar draws a bar graph of a given width.
-}
bar : List (Svg.Attribute a) -> Float -> DataSet -> Element a
bar attr width data =
    CommandSet (barCmd width) data attr |> Command


{-| Label plots text on the graph
-}
label : List (Svg.Attribute a) -> LabelSet a -> Element a
label attr labelSet =
    let
        -- map out just the points to use as the underlying data
        data =
            List.map (\( p, _, _ ) -> p) labelSet
    in
    CommandSet (labelCmd labelSet) data attr |> Command



-- OPTIONS


{-| Domain includes the given data into the graph's domain.
This is useful when creating multiple graphs that need to have the same scale.
-}
domain : DataSet -> Element a
domain data =
    CommandSet noop data [] |> Command


{-| Highlight is used to draw a region that has been selected. See `onSelect`
-}
highlight : List (Svg.Attribute a) -> Constraint -> Listener -> Element a
highlight attr con l =
    CommandSet (highlightCmd attr con l) [] attr |> Command


{-| ZeroLine will draw a line at the 0 y axis
-}
zeroLine : List (Svg.Attribute a) -> Element a
zeroLine attr =
    CommandSet zeroLineCmd [] attr |> Command



-- EVENTS


{-| onSelect event for when a selection is made.
-}
onSelect : Listener -> (Listener -> a) -> Element a
onSelect l msg =
    EventSelect l msg |> Event


{-| onHover tracks the mouse moving over the chart.
-}
onHover : Listener -> (Listener -> a) -> Element a
onHover l msg =
    EventHover l msg |> Event


{-| onClick tracks click events.
-}
onClick : Listener -> (Listener -> a) -> Element a
onClick l msg =
    EventClick l msg |> Event



-- MISC


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
noop data attr _ =
    []


lineLazy : DataSet -> List (Svg.Attribute a) -> Scalar -> Svg.Svg a
lineLazy data attr scalar =
    Svg.path
        ([ fill "none"
         , stroke "#000"
         , setAttr strokeWidth 1
         , d (path scalar.range data)
         ]
            ++ attr
        )
        []


lineCmd : Method a
lineCmd data attr scalar =
    [ lineLazy data attr scalar ]


areaCmd : Method a
areaCmd data attr scalar =
    let
        ( ( _, miny ), ( _, maxy ) ) =
            scalar.domain

        ( minx, maxx ) =
            Charter.Extras.minMax Charter.Extras.X data

        ( dyMin, dyMax ) =
            Charter.Extras.minMax Charter.Extras.Y data

        y =
            max (min dyMin 0) miny

        p0 =
            ( minx, y )

        p1 =
            ( maxx, y )

        cappedData =
            [ p0 ] ++ data ++ [ p1 ]
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


barCmd : Float -> Method a
barCmd w data attr scalar =
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


labelCmd : LabelSet a -> Method a
labelCmd labels data styled scalar =
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


domainCmd : List DataSet -> Domain
domainCmd dataset =
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
    { mouse : Mouse
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
    Listener
        { mouse = MouseInactive
        , box = Box 0 0 0 0
        , scalar = Nothing

        -- the starting point on the page
        , offset = Nothing
        , start = Nothing

        -- the bounding box in absolute size to the SVG graph
        , current = Nothing

        -- last clicked position
        , clicked = Nothing
        , hover = Nothing
        }


type Mouse
    = MouseDown
    | MouseDragging
    | MouseInactive


{-| When tracking `onSelect` a subscription will be required. The mouse events are tracked outside of the chart's SVG element.

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
                case sel.mouse of
                    MouseInactive ->
                        []

                    MouseDown ->
                        [ Browser.onMouseMove (offsetPosition { sel | mouse = MouseDragging } eventMsg)
                        , Browser.onMouseUp (Json.succeed ({ sel | clicked = Nothing, mouse = MouseInactive } |> Listener |> eventMsg))
                        ]

                    MouseDragging ->
                        [ Browser.onMouseMove (offsetPosition sel eventMsg)
                        , Browser.onMouseUp (offsetPosition { sel | clicked = Nothing, mouse = MouseInactive } eventMsg)
                        ]


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
                                , mouse = MouseDown
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
            case s.mouse of
                MouseInactive ->
                    [ E.on "mousedown" (offsetStart s msg) ]

                MouseDown ->
                    []

                MouseDragging ->
                    []


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
                                , mouse = MouseInactive
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


highlightCmd : List (Svg.Attribute a) -> Constraint -> Listener -> Method a
highlightCmd style constraint selected _ _ scalar =
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
