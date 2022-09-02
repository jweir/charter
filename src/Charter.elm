module Charter exposing
    ( sparkline, Size, Element, layer, chart, Layer, Box
    , Point, DataSet, LabelSet
    , line, area, dot, bar, label
    , zeroLine, highlight, Constraint(..), extents
    , Domain(..), getDomain, include
    , Listener, listener, subscribe, onSelect, onClick, onHover
    , selection, clicked, hover, active
    )

{-| This library is for generating inline graphs, called sparklines.


# Definition

@docs sparkline, Size, Element, layer, chart, Layer, Box


# Data

@docs Point, DataSet, LabelSet


# Drawing

@docs line, area, dot, bar, label


# Options

@docs zeroLine, highlight, Constraint, extents


# Domain

@docs Domain, getDomain, include


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

@docs selection, clicked, hover, active

-}

import Array
import Browser.Events as Browser
import Charter.Extras
import Json.Decode as Json
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
import Svg.Events as E
import Svg.Lazy as Svg


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


type alias Method a =
    DataSet
    -> List (Svg.Attribute a)
    -> Transformer
    -> List (Svg a)


type alias Transformer =
    { domain : Domain

    -- range scales from the orginal data to the coordinates on the sized canvas
    , scale : Range

    -- invert does the opposite of range
    , invert : Range
    , box : Box
    }


{-| Layer type
-}
type Layer a
    = Layer
        { box : Box
        , commands : List (CommandSet a)
        , events : List (EventSet a)
        , domain : Domain
        , transformer : Transformer
        }


{-| Layers group the chart elements. They have a size and x, y offsets

Layers are drawn in the order of the list. If you want something drawn over another layer, place it after that layer.

-}
layer : Box -> List (Element a) -> Layer a
layer box elements =
    let
        func set ( coms, evts, inc ) =
            case set of
                Node a b ->
                    func a ( coms, evts, inc )
                        |> func b

                DataElement s ->
                    ( coms ++ [ s ], evts, inc )

                IncludedData rec ->
                    ( coms, evts, rec :: inc )

                Event e ->
                    ( coms, evts ++ [ e ], inc )

        ( commands, events, included ) =
            elements
                |> List.foldl func ( [], [], [] )

        domain_ : Domain
        domain_ =
            commands
                |> List.concatMap .data
                |> findDomain (List.concatMap identity included)

        transformer : Transformer
        transformer =
            { domain = domain_
            , scale = range box domain_
            , invert = invert box domain_
            , box = box
            }
    in
    Layer
        { box = box
        , commands = commands
        , events = events
        , domain = domain_
        , transformer = transformer
        }


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
        |> List.map (Svg.lazy convert)
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
    frame size [ Svg.lazy convert (layer { width = size.width, height = size.height, x = 0, y = 0 } elements) ]


convert : Layer a -> Svg a
convert (Layer rec) =
    let
        eval : CommandSet a -> List (Svg a)
        eval token =
            token.method token.data token.attributes rec.transformer

        events =
            rec.events
                |> List.filterMap
                    (\event ->
                        case event of
                            EventSelect (Listener listener_) msg ->
                                case listener_.mouse of
                                    MouseInactive ->
                                        E.on "mousedown"
                                            (decodeSelection
                                                (Listener listener_ |> rescaleListener rec.transformer)
                                                rec.box
                                                rec.transformer
                                                msg
                                            )
                                            |> Just

                                    MouseDown ->
                                        Nothing

                                    MouseDragging ->
                                        Nothing

                            EventClick listener_ msg ->
                                E.on "click"
                                    (decodeClick
                                        (listener_ |> rescaleListener rec.transformer)
                                        rec.box
                                        rec.transformer
                                        msg
                                    )
                                    |> Just

                            EventHover listener_ msg ->
                                E.on "mousemove"
                                    (decodeMove
                                        (listener_ |> rescaleListener rec.transformer)
                                        rec.box
                                        rec.transformer
                                        msg
                                    )
                                    |> Just
                    )
    in
    lazyLayer rec.box
        ((rec.commands
            |> List.concatMap eval
         )
            -- add the events to the end, so it is on top of the other elements
            ++ [ eventArea rec.transformer events ]
        )


{-| -}
type Element a
    = DataElement (CommandSet a)
    | IncludedData (List ( Maybe Float, Maybe Float ))
    | Event (EventSet a)
    | Node (Element a) (Element a)


type alias CommandSet a =
    { data : DataSet
    , attributes : List (Svg.Attribute a)
    , method : Method a
    }



-- DRAWING


commandSet : DataSet -> List (Svg.Attribute a) -> Method a -> CommandSet a
commandSet data =
    CommandSet data


{-| Line creates a line chart
-}
line : List (Svg.Attribute a) -> DataSet -> Element a
line attr data =
    commandSet data attr lineCmd |> DataElement


{-| Area creates a graph meant to be filled
-}
area : List (Svg.Attribute a) -> DataSet -> Element a
area attr data =
    commandSet data attr areaCmd |> DataElement


{-| Dot draws a dot at each point. Set the radius of the dot by styling it `[Svg.r "3"]`
-}
dot : List (Svg.Attribute a) -> DataSet -> Element a
dot attr data =
    commandSet data attr dotCmd |> DataElement


{-| Bar draws a bar graph of a given width.
-}
bar : List (Svg.Attribute a) -> Float -> DataSet -> Element a
bar attr width data =
    commandSet data attr (barCmd width) |> DataElement


{-| Label plots text on the graph
-}
label : List (Svg.Attribute a) -> LabelSet a -> Element a
label attr labelSet =
    let
        -- map out just the points to use as the underlying data
        data =
            List.map (\( p, _, _ ) -> p) labelSet
    in
    commandSet data attr (labelCmd labelSet) |> DataElement



-- OPTIONS


{-| extents will return a Maybe of two tuples from the passed Elements.

The first tuple is the low range of (x,y)
The second tuple is the high range of (x,y)

**Deprecated**

-}
extents : List (Element a) -> Maybe ( ( Float, Float ), ( Float, Float ) )
extents elements =
    elements
        |> List.filterMap
            (\e ->
                case e of
                    Node _ _ ->
                        Nothing

                    DataElement set ->
                        Just set.data

                    IncludedData _ ->
                        Nothing

                    Event _ ->
                        Nothing
            )
        |> List.concatMap identity
        |> (\points ->
                let
                    minMax v =
                        ( List.minimum v, List.maximum v )

                    ( x0, x1 ) =
                        List.map Tuple.first points
                            |> minMax

                    ( y0, y1 ) =
                        List.map Tuple.second points
                            |> minMax
                in
                case ( ( x0, y0 ), ( x1, y1 ) ) of
                    ( ( Just xlo, Just ylo ), ( Just xhi, Just yhi ) ) ->
                        Just ( ( xlo, ylo ), ( xhi, yhi ) )

                    _ ->
                        Nothing
           )


{-| Highlight is used to draw a region that has been selected. See `onSelect`
-}
highlight : List (Svg.Attribute a) -> Constraint -> Listener -> Element a
highlight attr con l =
    commandSet [] attr (highlightCmd attr con l) |> DataElement


{-| ZeroLine will draw a line at the 0 y axis
-}
zeroLine : List (Svg.Attribute a) -> Element a
zeroLine attr =
    DataElement (CommandSet [] attr zeroLineCmd)
        |> Node (IncludedData [ ( Nothing, Just 0 ) ])



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


lazyLayer : Box -> List (Svg a) -> Svg a
lazyLayer { x, y } children =
    Svg.lazy2
        Svg.g
        [ A.transform
            ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")
        ]
        children


zeroLineCmd : Method a
zeroLineCmd _ attr scalar =
    let
        (Domain ( ( x1, _ ), ( x2, _ ) )) =
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
         , d (path scalar.scale data)
         ]
            ++ attr
        )
        []
    ]


areaCmd : Method a
areaCmd data attr scalar =
    let
        (Domain ( ( _, miny ), _ )) =
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
        |> scale scalar.scale
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
        (Domain ( ( x0, _ ), ( x1, _ ) )) =
            scalar.domain

        ( mx, my ) =
            scalar.scale
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
        |> scale scalar.scale
        |> Array.fromList
        |> Array.toIndexedList
        |> List.concatMap
            (\( index, ( x, y ) ) ->
                case Array.get index indexed of
                    Nothing ->
                        []

                    Just ( _, attr, label_ ) ->
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



-- DOMAIN


{-| Domain defines the min (x,u) and max (x,y) points in the data.
-}
type Domain
    = Domain ( Point, Point )


findDomain : List ( Maybe Float, Maybe Float ) -> DataSet -> Domain
findDomain included dataset =
    let
        includedX =
            included
                |> List.filterMap Tuple.first

        includedY =
            included
                |> List.filterMap Tuple.second

        ensure (Domain ( ( x0, y0 ), ( x1, y1 ) )) =
            if y0 == y1 then
                ( ( x0, min 0 y0 ), ( x1, y1 ) ) |> Domain

            else
                ( ( x0, y0 ), ( x1, y1 ) ) |> Domain

        seedLo ( x, y ) =
            ( x :: includedX |> List.minimum |> Maybe.withDefault x
            , y :: includedY |> List.minimum |> Maybe.withDefault y
            )

        seedHi ( x, y ) =
            ( x :: includedX |> List.maximum |> Maybe.withDefault x
            , y :: includedY |> List.maximum |> Maybe.withDefault y
            )
    in
    case ( includedX, includedY, dataset ) of
        -- TODO returning an EmptyDomain would be better
        -- and not rendering anything when there is no data
        ( [ x0, x1 ], [ y0, y1 ], [] ) ->
            Domain ( ( x0, y0 ), ( x1, y1 ) )

        ( x, y, [] ) ->
            Domain
                ( ( List.minimum x |> Maybe.withDefault 0
                  , List.minimum y |> Maybe.withDefault 0
                  )
                , ( List.maximum x |> Maybe.withDefault 0
                  , List.maximum y |> Maybe.withDefault 0
                  )
                )

        ( _, _, seed :: rest ) ->
            rest
                |> List.foldr
                    (\( x, y ) ( ( xlo, ylo ), ( xhi, yhi ) ) ->
                        ( ( Basics.min xlo x, Basics.min ylo y )
                        , ( Basics.max xhi x, Basics.max yhi y )
                        )
                    )
                    ( seedLo seed, seedHi seed )
                |> Domain
                |> ensure


{-| domain will insert points into the Layer's Domain that are not meant to be rendered.
This is useful when creating multiple Layers that need to have the same scale
or ensure that a min/max value is in the scale.

For example:

        You are graphing the points [(10,5), (20,6)] but you want the graph to start at (0,0)

        call

        include  [(Just 0, Just 0)]

-}
include : List ( Maybe Float, Maybe Float ) -> Element a
include rec =
    IncludedData rec


{-| Get the Domain from a Layer
-}
getDomain : Layer a -> Domain
getDomain (Layer rec) =
    rec.domain


{-| creates x and y scale functions
-}
range : Box -> Domain -> Range
range box (Domain ( ( x0, y0 ), ( x1, y1 ) )) =
    ( \x ->
        (x - x0) * (box.width / (x1 - x0)) |> noNan
    , \y ->
        (y1 - y) * (box.height / (y1 - y0)) |> noNan
    )


{-| creates x and y scale inversion functions
-}
invert : Box -> Domain -> Range
invert box (Domain ( ( x0, y0 ), ( x1, y1 ) )) =
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
    , scalar : Maybe Transformer

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
subscribe (Listener listener_) eventMsg =
    Sub.batch
        (case listener_.mouse of
            MouseInactive ->
                []

            MouseDown ->
                [ Browser.onMouseMove (offsetPosition { listener_ | mouse = MouseDragging } eventMsg)
                , Browser.onMouseUp (Json.succeed ({ listener_ | clicked = Nothing, mouse = MouseInactive } |> Listener |> eventMsg))
                ]

            MouseDragging ->
                [ Browser.onMouseMove (offsetPosition listener_ eventMsg)
                , Browser.onMouseUp (offsetPosition { listener_ | clicked = Nothing, mouse = MouseInactive } eventMsg)
                ]
        )


{-| Clicked returns a point from a click event.
-}
clicked : Listener -> Maybe Point
clicked (Listener listener_) =
    case ( listener_.scalar, listener_.clicked ) of
        ( Just scalar, Just ( x, y ) ) ->
            let
                ( ix, iy ) =
                    scalar.invert
            in
            Just
                ( ix x, iy (y - listener_.box.height |> abs) )

        _ ->
            Nothing


{-| Hover returns a point from a hover event.
-}
hover : Listener -> Maybe Point
hover (Listener listener_) =
    case ( listener_.scalar, listener_.hover ) of
        ( Just scalar, Just ( x, y ) ) ->
            let
                ( ix, iy ) =
                    scalar.invert
            in
            Just
                ( ix x, iy (y - listener_.box.height |> abs) )

        _ ->
            Nothing


{-| Hover returns a point from a hover event.
-}
active : Listener -> Bool
active (Listener listener_) =
    case listener_.mouse of
        MouseInactive ->
            False

        MouseDown ->
            True

        MouseDragging ->
            True


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
selection (Listener listener_) =
    case ( listener_.scalar, listener_.start, listener_.current ) of
        ( Just scalar, Just ( dx0, dy0 ), Just ( dx1, dy1 ) ) ->
            let
                ( ix, iy ) =
                    scalar.invert

                ( bx0, by0 ) =
                    ( ix dx0, iy (dy0 - listener_.box.height |> abs) )

                ( bx1, by1 ) =
                    ( ix dx1, iy (dy1 - listener_.box.height |> abs) )
            in
            Just
                ( ( min bx0 bx1, min by0 by1 )
                , ( max bx0 bx1, max by0 by1 )
                )

        _ ->
            Nothing


offsetPosition : EventRecord -> (Listener -> a) -> Json.Decoder a
offsetPosition listener_ msg =
    case listener_.offset of
        Nothing ->
            Json.succeed (msg (Listener listener_))

        Just ( x0, y0 ) ->
            Json.map2
                (\x y ->
                    let
                        sel_ =
                            { listener_ | current = Just ( (x |> toFloat) - x0, (y |> toFloat) - y0 ) }
                    in
                    Listener sel_ |> msg
                )
                (Json.field "pageX" Json.int)
                (Json.field "pageY" Json.int)


decodeSelection : Listener -> Box -> Transformer -> (Listener -> msg) -> Json.Decoder msg
decodeSelection (Listener listener_) box scalar msg =
    Json.map4
        (\oX oY x y ->
            let
                sel_ =
                    { listener_
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


decodeClick : Listener -> Box -> Transformer -> (Listener -> msg) -> Json.Decoder msg
decodeClick (Listener listener_) box scalar msg =
    Json.map2
        (\x y ->
            let
                sel_ =
                    { listener_
                        | scalar = Just scalar
                        , mouse = MouseInactive
                        , box = box
                        , clicked =
                            if listener_.current == Nothing then
                                Just ( (x |> toFloat) - box.x, (y |> toFloat) - box.y )

                            else
                                Nothing
                    }
            in
            msg (Listener sel_)
        )
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)


decodeMove : Listener -> Box -> Transformer -> (Listener -> msg) -> Json.Decoder msg
decodeMove (Listener listener_) box scalar msg =
  let
      _ = Debug.log "l" listener_.mouse
  in
    Json.map3
        (\x y button ->
            (if button == 0 && listener_.mouse == MouseDragging then
                { listener_ | mouse = MouseInactive }

             else
                { listener_
                    | scalar = Just scalar
                    , box = box
                    , hover = Just ( (x |> toFloat) - box.x, (y |> toFloat) - box.y )
                }
            )
                |> Listener
                |> msg
        )
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)
        (Json.field "buttons" Json.int)


eventArea : Transformer -> List (Svg.Attribute a) -> Svg a
eventArea scalar events =
    let
        (Domain ( ( x1, y1 ), ( x2, y2 ) )) =
            scalar.domain

        ( mx, my ) =
            scalar.scale
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



-- the graph size changed since the highlight was created, we need to fix it


rescaleListener : Transformer -> Listener -> Listener
rescaleListener scalar (Listener l) =
    Listener <|
        case l.scalar of
            Nothing ->
                l

            Just s ->
                let
                    xScale =
                        scalar.box.width / s.box.width

                    yScale =
                        scalar.box.height / s.box.height

                    map =
                        Maybe.map
                            (\( x, y ) ->
                                ( x * xScale, y * yScale )
                            )
                in
                { l | current = map l.current, start = map l.start }


highlightCmd : List (Svg.Attribute a) -> Constraint -> Listener -> Method a
highlightCmd style constraint listener_ _ _ scalar =
    case rescaleListener scalar listener_ of
        Listener l ->
            case ( l.start, l.current ) of
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
