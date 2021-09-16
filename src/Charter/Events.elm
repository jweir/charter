module Charter.Events exposing
    ( listener, Listener, subscribe
    , onSelect, onClick, onHover
    , selection, clicked, hover, active
    , highlight, filter, Constraint(..)
    )

{-| In Charter Events are tracked with a Listener which will be part of the program's model.

The Listener can be shared across different charts to keep them in sync.

If you are tracking Selection events you must have a subscription to the listener.

    -- FIXME  make this real
    type alias Model =
        { listener : Charter.Listener
        }

    type Msg
        = Select Charter.Listener
        | Click Charter.Listener
        | Hover Charter.Listener

    subscriptions =
        Sub.batch [ subscribe model.listener Select ]

    view model =
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

@docs listener, Listener, subscribe


# Events

The below attributes are added to a Layer to track the events.

@docs onSelect, onClick, onHover


# Listener Data

When an event occurs the Listener can be examined get the selection area,
clicked point, hovered point or if the mouse is down. Any Points returned are
based on the data not the screen poisition.

@docs selection, clicked, hover, active


# View

@docs highlight, filter, Constraint

-}

import Browser.Events as Browser
import Charter.Internals as I exposing (..)
import Json.Decode as Json
import Svg
    exposing
        ( rect
        )
import Svg.Attributes as A
    exposing
        ( fill
        , height
        , width
        , x
        , y
        )


{-| When highlighting a selected region the application can have the selection contrainted to just the X axis or be free.

You would most likely use OnlyX when selecting a timeseries.

-}
type Constraint
    = XY
    | OnlyX
    | OnlyY


{-| A listener to maintain the state of events (selection, hover and clicks). A
listener can be shared across charts with the same scale.
-}
type alias Listener =
    I.Listener


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


{-| Highlight is used to draw a region that has been selected. See `onSelect`
-}
highlight : List (Svg.Attribute a) -> Constraint -> Listener -> Element a
highlight attr con l =
    CommandSet (highlightCmd attr con l) [] attr |> Command


{-| Filter selected data
-}
filter : List Point -> Constraint -> Listener -> List Point
filter data constraint listener_ =
    case selection listener_ of
        Nothing ->
            []

        Just ( ( x0, y0 ), ( x1, y1 ) ) ->
            let
                between =
                    case constraint of
                        XY ->
                            \( x, y ) -> (x >= x0 && x <= x1) && (y >= y0 && y <= y1)

                        OnlyX ->
                            \( x, _ ) -> x >= x0 && x <= x1

                        OnlyY ->
                            \( _, y ) -> y >= y0 && y <= y1
            in
            data
                |> List.filter between


highlightCmd : List (Svg.Attribute a) -> Constraint -> Listener -> Method a
highlightCmd style constraint listener_ _ _ scalar =
    case rescaleListener scalar listener_ of
        Listener l ->
            case ( l.start, l.current ) of
                ( Just ( ax1, ay1 ), Just ( bx1, by1 ) ) ->
                    let
                        box =
                            case constraint of
                                XY ->
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

                                OnlyY ->
                                    Box
                                        scalar.box.width
                                        (ay1 - by1 |> abs)
                                        0
                                        (min ay1 by1)
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


rescaleListener : Scalar -> Listener -> Listener
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


{-| When tracking `onSelect` a subscription will be required. The mouse events
are tracked outside of the chart's SVG element.

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


{-| Returns a point from a click event.
-}
clicked : Listener -> Maybe Point
clicked (Listener listener_) =
    case ( listener_.scalar, listener_.clicked ) of
        ( Just scalar, Just ( x, y ) ) ->
            let
                ( ix, iy ) =
                    scalar.inverter
            in
            Just
                ( ix x, iy (y - listener_.box.height |> abs) )

        _ ->
            Nothing


{-| Returns a point from a hover event.
-}
hover : Listener -> Maybe Point
hover (Listener listener_) =
    case ( listener_.scalar, listener_.hover ) of
        ( Just scalar, Just ( x, y ) ) ->
            let
                ( ix, iy ) =
                    scalar.inverter
            in
            Just
                ( ix x, iy (y - listener_.box.height |> abs) )

        _ ->
            Nothing


{-| Returns true when the mouse is down or dragging.
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


{-| Returns returns a box with the selected boundaries of the **data** not the screen position.

This can be used to filter the data that is within the selection. For example
this filter will return all data within the X boundaries.

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
                    scalar.inverter

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



-- EVENTS


{-| onSelect event for when a selection is made.

**Note** Once a selection is created the next click event will remove the selection. This will override any onClick attributes.

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
