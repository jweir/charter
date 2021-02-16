module Charter.Events exposing
    ( Listener(..), Mouse(..), Record, Set(..)
    , listener, onClick, onHover, onSelect, subscribe
    , active, clicked, hover, selection
    )

{-| This is the Events module for Charter


# Example

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


# Records

@docs Listener, Mouse, Record, Set


# Events

@docs listener, onClick, onHover, onSelect, subscribe


# Event Data

Use the below functions to extract the data from events. The Point values
returned are scaled to the input data, not the mouse events.

@docs active, clicked, hover, selection

-}

import Browser.Events as BE
import Json.Decode as Json
import Svg exposing (Svg)



-- RECORDS


{-| A listener to maintain the state of events (selection, hover and clicks). A
listener can be shared across charts with the same scale.
-}
type Listener
    = Listener Record


{-| Mouse Events
-}
type Mouse
    = MouseDown
    | MouseDragging
    | MouseInactive


{-| Record type that holds event data
-}
type alias Record =
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


{-| Set of all possible Events
-}
type Set msg
    = Select Listener (Listener -> msg)
    | Click Listener (Listener -> msg)
    | Hover Listener (Listener -> msg)



-- EVENTS


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


{-| onClick tracks click events.
-}
onClick : Listener -> (Listener -> msg) -> Element msg
onClick l eventMsg =
    Click l eventMsg |> Event


{-| onHover tracks the mouse moving over the chart.
-}
onHover : Listener -> (Listener -> msg) -> Element msg
onHover l eventMsg =
    Hover l eventMsg |> Event


{-| onSelect event for when a selection is made.
-}
onSelect : Listener -> (Listener -> msg) -> Element msg
onSelect l eventMsg =
    Select l eventMsg |> Event


{-| When tracking `onSelect` a subscription will be required. The mouse events are tracked outside of the chart's SVG element.

        type alias Model =
            { listener : Listener }

        type Msg
            = Select Listener

        subscriptions =
            Sub.batch [ subscribe model.listener Select ]

-}
subscribe : Listener -> (Listener -> msg) -> Sub msg
subscribe (Listener listener_) eventMsg =
    Sub.batch
        (case listener_.mouse of
            MouseInactive ->
                []

            MouseDown ->
                [ BE.onMouseMove (offsetPosition { listener_ | mouse = MouseDragging } eventMsg)
                , BE.onMouseUp (Json.succeed ({ listener_ | clicked = Nothing, mouse = MouseInactive } |> Listener |> eventMsg))
                ]

            MouseDragging ->
                [ BE.onMouseMove (offsetPosition listener_ eventMsg)
                , BE.onMouseUp (offsetPosition { listener_ | clicked = Nothing, mouse = MouseInactive } eventMsg)
                ]
        )


offsetPosition : Record -> (Listener -> msg) -> Json.Decoder msg
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



-- EVENT DATA


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


{-| Clicked returns a point from a click event.
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


{-| Hover returns a point from a hover event.
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


{-| Selection returns a box with the selected boundaries of the data.

Use this selection to filter the applications data into a subset.

    filter : DataSet -> Listener -> DataSet
    filter data listener_ =
        case selection listener_ of
            Nothing ->
                []

            Just ( ( x0, _ ), ( x1, _ ) ) ->
                data
                    |> List.filter (\( x, _ ) -> x >= x0 && x <= x1)

-}
selection : Listener -> Maybe ( Point, Point )
selection (Listener listener_) =
    -- TODO consider renaming to selected?
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



-- TYPES (DUPLICATES -- MOVE ELSEWHERE?)


{-| -}
type Element msg
    = Command (CommandSet msg)
    | Event (Set msg)


type alias CommandSet msg =
    { method : Method msg
    , data : DataSet
    , attributes : List (Svg.Attribute msg)
    }


type alias Method msg =
    DataSet
    -> List (Svg.Attribute msg)
    -> Scalar
    -> List (Svg msg)


{-| The data to be rendered.
-}
type alias DataSet =
    List Point


type alias Scalar =
    { domain : Domain

    -- range scales from the orginal data to the coordinates on the sized canvas
    , range : Range

    -- invert does the opposite of range
    , inverter : Range
    , box : Box
    }


type alias Domain =
    ( Point, Point )


{-| Tuple of (x,y) value
-}
type alias Point =
    ( Float, Float )


{-| Defines the size and position of chart elements
-}
type alias Box =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    }


type alias Range =
    ( Float -> Float, Float -> Float )
