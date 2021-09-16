module Charter.Internals exposing (..)

import Json.Decode as Json
import Svg
    exposing
        ( Svg
        , rect
        )
import Svg.Attributes as A
    exposing
        ( fill
        , height
        , width
        , x
        , y
        )
import Svg.Events as E


type Mouse
    = MouseDown
    | MouseDragging
    | MouseInactive


type alias Box =
    { width : Float
    , height : Float
    , x : Float
    , y : Float
    }


type alias Point =
    ( Float, Float )


type alias Scale =
    ( Float -> Float, Float -> Float )


type Element a
    = Command (CommandSet a)
    | Event (EventSet a)


type EventSet a
    = EventSelect Listener (Listener -> a)
    | EventClick Listener (Listener -> a)
    | EventHover Listener (Listener -> a)


type alias Domain =
    ( Point, Point )


type alias Method a =
    List Point
    -> List (Svg.Attribute a)
    -> Scalar
    -> List (Svg a)


type alias Scalar =
    { domain : Domain

    -- range scales from the orginal data to the coordinates on the sized canvas
    , range : Scale

    -- invert does the opposite of range
    , inverter : Scale
    , box : Box
    }


type alias CommandSet a =
    { method : Method a
    , data : List Point
    , attributes : List (Svg.Attribute a)
    }



-- EVENTS


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



-- FUNCS


setAttr : (String -> a) -> Float -> a
setAttr fun n =
    fun (String.fromFloat n)



-- EVENTS


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


decodeSelection : Listener -> Box -> Scalar -> (Listener -> msg) -> Json.Decoder msg
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


decodeClick : Listener -> Box -> Scalar -> (Listener -> msg) -> Json.Decoder msg
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


decodeMove : Listener -> Box -> Scalar -> (Listener -> msg) -> Json.Decoder msg
decodeMove (Listener listener_) box scalar msg =
    Json.map2
        (\x y ->
            let
                sel_ =
                    { listener_
                        | scalar = Just scalar
                        , box = box
                        , hover = Just ( (x |> toFloat) - box.x, (y |> toFloat) - box.y )
                    }
            in
            msg (Listener sel_)
        )
        (Json.field "offsetX" Json.int)
        (Json.field "offsetY" Json.int)


eventArea : Scalar -> List (Svg.Attribute a) -> Svg a
eventArea scalar events =
    let
        ( ( x1, y1 ), ( x2, y2 ) ) =
            scalar.domain

        ( mx, my ) =
            scalar.range
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


eventConvert : List (EventSet msg) -> Box -> Scalar -> List (Svg.Attribute msg)
eventConvert eventELs box scalar =
    eventELs
        |> List.filterMap
            (\eventSet ->
                case eventSet of
                    EventSelect (Listener listener_) msg ->
                        case listener_.mouse of
                            MouseInactive ->
                                Just <|
                                    E.on "mousedown"
                                        (decodeSelection
                                            (Listener listener_ |> rescaleListener scalar)
                                            box
                                            scalar
                                            msg
                                        )

                            MouseDown ->
                                Nothing

                            MouseDragging ->
                                Nothing

                    EventClick listener_ msg ->
                        Just <|
                            E.on "click"
                                (decodeClick
                                    (listener_ |> rescaleListener scalar)
                                    box
                                    scalar
                                    msg
                                )

                    EventHover listener_ msg ->
                        Just <|
                            E.on "mousemove"
                                (decodeMove
                                    (listener_ |> rescaleListener scalar)
                                    box
                                    scalar
                                    msg
                                )
            )
