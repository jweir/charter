module Example exposing (main)

import Browser
import Charter exposing (Box, Layer(..), Point, Size, bar, chart, labels, sparkline)
import Charter.Events as Charter
import Charter.Extras as Charter
import Html as Html
import Html.Attributes as HA
import Html.Events as HE
import Svg
import Svg.Attributes as Svg
import Time


type alias Model =
    { listener : Charter.Listener
    , listener2 : Charter.Listener
    , clicked : Maybe Point
    , hover : Maybe Point
    , width : Size
    }


type Size
    = Large
    | Small


type Msg
    = Select Charter.Listener
    | Select2 Charter.Listener
    | Click Charter.Listener
    | Hover Charter.Listener
    | ChangeSize Size


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { listener = Charter.listener
                  , listener2 = Charter.listener
                  , clicked = Nothing
                  , hover = Nothing
                  , width = Large
                  }
                , Cmd.none
                )
        , view = \model -> view model
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Charter.subscribe model.listener Select
        , Charter.subscribe model.listener2 Select2
        ]


filter : List Charter.Point -> Charter.Listener -> List Charter.Point
filter data listener =
    case Charter.selection listener of
        Nothing ->
            []

        Just ( ( x0, _ ), ( x1, _ ) ) ->
            data
                |> List.filter (\( x, _ ) -> x >= x0 && x <= x1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSize width ->
            ( { model
                | width = width
              }
            , Cmd.none
            )

        Click sel ->
            ( { model | listener = sel, clicked = Charter.clicked sel }
            , Cmd.none
            )

        Hover sel ->
            ( { model | listener = sel, hover = Charter.hover sel }
            , Cmd.none
            )

        Select sel ->
            ( { model | listener = sel }
            , Cmd.none
            )

        Select2 sel ->
            ( { model | listener2 = sel }
            , Cmd.none
            )


view : Model -> Html.Html Msg
view model =
    let
        width =
            if model.width == Large then
                600

            else
                300
    in
    Html.div []
        [ Html.div []
            [ Html.p []
                [ Charter.chart (Charter.Size 100 100)
                    [ Charter.Layer (Charter.Box 90 90 5 5)
                        [ Charter.area [] [ ( 0, 10 ), ( 10, 20 ), ( 20, 0 ) ]
                        , Charter.area [ Svg.stroke "red" ] [ ( 5, 5 ), ( 10, 5 ) ]
                        ]
                    ]
                ]
            , Html.p []
                [ chart (Size 400 200)
                    [ Layer (Box 400 50 0 0)
                        [ Charter.zeroLine []
                        , bar [] 5 [ ( 0, 10 ), ( 1, -5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                        ]
                    , Layer (Box 400 50 0 50)
                        [ Charter.zeroLine []
                        , bar [ Svg.fill "red" ] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                        ]
                    ]
                ]
            , Html.p []
                [ chart (Size 400 200)
                    [ Layer (Box 400 50 0 0)
                        [ Charter.zeroLine []
                        , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                        , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                        ]
                    , Layer (Box 400 50 0 50)
                        [ Charter.zeroLine []
                        , bar [ Svg.fill "red" ] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                        , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                        ]
                    , Layer (Box 400 50 0 100)
                        [ Charter.zeroLine []
                        , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                        , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                        ]
                    , Layer (Box 400 50 0 150)
                        [ Charter.zeroLine []
                        , bar [] 5 [ ( 0, 10 ), ( 1, 5 ), ( 2, 4 ), ( 3, 8 ), ( 4, 6 ), ( 5, 11 ) ]
                        , Charter.insertDomain [ ( 0, 0 ), ( 1, 1 ) ]
                        ]
                    ]
                ]
            , Html.p []
                [ Html.text "Mouse over, click and click and drag the below chart for example interactions"
                , if Charter.active model.listener == True then
                    Html.text " (mouse is down)"

                  else
                    Html.text " (mouse is up)"
                , if model.width == Small then
                    Html.button [ HE.onClick (ChangeSize Large) ] [ Html.text "Larger graph" ]

                  else
                    Html.button [ HE.onClick (ChangeSize Small) ] [ Html.text "Smaller graph" ]
                ]
            , chart (Size (width + 20) 120)
                [ Layer
                    (Box width 70 10 10)
                    [ Charter.highlight
                        [ Svg.fill "rgba(255,255,0,0.4)", Svg.stroke "rgba(0,0,0,0.2)", Svg.strokeWidth "0 2 0 2" ]
                        Charter.OnlyX
                        model.listener
                    ]
                , Layer
                    (Box width 50 10 10)
                    [ Charter.onClick model.listener Click
                    , Charter.onSelect model.listener Select
                    , Charter.onHover model.listener Hover
                    , Charter.line [ Svg.stroke "red" ] data0
                    , Charter.line [ Svg.stroke "gray" ] (data0 |> Charter.step Charter.Before)
                    , Charter.line [] (data1 |> Charter.step Charter.Before)
                    , Charter.dot [ Svg.r "5" ] (List.filterMap identity [ model.clicked ])
                    , Charter.dot [ Svg.fill "red", Svg.r "2" ] (List.filterMap identity [ nearestPoint data0 model.hover ])
                    , Charter.labels [] (hoverLabel model.hover)
                    , Charter.zeroLine []
                    ]
                , Layer
                    (Box width 20 10 60)
                    [ Charter.bar [] 2 data2
                    , Charter.zeroLine []
                    ]
                , timeAxis (Box width 10 10 80) 10 data0
                ]
            , if Charter.selection model.listener /= Nothing then
                Html.div [ HA.style "position" "absolute", HA.style "left" "600px" ]
                    [ chart (Size 820 440)
                        [ Layer
                            (Box 800 380 10 10)
                            [ Charter.line [] (filter data0 model.listener)
                            , Charter.line [] (filter data1 model.listener)
                            , Charter.zeroLine []
                            ]
                        , Layer (Box 800 20 10 390)
                            [ Charter.area [ Svg.stroke "none", Svg.fill "rgb(150,150,255)" ] (filter data2 model.listener) ]
                        , timeAxis (Box 800 10 10 410) 10 (filter data1 model.listener)
                        ]
                    ]

              else
                Html.text ""
            , Html.p []
                [ Html.text "Here is a sparkline. "
                , sparkline (Size 100 20)
                    [ Charter.onSelect model.listener2 Select2
                    , Charter.highlight [] Charter.OnlyX model.listener2
                    , Charter.line [] data0
                    , Charter.line [] data1
                    ]
                , Html.text " Click and drag on it for details."
                , if Charter.selection model.listener2 /= Nothing then
                    let
                        a =
                            filter data0 model.listener2

                        b =
                            filter data1 model.listener2
                    in
                    Html.div []
                        [ chart (Size 620 140)
                            [ Layer (Box 600 100 10 10)
                                [ Charter.line [] a
                                , Charter.line [] b
                                , Charter.zeroLine []
                                ]
                            , timeAxis (Charter.Box 600 10 10 110) 20 a
                            ]
                        ]

                  else
                    Html.text ""
                ]
            ]
        ]


hoverLabel : Maybe Point -> List ( List (Svg.Attribute msg), Point, String )
hoverLabel point =
    [ nearestPoint data0 point ]
        |> List.filterMap identity
        |> List.map (\( x, y ) -> ( [], ( x, y ), toFloat (round (y * 10)) / 10 |> String.fromFloat ))


nearestPoint : List Point -> Maybe Point -> Maybe Point
nearestPoint points point =
    point
        |> Maybe.andThen
            (\( x0, _ ) ->
                points
                    |> List.foldr
                        (\( x, y ) p ->
                            if x0 < x then
                                Just ( x, y )

                            else
                                p
                        )
                        Nothing
            )


timeAxis : Charter.Box -> Int -> List Charter.Point -> Charter.Layer Msg
timeAxis box ticks data =
    let
        ( x0, x1 ) =
            Charter.minMax Charter.X data

        delta =
            (x1 - x0) / toFloat ticks

        times =
            List.repeat ticks ( x0, box.height )
                |> List.indexedMap (\i ( x, y ) -> ( x + (toFloat i * delta), y ))

        fmtTime time =
            time
                |> round
                |> Time.millisToPosix
                |> (\t ->
                        (String.fromInt (Time.toHour Time.utc t) |> String.padLeft 2 '0')
                            ++ ":"
                            ++ (String.fromInt (Time.toMinute Time.utc t) |> String.padLeft 2 '0')
                   )
    in
    Layer box
        [ Charter.insertDomain [ ( x0, 0 ), ( x1, box.height ) ]
        , Charter.bar [] 1 times
        , Charter.labels
            [ Svg.fontSize "10px", Svg.textAnchor "middle", Svg.transform "translate(0, 10)" ]
            (List.map (\( x, y ) -> ( [], ( x, y - 10 ), x |> fmtTime )) times)
        ]


data0 : List Point
data0 =
    [ ( 1566316269000, 56.99155771405509 ), ( 1566316569000, 65.40522127334349 ), ( 1566316869000, 62.28250742065351 ), ( 1566317169000, 54.494141612308304 ), ( 1566317469000, 160.22734698131669 ), ( 1566317769000, 59.694706746841916 ), ( 1566318069000, 67.20187203253775 ), ( 1566318369000, 68.48341987359878 ), ( 1566318669000, 76.33499789564587 ), ( 1566318969000, 85.6591246526396 ), ( 1566319269000, 95.40680719354866 ), ( 1566319569000, 89.29637815054691 ), ( 1566319869000, 185.74396266388206 ), ( 1566320169000, 90.87238160570496 ), ( 1566320469000, 89.94887948404033 ), ( 1566320769000, 82.06833616917892 ), ( 1566321069000, 72.8788211599817 ), ( 1566321369000, 73.16664675394458 ), ( 1566321669000, 77.09334163692355 ), ( 1566321969000, 78.18059846595753 ), ( 1566322269000, 76.93853078567946 ), ( 1566322569000, 80.7223619342112 ), ( 1566322869000, 77.07967381927261 ), ( 1566323169000, 73.8079768300908 ), ( 1566323469000, 69.91058881264279 ), ( 1566323769000, 67.14232112141768 ), ( 1566324069000, 65.91263668636374 ), ( 1566324369000, 62.25260373990991 ), ( 1566324669000, 72.13677749730174 ), ( 1566324969000, 71.81783779340581 ), ( 1566325269000, 70.29526824888524 ), ( 1566325569000, 67.29508464523052 ), ( 1566325869000, 71.91888223996087 ), ( 1566326169000, 67.63718601336727 ), ( 1566326469000, 72.86868048033637 ), ( 1566326769000, 72.9198601852277 ), ( 1566327069000, 78.7385083733748 ), ( 1566327369000, 88.60900611498715 ), ( 1566327669000, 98.3734720315465 ), ( 1566327969000, 100.73544692707404 ), ( 1566328269000, 106.31840394509402 ), ( 1566328569000, 101.57435763410402 ), ( 1566328869000, 92.88272478653661 ), ( 1566329169000, 89.81975842131928 ), ( 1566329469000, 91.91776111259256 ), ( 1566329769000, 99.86885670866818 ), ( 1566330069000, 98.00638234776305 ), ( 1566330369000, 101.73392346657764 ), ( 1566330669000, 92.16320323953553 ), ( 1566330969000, 94.22195066118152 ), ( 1566331269000, 99.54868609518785 ), ( 1566331569000, 108.16504543445346 ), ( 1566331869000, 103.57265146344885 ), ( 1566332169000, 103.60357336916117 ), ( 1566332469000, 108.36828266785857 ), ( 1566332769000, 110.50706089732918 ), ( 1566333069000, 109.69338540873346 ), ( 1566333369000, 109.00319764619583 ), ( 1566333669000, 109.98535981008649 ), ( 1566333969000, 101.67332287305689 ), ( 1566334269000, 103.01486349894994 ), ( 1566334569000, 108.68948536624723 ), ( 1566334869000, 113.13639608088786 ), ( 1566335169000, 118.7062444152466 ), ( 1566335469000, 128.16266912238297 ), ( 1566335769000, 126.21255847962469 ), ( 1566336069000, 130.8889819364268 ), ( 1566336369000, 140.84168269336902 ), ( 1566336669000, 139.63831620893356 ), ( 1566336969000, 147.52729051845563 ), ( 1566337269000, 140.72998801414235 ), ( 1566337569000, 137.48387692291948 ), ( 1566337869000, 144.76561406382083 ), ( 1566338169000, 148.06788323504458 ), ( 1566338469000, 146.8075203195303 ), ( 1566338769000, 143.6832929206303 ), ( 1566339069000, 146.33798423209296 ), ( 1566339369000, 142.29021524719914 ), ( 1566339669000, 135.476475253683 ), ( 1566339969000, 128.0611322285998 ), ( 1566340269000, 128.59584153917174 ), ( 1566340569000, 119.9586881415845 ), ( 1566340869000, 127.35214289391067 ), ( 1566341169000, 126.2087994271236 ), ( 1566341469000, 127.44381369096834 ), ( 1566341769000, 122.39235945563749 ), ( 1566342069000, 124.03211366151912 ), ( 1566342369000, 131.081400509839 ), ( 1566342669000, 129.50931878753647 ), ( 1566342969000, 132.7408682104988 ), ( 1566343269000, 126.93839689810137 ), ( 1566343569000, 117.58039744496676 ), ( 1566343869000, 125.49725419555367 ), ( 1566344169000, 130.32281910483903 ), ( 1566344469000, 132.58652208275032 ), ( 1566344769000, 141.52624070766342 ), ( 1566345069000, 149.815695141087 ), ( 1566345369000, 158.34152662712927 ), ( 1566345669000, 165.19020452982753 ), ( 1566345969000, 160.38097971139362 ) ]


data1 : List Point
data1 =
    [ ( 1566316269000, 53.88115227817786 ), ( 1566316569000, 49.43940495062502 ), ( 1566316869000, 47.916388474463744 ), ( 1566317169000, 50.413720202088726 ), ( 1566317469000, 46.01439316350987 ), ( 1566317769000, 37.23541455349272 ), ( 1566318069000, 44.57533851745928 ), ( 1566318369000, 54.162160945215874 ), ( 1566318669000, 47.49156258285102 ), ( 1566318969000, 50.194073098969234 ), ( 1566319269000, 42.28044306409957 ), ( 1566319569000, 38.966813333037166 ), ( 1566319869000, 37.851817515024344 ), ( 1566320169000, 34.44280593237343 ), ( 1566320469000, 32.59711379917507 ), ( 1566320769000, 38.85047424217096 ), ( 1566321069000, 29.066983094071627 ), ( 1566321369000, 21.552656029448986 ), ( 1566321669000, 27.064398649696667 ), ( 1566321969000, 35.05928636700749 ), ( 1566322269000, 29.79402670171838 ), ( 1566322569000, 26.220545246988156 ), ( 1566322869000, 33.383411262579365 ), ( 1566323169000, 32.257891418725194 ), ( 1566323469000, 28.773605117439757 ), ( 1566323769000, 31.620756736227385 ), ( 1566324069000, 37.57149893465314 ), ( 1566324369000, 37.06246065675279 ), ( 1566324669000, 42.395330417281755 ), ( 1566324969000, 38.939407107270014 ), ( 1566325269000, 41.62523822057247 ), ( 1566325569000, 32.258712323202325 ), ( 1566325869000, 40.91534283536247 ), ( 1566326169000, 40.19918272043242 ), ( 1566326469000, 32.440322525508755 ), ( 1566326769000, 22.639968051915844 ), ( 1566327069000, 18.686299929729536 ), ( 1566327369000, 18.457765131041697 ), ( 1566327669000, 12.656526635761743 ), ( 1566327969000, 6.803985185340876 ), ( 1566328269000, 3.793862395429123 ), ( 1566328569000, 11.025979989639005 ), ( 1566328869000, 1.7575070758118798 ), ( 1566329169000, 9.476224543563925 ), ( 1566329469000, 1.1569837864079116 ), ( 1566329769000, 2.5535278903328713 ), ( 1566330069000, 11.197574030034112 ), ( 1566330369000, 16.287902809915995 ), ( 1566330669000, 7.815446250859516 ), ( 1566330969000, 16.834724101805413 ), ( 1566331269000, 9.121018471602781 ), ( 1566331569000, 0.24572437557188564 ), ( 1566331869000, -7.510865378120805 ), ( 1566332169000, 2.374656152545896 ), ( 1566332469000, 3.6049472147515473 ), ( 1566332769000, 7.282340595178235 ), ( 1566333069000, 5.450104168544147 ), ( 1566333369000, 7.4322548179553785 ), ( 1566333669000, 11.432678412946197 ), ( 1566333969000, 10.890952977617564 ), ( 1566334269000, 14.583388473199536 ), ( 1566334569000, 9.228481627841273 ), ( 1566334869000, 9.50943178914393 ), ( 1566335169000, 13.401060404511899 ), ( 1566335469000, 14.13628428469293 ), ( 1566335769000, 10.439867316836143 ), ( 1566336069000, 8.398437046737842 ), ( 1566336369000, 1.723005143529468 ), ( 1566336669000, -3.880787387208862 ), ( 1566336969000, -2.4670677104933763 ), ( 1566337269000, 4.349545891841045 ), ( 1566337569000, 1.5830328080329181 ), ( 1566337869000, 2.007219163803155 ), ( 1566338169000, 7.460902598064358 ), ( 1566338469000, 7.823268445085985 ), ( 1566338769000, 16.97604488777312 ), ( 1566339069000, 20.66373556938672 ), ( 1566339369000, 16.15615584656919 ), ( 1566339669000, 23.048793850852086 ), ( 1566339969000, 14.944506583665774 ), ( 1566340269000, 8.050113331324727 ), ( 1566340569000, 2.7492179941194443 ), ( 1566340869000, -1.9415485951220255 ), ( 1566341169000, -8.497733740585794 ), ( 1566341469000, -1.6956569580919494 ), ( 1566341769000, 4.686255969861807 ), ( 1566342069000, 11.674306796732754 ), ( 1566342369000, 20.735828719891654 ), ( 1566342669000, 16.87902745119135 ), ( 1566342969000, 21.760644951074177 ), ( 1566343269000, 29.26264087650107 ), ( 1566343569000, 24.89379282105058 ), ( 1566343869000, 34.63199343506639 ), ( 1566344169000, 37.90493460661793 ), ( 1566344469000, 40.11326790432958 ), ( 1566344769000, 43.069016701710375 ), ( 1566345069000, 40.6075115869604 ), ( 1566345369000, 42.235349819431114 ), ( 1566345669000, 43.854287937404386 ), ( 1566345969000, 42.86712185276707 ) ]


data2 : List Point
data2 =
    [ ( 1566316269000, 12.343692811576366 ), ( 1566316569000, 18.32421707424968 ), ( 1566316869000, 22.568692523502797 ), ( 1566317169000, 25.033745172088846 ), ( 1566317469000, 15.55284354173504 ), ( 1566317769000, 14.176439071996576 ), ( 1566318069000, 17.069576657214373 ), ( 1566318369000, 19.574660451387356 ), ( 1566318669000, 17.14782954081708 ), ( 1566318969000, 22.563226096694088 ), ( 1566319269000, 18.047238133888225 ), ( 1566319569000, 20.88362289388259 ), ( 1566319869000, 30.376809928300005 ), ( 1566320169000, 24.28120469635714 ), ( 1566320469000, 29.283096974494725 ), ( 1566320769000, 30.98115288563177 ), ( 1566321069000, 25.04306996197787 ), ( 1566321369000, 22.465752291040786 ), ( 1566321669000, 31.918458121426625 ), ( 1566321969000, 34.62210131352956 ), ( 1566322269000, 35.652046018720014 ), ( 1566322569000, 35.704747612455684 ), ( 1566322869000, 25.886492358371093 ), ( 1566323169000, 30.58484525662365 ), ( 1566323469000, 26.5358881721111 ), ( 1566323769000, 25.993841253135685 ), ( 1566324069000, 20.48783621456232 ), ( 1566324369000, 26.305339494259393 ), ( 1566324669000, 16.4776512314695 ), ( 1566324969000, 14.851896453090198 ), ( 1566325269000, 23.951092995408718 ), ( 1566325569000, 16.293541003339808 ), ( 1566325869000, 17.216323905936456 ), ( 1566326169000, 7.38779235184289 ), ( 1566326469000, 15.398382841931237 ), ( 1566326769000, 20.713689090180807 ), ( 1566327069000, 21.080438167649515 ), ( 1566327369000, 21.94027474445918 ), ( 1566327669000, 18.834389907026527 ), ( 1566327969000, 11.185612308820598 ), ( 1566328269000, 16.418506721114483 ), ( 1566328569000, 23.247595127550426 ), ( 1566328869000, 15.273021820315858 ), ( 1566329169000, 8.638815343453683 ), ( 1566329469000, 16.07309347559972 ), ( 1566329769000, 7.791020014825893 ), ( 1566330069000, 9.102329832135556 ), ( 1566330369000, 6.850929728461633 ), ( 1566330669000, 2.191744549870476 ), ( 1566330969000, 3.344997222490367 ), ( 1566331269000, -0.09300927983148632 ), ( 1566331569000, -0.31489418453406337 ), ( 1566331869000, 1.3765181554223602 ), ( 1566332169000, 6.826858750382611 ), ( 1566332469000, 14.307006431856845 ), ( 1566332769000, 14.227781590576429 ), ( 1566333069000, 17.145461336738798 ), ( 1566333369000, 14.00578964596659 ), ( 1566333669000, 7.162118769712023 ), ( 1566333969000, 5.572240768882027 ), ( 1566334269000, 5.225784605104701 ), ( 1566334569000, 12.052239963899162 ), ( 1566334869000, 17.76552733742874 ), ( 1566335169000, 18.5940417039428 ), ( 1566335469000, 10.905957340765351 ), ( 1566335769000, 19.687883923025822 ), ( 1566336069000, 25.004497361416835 ), ( 1566336369000, 34.08813957287924 ), ( 1566336669000, 26.08862110071452 ), ( 1566336969000, 20.393746491543567 ), ( 1566337269000, 14.748818924168617 ), ( 1566337569000, 22.308754721808747 ), ( 1566337869000, 19.34967723391255 ), ( 1566338169000, 19.510566032935888 ), ( 1566338469000, 20.09624854534548 ), ( 1566338769000, 14.381292268409098 ), ( 1566339069000, 5.323030481311783 ), ( 1566339369000, 0.22908293991497786 ), ( 1566339669000, 9.481397857124655 ), ( 1566339969000, 17.60423212502989 ), ( 1566340269000, 19.351733959063026 ), ( 1566340569000, 11.287736553018902 ), ( 1566340869000, 17.7859973842938 ), ( 1566341169000, 23.893612698937748 ), ( 1566341469000, 17.690989604869333 ), ( 1566341769000, 9.959437927249034 ), ( 1566342069000, 3.6753232203630297 ), ( 1566342369000, 9.33161645796841 ), ( 1566342669000, 13.38763452096049 ), ( 1566342969000, 5.245845947757765 ), ( 1566343269000, 7.434690571063857 ), ( 1566343569000, 9.997209946357971 ), ( 1566343869000, 5.050794098148275 ), ( 1566344169000, -0.2602705192075767 ), ( 1566344469000, -6.400127162699704 ), ( 1566344769000, -3.766461822059463 ), ( 1566345069000, -0.4378409408076047 ), ( 1566345369000, 1.957670763901274 ), ( 1566345669000, -2.1598599744123907 ), ( 1566345969000, 3.814018150869013 ) ]
