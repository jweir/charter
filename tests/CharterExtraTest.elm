module CharterExtraTest exposing (all)

import Charter exposing (DataSet)
import Charter.Extras as Charter exposing (Axes(..), intersect)
import Expect
import Test as T exposing (describe, test)


stackPoints : List DataSet -> List DataSet
stackPoints x =
    let
        hoist set ( prev, out ) =
            let
                newSet =
                    set
                        |> List.map
                            (\( px, py ) ->
                                case intersect X px prev of
                                    Nothing ->
                                        ( px, py )

                                    Just ( ax, ay ) ->
                                        ( px, py + ay )
                            )

                prevBalanced =
                    prev
                        |> List.map
                            (\( px, py ) ->
                                case intersect X px newSet of
                                    Nothing ->
                                        ( px, py )

                                    Just ( ax, ay ) ->
                                        ( px, py + ay )
                            )

                xx =
                    newSet ++ prevBalanced
            in
            ( xx, out ++ [ xx ] )
    in
    -- for each set after the first
    -- add the previous set's intersect y to the current point
    case x of
        first :: rest ->
            List.foldl
                hoist
                ( first, [ first ] )
                rest
                |> Tuple.second

        a ->
            a


all : T.Test
all =
    describe "all"
        [ describe "intersect"
            [ {-
                   test "a"
                     (\_ ->
                         Expect.equal (Between ( 0, 60 ) ( 10, 30 ))
                             (findIntersect ( 5, 5 ) [ ( 0, 60 ), ( 10, 30 ), ( 20, 10 ) ])
                     )
                 , test "c"
                     (\_ ->
                         Expect.equal (Exact ( 10, 4 ))
                             (findIntersect ( 10, 1 ) [ ( 0, 5 ), ( 10, 4 ) ])
                     )
                     ,
              -}
              test "intersect on the X axis"
                (\_ ->
                    Expect.equal (Just ( 17, 17.5 ))
                        (Charter.intersect Charter.X 17 [ ( 0, 60 ), ( 10, 35 ), ( 20, 10 ), ( 25, 2 ) ])
                )
            , test "intersect on the Y axis"
                (\_ ->
                    Expect.equal (Just ( 12.5, 2.5 ))
                        (Charter.intersect Charter.Y 2.5 [ ( 10, 0 ), ( 15, 5 ), ( 8, 10 ) ])
                )
            , test "with no intersection"
                (\_ ->
                    Expect.equal Nothing
                        (Charter.intersect Charter.X 9 [ ( 10, 0 ), ( 15, 5 ), ( 8, 10 ) ])
                )
            ]
        ]
