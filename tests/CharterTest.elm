module CharterTest exposing (all)

import Charter
import Charter.Extras as CE
import Expect
import Test as T exposing (describe, test)


data : List ( number, number )
data =
    [ ( 1, 0 )
    , ( 2, 4 )
    , ( 3, 0 )
    , ( 3, 1 )
    ]


line : Charter.Element a
line =
    Charter.line [] data


all : T.Test
all =
    let
        els =
            [ Charter.line [] [ ( 1, 2 ), ( 2, 3 ) ]
            , Charter.line [] [ ( 4, 3 ), ( 5, 4 ) ]
            ]

        box =
            Charter.Box 0 0 100 100

        layer =
            Charter.layer box els

        range =
            Charter.include [ ( Just 1, Just 2 ), ( Just 10, Just 17 ) ]
    in
    describe "all"
        [ describe "Layer"
            [ test "getDomain"
                (\() -> Expect.equal (Charter.Domain ( ( 1, 2 ), ( 5, 4 ) )) (Charter.getDomain layer))
            , test "getDomain from a layer with only an include"
                (\() -> Expect.equal (Charter.Domain ( ( 1, 2 ), ( 10, 17 ) )) (Charter.getDomain (Charter.layer box [ range ])))
            , test "getDomain from a layer with a partial include"
                (\() ->
                    Expect.equal (Charter.Domain ( ( 10, -2 ), ( 10, 2 ) ))
                        (Charter.getDomain
                            (Charter.layer box
                                [ Charter.include [ ( Nothing, Just -2 ), ( Just 10, Just 2 ) ]
                                ]
                            )
                        )
                )
            , test "include"
                (\() ->
                    Expect.equal (Charter.Domain ( ( 0, 2 ), ( 5, 10 ) ))
                        (Charter.layer box (Charter.include [ ( Just 0, Just 2 ), ( Nothing, Just 10 ) ] :: els) |> Charter.getDomain)
                )
            ]
        , test "extents for a filled list " <|
            \() ->
                Expect.equal
                    (Just ( ( 1, 0 ), ( 3, 4 ) ))
                    (Charter.extents [ line ])
        , test "extents for an empty list " <|
            \() ->
                Expect.equal
                    Nothing
                    (Charter.extents [])
        ]
