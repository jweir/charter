module CharterTest exposing (all)

import Charter
import Expect
import Test as T


data =
    [ ( 1, 0 )
    , ( 2, 4 )
    , ( 3, 0 )
    , ( 3, 1 )
    ]


line =
    Charter.line [] data


all : T.Test
all =
    T.test "extents produces " <|
        \() ->
            Expect.equal
                (Just ( ( 1, 0 ), ( 3, 4 ) ))
                (Charter.extents [ line ])
