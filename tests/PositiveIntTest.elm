module PositiveIntTest exposing (..)

import Core.PositiveInt exposing (..)
import Helpers
import Test exposing (..)


i26 : PositiveInt
i26 =
    one
        |> next
        |> mul ten
        |> next
        |> next
        |> next
        |> next
        |> next
        |> next


suite : Test
suite =
    Helpers.testCollection "PositiveInt.toLetters"
        toLetters
        [ ( "1 = A", one, "A" )
        , ( "26 = Z", i26, "Z" )
        , ( "26 + 1 = AA", i26 |> next, "AA" )
        , ( "26 + 26 = AZ", add i26 i26, "AZ" )
        , ( "26*26 + 1 = ZA", mul i26 i26 |> next, "ZA" )
        , ( "26*26 + 26 = ZZ", mul i26 i26 |> add i26, "ZZ" )
        , ( "26*26 + 26 + 1 = AAA", mul i26 i26 |> add i26 |> next, "AAA" )
        ]
