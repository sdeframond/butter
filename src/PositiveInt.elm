module PositiveInt exposing
    ( PositiveInt
    , add
    , decoder
    , encode
    , mul
    , next
    , one
    , range
    , ten
    , toLetters
    , toString
    )

import Json.Decode as Decode
import Json.Encode as Encode


type PositiveInt
    = PositiveInt Int


one : PositiveInt
one =
    PositiveInt 1


ten : PositiveInt
ten =
    PositiveInt 10


mul : PositiveInt -> PositiveInt -> PositiveInt
mul (PositiveInt x) (PositiveInt y) =
    PositiveInt (x * y)


add : PositiveInt -> PositiveInt -> PositiveInt
add (PositiveInt x) (PositiveInt y) =
    PositiveInt (x + y)


next : PositiveInt -> PositiveInt
next (PositiveInt i) =
    PositiveInt (i + 1)


toString : PositiveInt -> String
toString (PositiveInt i) =
    String.fromInt i


toLetters : PositiveInt -> String
toLetters (PositiveInt i) =
    let
        help list rest =
            if rest == 0 then
                list

            else if modBy 26 rest == 0 then
                help ('Z' :: list) (rest // 26 - 1)

            else
                help
                    ((char <| modBy 26 rest) :: list)
                    (rest // 26)

        char c =
            Char.fromCode (c - 1 + Char.toCode 'A')
    in
    help [] i
        |> String.fromList


range : Int -> List PositiveInt
range i =
    List.range 1 i |> List.map PositiveInt



-- JSON


decoder : Decode.Decoder PositiveInt
decoder =
    let
        checkPositivity i =
            if i > 0 then
                Decode.succeed i

            else
                Decode.fail ("Not positive: " ++ String.fromInt i)
    in
    Decode.map PositiveInt (Decode.int |> Decode.andThen checkPositivity)


encode : PositiveInt -> Encode.Value
encode (PositiveInt i) =
    Encode.int i
