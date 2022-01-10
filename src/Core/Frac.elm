module Core.Frac exposing
    ( Frac
    , between
    , compare
    , decoder
    , encode
    , init
    , max
    )

import BigRational as BR exposing (BigRational)
import Json.Decode
import Json.Encode


type Frac
    = Frac BigRational


init : Frac
init =
    Frac (BR.fromInt 0)


max : Frac
max =
    Frac (BR.fromInt 1)


between : Frac -> Frac -> Frac
between (Frac x) (Frac y) =
    Frac (BR.div (BR.add x y) (BR.fromInt 2))


compare : Frac -> Frac -> Order
compare (Frac x) (Frac y) =
    BR.compare x y



-- JSON


encode : Frac -> Json.Encode.Value
encode (Frac x) =
    Json.Encode.string (BR.toString x)


decoder : Json.Decode.Decoder Frac
decoder =
    let
        stringToBigRational str =
            BR.fromString str
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail ("Unable to parse: " ++ str))
    in
    Json.Decode.string
        |> Json.Decode.andThen stringToBigRational
        |> Json.Decode.map Frac
