module PositiveInt exposing
    ( PositiveInt
    , next
    , one
    , range
    , toLetters
    , toString
    )


type PositiveInt
    = PositiveInt Int


one : PositiveInt
one =
    PositiveInt 1


next : PositiveInt -> PositiveInt
next (PositiveInt i) =
    PositiveInt (i + 1)


toString : PositiveInt -> String
toString (PositiveInt i) =
    String.fromInt i


toLetters : PositiveInt -> String
toLetters (PositiveInt i) =
    Char.fromCode (i - 1 + Char.toCode 'A')
        |> String.fromChar


range : Int -> List PositiveInt
range i =
    List.range 1 i |> List.map PositiveInt
