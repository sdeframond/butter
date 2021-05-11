module Name exposing
    ( Name
    , Store
    , decoder
    , empty
    , encode
    , encodeStore
    , fromCoord
    , fromList
    , fromSheetId
    , fromString
    , get
    , insert
    , matchesString
    , member
    , parser
    , remove
    , removeString
    , storeDecoder
    , toString
    )

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Parser as P exposing ((|.), (|=))
import PositiveInt exposing (PositiveInt)
import Set


type Name
    = Name String


type Store a
    = Store (Dict String a)


fromString : String -> Maybe Name
fromString input =
    let
        parser_ =
            P.succeed identity
                |. P.spaces
                |= parser
                |. P.spaces
                |. P.end
    in
    P.run parser_ input |> Result.toMaybe


fromSheetId : PositiveInt -> Name
fromSheetId i =
    Name ("Sheet" ++ PositiveInt.toString i)


fromCoord : PositiveInt -> PositiveInt -> Name
fromCoord x y =
    Name (PositiveInt.toLetters x ++ PositiveInt.toString y)


parser : P.Parser Name
parser =
    P.succeed Name
        |= P.variable
            { start = Char.isAlpha
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }


toString : Name -> String
toString (Name str) =
    str


matchesString : String -> Name -> Bool
matchesString str1 (Name str2) =
    str1 == str2



-- STORE


empty : Store a
empty =
    Store Dict.empty


get : Name -> Store a -> Maybe a
get name (Store d) =
    Dict.get (toString name) d


fromList : List ( Name, a ) -> Store a
fromList list =
    list
        |> List.map (Tuple.mapFirst toString)
        |> Dict.fromList
        |> Store


insert : Name -> a -> Store a -> Store a
insert name value (Store d) =
    Dict.insert (toString name) value d
        |> Store


remove : Name -> Store a -> Store a
remove name =
    removeString (toString name)


removeString : String -> Store a -> Store a
removeString str (Store d) =
    Store (Dict.remove str d)


member : Name -> Store a -> Bool
member name (Store d) =
    Dict.member (toString name) d



-- JSON


decoder : Decode.Decoder Name
decoder =
    let
        parse input =
            fromString input
                |> Maybe.map Decode.succeed
                |> Maybe.withDefault (Decode.fail <| "Invalid name: " ++ input)
    in
    Decode.string |> Decode.andThen parse


storeDecoder : Decode.Decoder a -> Decode.Decoder (Store a)
storeDecoder itemDecoder =
    let
        checkKeys dict =
            Dict.keys dict
                |> List.map fromString
                |> (\names ->
                        if List.any ((==) Nothing) names then
                            Decode.fail "All names must be valid"

                        else
                            Decode.succeed dict
                   )
    in
    Decode.map Store (Decode.dict itemDecoder |> Decode.andThen checkKeys)


encode : Name -> Encode.Value
encode (Name str) =
    Encode.string str


encodeStore : (a -> Encode.Value) -> Store a -> Encode.Value
encodeStore encodeItem (Store dict) =
    Encode.dict identity encodeItem dict
