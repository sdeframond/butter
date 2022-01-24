module Core.IdDict exposing
    ( Id
    , IdDict
    , decoder
    , empty
    , encode
    , encodeId
    , fromList
    , get
    , idDecoder
    , insert
    , map
    , mapState
    , merge
    , remove
    , toList
    )

import Core.PositiveInt as Id exposing (PositiveInt)
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Result.Extra


type alias Id =
    PositiveInt


type IdDict a
    = IdDict (Dict Int a)


empty : IdDict a
empty =
    IdDict <| Dict.empty


get : Id -> IdDict a -> Maybe a
get id (IdDict d) =
    Dict.get (Id.toInt id) d


insert : Id -> a -> IdDict a -> IdDict a
insert id value (IdDict d) =
    Dict.insert (Id.toInt id) value d |> IdDict


remove : Id -> IdDict a -> IdDict a
remove id (IdDict d) =
    Dict.remove (Id.toInt id) d |> IdDict


map : (Id -> v -> b) -> IdDict v -> IdDict b
map func (IdDict dict) =
    Dict.map (\i -> func (Id.unsafeFromInt i)) dict |> IdDict


mapState : (s -> Id -> a -> ( s, a )) -> s -> IdDict a -> ( s, IdDict a )
mapState func state (IdDict dict) =
    let
        go id val ( s, d ) =
            func s (Id.unsafeFromInt id) val
                |> Tuple.mapSecond (\v -> Dict.insert id v d)
    in
    Dict.foldl go ( state, Dict.empty ) dict
        |> Tuple.mapSecond IdDict


toList : IdDict a -> List ( Id, a )
toList (IdDict dict) =
    Dict.toList dict |> List.map (Tuple.mapFirst Id.unsafeFromInt)


fromList : List ( Id, a ) -> IdDict a
fromList list =
    list |> List.map (Tuple.mapFirst Id.toInt) |> Dict.fromList |> IdDict


merge :
    (Id -> a -> result -> result)
    -> (Id -> a -> b -> result -> result)
    -> (Id -> b -> result -> result)
    -> IdDict a
    -> IdDict b
    -> result
    -> result
merge leftStep bothStep rightStep (IdDict leftDict) (IdDict rightDict) initialResult =
    Dict.merge
        (Id.unsafeFromInt >> leftStep)
        (Id.unsafeFromInt >> bothStep)
        (Id.unsafeFromInt >> rightStep)
        leftDict
        rightDict
        initialResult



-- JSON


encodeId : PositiveInt -> Json.Encode.Value
encodeId =
    Id.encode


encode : (a -> Json.Encode.Value) -> IdDict a -> Json.Encode.Value
encode encodeItem (IdDict dict) =
    Json.Encode.dict String.fromInt encodeItem dict


idDecoder : Json.Decode.Decoder Id
idDecoder =
    Id.decoder


decoder : Json.Decode.Decoder v -> Json.Decode.Decoder (IdDict v)
decoder itemDecoder =
    let
        firstToInt ( str, v ) =
            String.toInt str
                |> Result.fromMaybe (str ++ " is not and integer")
                |> Result.map (\i -> ( i, v ))

        mapKeys =
            Result.Extra.combineMap firstToInt
                >> Result.map (Dict.fromList >> IdDict)

        toIdDict l =
            case mapKeys l of
                Err msg ->
                    Json.Decode.fail msg

                Ok d ->
                    Json.Decode.succeed d
    in
    Json.Decode.keyValuePairs itemDecoder
        |> Json.Decode.andThen toIdDict
