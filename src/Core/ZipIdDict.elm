module Core.ZipIdDict exposing
    ( Id
    , ZipIdDict
    , decoder
    , encode
    , get
    , getCurrentId
    , getCurrentItem
    , getNextItem
    , insert
    , map
    , mapState
    , merge
    , remove
    , select
    , setCurrentItem
    , singleton
    , toIdDict
    , zipMap
    )

import Core.IdDict as IdDict exposing (IdDict)
import Json.Decode
import Json.Encode


type alias Id =
    IdDict.Id


type ZipIdDict a
    = ZD Id a (IdDict a)


get : Id -> ZipIdDict v -> Maybe v
get id (ZD curId curItem dict) =
    if id == curId then
        Just curItem

    else
        IdDict.get id dict


getCurrentId : ZipIdDict v -> Id
getCurrentId (ZD curId _ _) =
    curId


getCurrentItem : ZipIdDict v -> v
getCurrentItem (ZD _ curItem _) =
    curItem


getNextItem : (v -> v -> Order) -> ZipIdDict v -> Maybe v
getNextItem compare (ZD _ v dict) =
    IdDict.toList dict
        |> List.map Tuple.second
        |> List.filter (compare v >> (==) LT)
        |> List.sortWith compare
        |> List.head


setCurrentItem : v -> ZipIdDict v -> ZipIdDict v
setCurrentItem item (ZD curId _ dict) =
    ZD curId item dict


singleton : Id -> v -> ZipIdDict v
singleton id v =
    ZD id v IdDict.empty


insert : Id -> v -> ZipIdDict v -> ZipIdDict v
insert id v (ZD curId curItem dict) =
    ZD id v (IdDict.insert curId curItem dict |> IdDict.remove id)


remove : Id -> ZipIdDict v -> ZipIdDict v
remove id (ZD curId curItem dict) =
    if id == curId then
        case IdDict.toList dict of
            [] ->
                ZD curId curItem dict

            ( newCurId, newCurItem ) :: rest ->
                ZD newCurId newCurItem (IdDict.fromList rest)

    else
        ZD curId curItem (IdDict.remove id dict)


map : (Id -> v -> b) -> ZipIdDict v -> ZipIdDict b
map func (ZD curId curItem dict) =
    ZD curId (func curId curItem) (IdDict.map func dict)


mapState : (s -> Id -> a -> ( s, a )) -> s -> ZipIdDict a -> ( s, ZipIdDict a )
mapState func state (ZD curId curItem dict) =
    let
        ( stateAfterCur, newCurItem ) =
            func state curId curItem

        ( finalState, newDict ) =
            IdDict.mapState func stateAfterCur dict
    in
    ( finalState
    , ZD curId newCurItem newDict
    )


select : Id -> ZipIdDict v -> Maybe (ZipIdDict v)
select id (ZD curId curItem dict) =
    if id == curId then
        Just (ZD curId curItem dict)

    else
        IdDict.get id dict
            |> Maybe.map
                (\item ->
                    ZD id item (dict |> IdDict.remove id |> IdDict.insert curId curItem)
                )


zipMap : (a -> a -> Order) -> (ZipIdDict a -> Bool -> b) -> ZipIdDict a -> List b
zipMap compare f ((ZD curId curItem dict) as current) =
    let
        dictWithCurrentItem =
            IdDict.insert curId curItem dict

        zipItem ( id, item ) =
            ( ZD id item (IdDict.remove id dictWithCurrentItem), False )

        zipped =
            ( current, True ) :: (IdDict.toList dict |> List.map zipItem)

        compareZippedItem ( ZD _ a _, _ ) ( ZD _ b _, _ ) =
            compare a b
    in
    zipped
        |> List.sortWith compareZippedItem
        |> List.map (\( d, isCurrent ) -> f d isCurrent)


merge :
    (Id -> a -> result -> result)
    -> (Id -> a -> b -> result -> result)
    -> (Id -> b -> result -> result)
    -> ZipIdDict a
    -> ZipIdDict b
    -> result
    -> result
merge leftStep bothStep rightStep (ZD lCurId lCurV left) (ZD rCurId rCurV right) initialResult =
    IdDict.merge leftStep
        bothStep
        rightStep
        (IdDict.insert lCurId lCurV left)
        (IdDict.insert rCurId rCurV right)
        initialResult


toIdDict : ZipIdDict a -> IdDict a
toIdDict (ZD curId curItem dict) =
    IdDict.insert curId curItem dict



-- JSON


encode : (a -> Json.Encode.Value) -> ZipIdDict a -> Json.Encode.Value
encode encodeItem (ZD curId curItem items) =
    Json.Encode.object
        [ ( "currentId", IdDict.encodeId curId )
        , ( "currentItem", encodeItem curItem )
        , ( "items", IdDict.encode encodeItem items )
        ]


decoder : Json.Decode.Decoder a -> Json.Decode.Decoder (ZipIdDict a)
decoder itemDecoder =
    Json.Decode.map3 ZD
        (Json.Decode.field "currentId" IdDict.idDecoder)
        (Json.Decode.field "currentItem" itemDecoder)
        (Json.Decode.field "items" (IdDict.decoder itemDecoder))
