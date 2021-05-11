module ZipList exposing
    ( ZipList
    , append
    , current
    , decoder
    , encode
    , filter
    , get
    , map
    , member
    , removeCurrent
    , select
    , setCurrent
    , singleton
    , toList
    , toListWithPosition
    )

import Html exposing (a)
import Json.Decode as Decode
import Json.Encode as Encode
import List as L


type ZipList a
    = ZipList (Data a)


type alias Data a =
    { before_ : List a
    , current_ : a
    , after_ : List a
    }


singleton : a -> ZipList a
singleton a =
    ZipList (Data [] a [])


get : (a -> Bool) -> ZipList a -> Maybe a
get condition list =
    toList list
        |> List.filter condition
        |> List.head


map : (a -> b) -> ZipList a -> ZipList b
map f (ZipList { before_, current_, after_ }) =
    ZipList (Data (L.map f before_) (f current_) (L.map f after_))


type alias PositionTags a b =
    { before : a -> b
    , current : a -> b
    , after : a -> b
    }


toList : ZipList a -> List a
toList (ZipList { before_, current_, after_ }) =
    L.concat [ before_, [ current_ ], after_ ]


toListWithPosition : PositionTags a b -> ZipList a -> List b
toListWithPosition tag (ZipList { before_, current_, after_ }) =
    L.concat
        [ L.map tag.before before_
        , [ tag.current current_ ]
        , L.map tag.after after_
        ]


{-| Move `current` to the first item the matches the given condition.
-}
select : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
select f (ZipList { before_, current_, after_ }) =
    let
        process item ( localBefore, localCurrent, localAfter ) =
            if f item then
                -- found the new localCurrent item.
                ( localBefore, Just item, localAfter )

            else
                case localCurrent of
                    Just _ ->
                        -- the new localCurrent item has been found, append to after_
                        ( localBefore, localCurrent, L.append localAfter [ item ] )

                    Nothing ->
                        -- the new localCurrent item has not been found yet, append to before_
                        ( L.append localBefore [ item ], localCurrent, localAfter )

        ( newBefore, maybeCurrent, newAfter ) =
            L.foldl process
                (L.foldl process ( [], Nothing, [] ) before_)
                (current_ :: after_)
    in
    maybeCurrent
        |> Maybe.map (\newCurrent -> ZipList (Data newBefore newCurrent newAfter))


member : a -> ZipList a -> Bool
member a (ZipList { before_, current_, after_ }) =
    L.member a before_
        || L.member a after_
        || (a == current_)


append : List a -> ZipList a -> ZipList a
append a (ZipList data) =
    ZipList { data | after_ = L.append data.after_ a }


filter : (a -> Bool) -> ZipList a -> Maybe (ZipList a)
filter f (ZipList { before_, current_, after_ }) =
    if not (f current_) then
        -- cannot remove the current_ item
        Nothing

    else
        Just <|
            ZipList
                { current_ = current_
                , before_ = L.filter f before_
                , after_ = L.filter f after_
                }


current : ZipList a -> a
current (ZipList data) =
    data.current_


setCurrent : a -> ZipList a -> ZipList a
setCurrent a (ZipList data) =
    ZipList { data | current_ = a }


removeCurrent : ZipList a -> Maybe (ZipList a)
removeCurrent (ZipList { before_, after_ }) =
    case ( before_, after_ ) of
        ( _, head :: tail ) ->
            Just (ZipList (Data before_ head tail))

        ( head :: tail, _ ) ->
            Just (ZipList (Data tail head []))

        _ ->
            Nothing



-- JSON


jsonKeys : { before : String, current : String, after : String }
jsonKeys =
    { before = "before"
    , current = "current"
    , after = "after"
    }


decoder : Decode.Decoder a -> Decode.Decoder (ZipList a)
decoder itemDecoder =
    Decode.map ZipList (dataDecoder itemDecoder)


dataDecoder : Decode.Decoder a -> Decode.Decoder (Data a)
dataDecoder itemDecoder =
    Decode.map3 Data
        (Decode.field jsonKeys.before (Decode.list itemDecoder))
        (Decode.field jsonKeys.current itemDecoder)
        (Decode.field jsonKeys.after (Decode.list itemDecoder))


encode : (a -> Encode.Value) -> ZipList a -> Encode.Value
encode encodeItem (ZipList { before_, current_, after_ }) =
    Encode.object
        [ ( jsonKeys.before, Encode.list encodeItem before_ )
        , ( jsonKeys.current, encodeItem current_ )
        , ( jsonKeys.after, Encode.list encodeItem after_ )
        ]
