module ZipList exposing
    ( ZipList
    , append
    , current
    , decoder
    , encode
    , filter
    , get
    , map
    , mapState
    , member
    , select
    , setCurrent
    , singleton
    , toList
    , zipMap
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


mapState : (s -> a -> ( s, a )) -> s -> ZipList a -> ( s, ZipList a )
mapState f state (ZipList { before_, current_, after_ }) =
    let
        wrappedF : a -> ( s, List a ) -> ( s, List a )
        wrappedF item ( s, list ) =
            f s item
                |> Tuple.mapSecond (\newItem -> newItem :: list)

        ( stateAfterBefore, newBefore ) =
            List.foldr wrappedF ( state, [] ) before_

        ( stateAfterCurrent, newCurrent ) =
            f stateAfterBefore current_

        ( finalState, newAfter ) =
            List.foldr wrappedF ( stateAfterCurrent, [] ) after_
    in
    ( finalState
    , ZipList
        { before_ = newBefore
        , current_ = newCurrent
        , after_ = newAfter
        }
    )


zipMap : (ZipList a -> Bool -> b) -> ZipList a -> List b
zipMap f zl =
    let
        go : List a -> a -> List a -> List b
        go before current_ after =
            let
                currentZL =
                    ZipList (Data before current_ after)
            in
            case after of
                [] ->
                    [ f currentZL (currentZL == zl) ]

                head :: rest ->
                    f currentZL (currentZL == zl) :: go (current_ :: before) head rest
    in
    case toList zl of
        [] ->
            []

        head :: rest ->
            go [] head rest


toList : ZipList a -> List a
toList (ZipList { before_, current_, after_ }) =
    L.concat [ List.reverse before_, [ current_ ], after_ ]


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
                        ( item :: localBefore, localCurrent, localAfter )

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
    let
        new =
            ZipList
                { current_ = current_
                , before_ = L.filter f before_
                , after_ = L.filter f after_
                }
    in
    if not <| f current_ then
        removeCurrent new

    else
        Just new


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
