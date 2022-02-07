module Core.DictDiff exposing
    ( Error
    , ValueDiff
    , applyDiff
    , diffValueDecoder
    , encodeDiffValue
    , makeDiff
    , revert
    )

import DecodeHelpers exposing (switch)
import Json.Decode as D
import Json.Encode as E


type ValueDiff value subDiff
    = Added value
    | Removed value
    | Changed subDiff


type Error e
    = Error String
    | SubError e


type alias Merge k lv rv left right result =
    (k -> lv -> result -> result)
    -> (k -> lv -> rv -> result -> result)
    -> (k -> rv -> result -> result)
    -> left
    -> right
    -> result
    -> result


type alias Insert k v d =
    k -> v -> d -> d


makeDiff :
    diff
    -> Insert key (ValueDiff value subDiff) diff
    -> Merge key value value dict dict diff
    -> (value -> value -> subDiff)
    -> dict
    -> dict
    -> diff
makeDiff emptyDiff insertInDiff mergeDict makeSubItemDiff new old =
    let
        onAdded key value result =
            insertInDiff key (Added value) result

        onChanged key newItem oldItem result =
            if newItem == oldItem then
                result

            else
                insertInDiff key (Changed <| makeSubItemDiff newItem oldItem) result

        onRemoved key value result =
            insertInDiff key (Removed value) result
    in
    mergeDict onAdded onChanged onRemoved new old emptyDiff


applyDiff :
    Merge key (ValueDiff value subDiff) value diff dict (Result (Error subError) dict)
    -> Insert key value dict
    -> (key -> dict -> dict)
    -> (subDiff -> value -> Result subError value)
    -> diff
    -> dict
    -> Result (Error subError) dict
applyDiff merge insert remove applySubDiff diff dict =
    let
        error str =
            Err (Error str)

        fromDiff key diffItem result =
            case diffItem of
                Added value ->
                    Result.map (insert key value) result

                Changed _ ->
                    error "Cannot modify an value that does not exist"

                Removed _ ->
                    error "Cannot remove an value that does not exist"

        mergeItemAndInsert key valueDiff value d =
            applySubDiff valueDiff value
                |> Result.mapError SubError
                |> Result.map (\i -> insert key i d)

        inBoth key diffItem value result =
            case diffItem of
                Added _ ->
                    error "Cannot add an value twice"

                Changed valueDiff ->
                    result |> Result.andThen (mergeItemAndInsert key valueDiff value)

                Removed _ ->
                    result |> Result.map (remove key)

        unchanged _ _ result =
            result
    in
    merge fromDiff inBoth unchanged diff dict (Ok dict)


revert :
    ((key -> ValueDiff v subDiff -> ValueDiff v subDiff) -> diff -> diff)
    -> (subDiff -> subDiff)
    -> diff
    -> diff
revert map revertSubDiff diff =
    let
        revertDiffValue _ diffValue =
            case diffValue of
                Added v ->
                    Removed v

                Removed v ->
                    Added v

                Changed subDiff ->
                    Changed (revertSubDiff subDiff)
    in
    map revertDiffValue diff



-- JSON


encodeDiffValue : (v -> E.Value) -> (subDiff -> E.Value) -> ValueDiff v subDiff -> E.Value
encodeDiffValue encodeValue encodeSubDiff dv =
    case dv of
        Added v ->
            E.object
                [ ( "type", E.string "added" )
                , ( "value", encodeValue v )
                ]

        Removed v ->
            E.object
                [ ( "type", E.string "removed" )
                , ( "value", encodeValue v )
                ]

        Changed sub ->
            E.object
                [ ( "type", E.string "changed" )
                , ( "value", encodeSubDiff sub )
                ]


diffValueDecoder : D.Decoder v -> D.Decoder subDiff -> D.Decoder (ValueDiff v subDiff)
diffValueDecoder valueDecoder subDiffDecoder =
    D.field "type" D.string
        |> switch "this type does not exist"
            [ ( "added", D.field "value" valueDecoder |> D.map Added )
            , ( "removed", D.field "value" valueDecoder |> D.map Removed )
            , ( "changed", D.field "value" subDiffDecoder |> D.map Changed )
            ]
