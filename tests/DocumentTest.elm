module DocumentTest exposing (suite)

import Document
import Expect
import Fuzz exposing (..)
import Grid
import Json.Decode as Decode
import MyTable
import Name
import PositiveInt exposing (PositiveInt)
import Sheet
import Test exposing (..)
import Types


positiveInt : Fuzzer PositiveInt
positiveInt =
    PositiveInt.range 100
        |> List.map constant
        |> oneOf


name : Fuzzer Name.Name
name =
    map2 Name.fromCoord positiveInt positiveInt


gridMsg : Fuzzer Grid.Msg
gridMsg =
    oneOf
        [ map Grid.StartEditing name
        , map Grid.UpdateEdit string
        , constant Grid.OnClickCellTypeBtn
        , constant Grid.OnClickCellDataTypeBtn
        ]


sheetIds : Fuzzer (Name.Store Types.SheetId)
sheetIds =
    tuple ( name, positiveInt )
        |> list
        |> map Name.fromList


grid : Fuzzer Grid.Grid
grid =
    let
        f ids =
            List.foldl (Grid.update (\n -> Name.get n ids)) Grid.init
    in
    map2 f sheetIds (list gridMsg)


tableMsg : Fuzzer MyTable.Msg
tableMsg =
    oneOf
        [ -- SetTableState T.State
          map2 MyTable.UpdateNewRowField name string
        , map MyTable.KeyDown int
        , map3 MyTable.EditCell positiveInt name string
        , map MyTable.UpdateEditedCell string
        , constant MyTable.OnClickAddFieldBtn
        , map MyTable.OnInputNewFieldName string
        , map MyTable.OnClickRemoveColumnBtn string
        , constant MyTable.OnClickNewFieldTypeBtn
        , constant MyTable.OnClickNewFieldDataTypeBtn
        , map MyTable.OnInputNewFieldFormula string
        ]


table : Fuzzer Types.Table
table =
    map2 Types.Table (list name) (constant [])


sheetMsg : Fuzzer Sheet.Msg
sheetMsg =
    oneOf
        [ map Sheet.GridMsg gridMsg
        , map Sheet.TableMsg tableMsg

        -- , map Sheet.PivotTableMsg pivotTableMsg
        ]


initSheet : Fuzzer Sheet.Sheet
initSheet =
    oneOf
        [ map Sheet.initGrid name
        , map2 Sheet.initPivotTable name table
        , map Sheet.initTable name
        ]


documentMsg : Fuzzer Document.Msg
documentMsg =
    oneOf
        [ map Document.SheetMsg sheetMsg
        , map Document.InsertSheet initSheet
        , constant Document.InsertGridSheet
        , constant Document.InsertTableSheet
        , map Document.SelectSheet positiveInt
        , map Document.RemoveSheet positiveInt
        , tuple ( positiveInt, name ) |> map Document.EditSheet
        , map Document.UpdateSheetName string
        ]


document : Fuzzer Document.Model
document =
    let
        f =
            Document.init
                |> List.foldl (\msg model -> Document.update msg model |> Tuple.first)
    in
    map f (list documentMsg)


suite : Test
suite =
    describe "Document"
        [ fuzz document "JSON Encode/Decode" <|
            \doc ->
                let
                    commited =
                        Document.update (Document.SelectSheet PositiveInt.one) doc
                            |> Tuple.first
                in
                Document.encode commited
                    |> Decode.decodeValue Document.decoder
                    |> Expect.equal (Ok commited)
        ]
