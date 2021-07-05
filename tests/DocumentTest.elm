module DocumentTest exposing (suite)

import Document
import Expect
import Fuzz exposing (..)
import Grid
import Json.Decode as Decode
import MyTable
import Name
import NamedAndOrderedStore
import PositiveInt exposing (PositiveInt)
import Sheet
import Test exposing (..)
import Types


positiveIntFuzzer : Fuzzer PositiveInt
positiveIntFuzzer =
    PositiveInt.range 100
        |> List.map constant
        |> oneOf


nameFuzzer : Fuzzer Name.Name
nameFuzzer =
    map2 Name.fromCoord positiveIntFuzzer positiveIntFuzzer


gridMsgFuzzer : Fuzzer Grid.Msg
gridMsgFuzzer =
    oneOf
        [ map Grid.StartEditing nameFuzzer
        , map Grid.UpdateEdit string
        , constant Grid.OnClickCellTypeBtn
        , constant Grid.OnClickCellDataTypeBtn
        ]


tableMsgFuzzer : Fuzzer MyTable.Msg
tableMsgFuzzer =
    oneOf
        [ -- SetTableState T.State
          map2 MyTable.UpdateNewRowField nameFuzzer string
        , map MyTable.KeyDown int
        , map3 MyTable.EditCell positiveIntFuzzer nameFuzzer string
        , map MyTable.UpdateEditedCell string
        , constant MyTable.OnClickAddFieldBtn
        , map MyTable.OnInputNewFieldName string
        , map MyTable.OnClickRemoveColumnBtn string
        , constant MyTable.OnClickNewFieldTypeBtn
        , constant MyTable.OnClickNewFieldDataTypeBtn
        , map MyTable.OnInputNewFieldFormula string
        ]


tableFuzzer : Fuzzer Types.Table
tableFuzzer =
    map2 Types.Table
        (list nameFuzzer)
        -- TODO write a real fuzzer here
        (constant [])


sheetMsgFuzzer : Fuzzer Sheet.Msg
sheetMsgFuzzer =
    oneOf
        [ map Sheet.GridMsg gridMsgFuzzer
        , map Sheet.TableMsg tableMsgFuzzer

        -- Not fuzzing this because all messages are from DnDList
        -- , map Sheet.PivotTableMsg pivotTableMsg
        ]


sheetParamsFuzzer : Fuzzer Sheet.Params
sheetParamsFuzzer =
    oneOf
        [ constant Sheet.allParams.grid
        , constant Sheet.allParams.table
        , map Sheet.allParams.pivotTable tableFuzzer
        ]


documentMsgFuzzer : Fuzzer Document.Msg
documentMsgFuzzer =
    oneOf
        [ map Document.SheetMsg sheetMsgFuzzer
        , map Document.InsertSheet sheetParamsFuzzer
        , map Document.SelectSheet positiveIntFuzzer
        , map Document.RemoveSheet positiveIntFuzzer
        , constant Document.EditSheet
        , map Document.UpdateSheetName string
        ]


documentFuzzer : Fuzzer Document.Model
documentFuzzer =
    let
        f =
            processMsgList Document.init
    in
    map f (list documentMsgFuzzer)


processMsgList : Document.Model -> List Document.Msg -> Document.Model
processMsgList doc =
    List.foldl (\msg model -> Document.update msg model |> Tuple.first) doc


suite : Test
suite =
    describe "Document"
        [ fuzz documentFuzzer "JSON Encode/Decode" <|
            \doc ->
                let
                    commited =
                        Document.update (Document.SelectSheet PositiveInt.one) doc
                            |> Tuple.first
                in
                Document.encode commited
                    |> Decode.decodeValue Document.decoder
                    |> Expect.equal (Ok commited)
        , fuzz (tuple3 ( nameFuzzer, documentFuzzer, sheetMsgFuzzer ))
            "when renaming a sheet with a valid name, focusing inside a sheet commits current edition"
          <|
            \( newName, doc, sheetMsg ) ->
                [ Document.EditSheet
                , Document.UpdateSheetName <| Name.toString newName
                , Document.SheetMsg sheetMsg
                ]
                    |> processMsgList doc
                    |> Expect.all
                        [ NamedAndOrderedStore.currentName >> Expect.equal newName
                        , NamedAndOrderedStore.isEditing >> Expect.false "Expected editing to be false"
                        ]
        , fuzz (tuple ( nameFuzzer, documentFuzzer ))
            "when renaming a sheet with a valid name, selecting a sheet commits current edition"
          <|
            \( newName, doc ) ->
                [ Document.EditSheet
                , Document.UpdateSheetName <| Name.toString newName
                , Document.SelectSheet <| NamedAndOrderedStore.currentId doc
                ]
                    |> processMsgList doc
                    |> Expect.all
                        [ NamedAndOrderedStore.currentName >> Expect.equal newName
                        , NamedAndOrderedStore.isEditing >> Expect.false "Expected editing to be false"
                        ]
        , fuzz (tuple ( documentFuzzer, sheetMsgFuzzer ))
            "when renaming a sheet with an invalid name, focusing inside a sheet cancels current edition"
          <|
            \( doc, sheetMsg ) ->
                [ Document.EditSheet
                , Document.UpdateSheetName "not a valid name"
                , Document.SheetMsg sheetMsg
                ]
                    |> processMsgList doc
                    |> Expect.all
                        [ NamedAndOrderedStore.currentName >> Expect.equal (NamedAndOrderedStore.currentName doc)
                        , NamedAndOrderedStore.isEditing >> Expect.false "Expected editing to be false"
                        ]
        , fuzz documentFuzzer
            "when renaming a sheet with an invalid name, renaming another sheet cancels current edition"
          <|
            \doc ->
                [ Document.EditSheet
                , Document.UpdateSheetName "not a valid name"
                , Document.SelectSheet <| NamedAndOrderedStore.currentId doc
                ]
                    |> processMsgList doc
                    |> Expect.all
                        [ NamedAndOrderedStore.currentName >> Expect.equal (NamedAndOrderedStore.currentName doc)
                        , NamedAndOrderedStore.isEditing >> Expect.false "Expected editing to be false"
                        ]
        ]
