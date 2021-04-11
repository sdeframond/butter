module Document exposing
    ( Document
    , Msg
    , Position(..)
    , Sheet(..)
    , cellSource
    , commitEdit
    , get
    , gridSheet
    , init
    ,  insert
       -- Not used but useful for testing.
       -- TODO: find a way to test without it.

    , insertSheet
    , pivotTableSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetNames
    , subscriptions
    , tableSheet
    , update
    , view
    )

import AST
    exposing
        ( AST(..)
        , BinaryOp(..)
        , FormulaAST(..)
        )
import Cell exposing (Cell)
import Css exposing (..)
import Dict as D exposing (Dict)
import Grid exposing (Grid)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import MyPivotTable exposing (PivotTable)
import MyTable as Table exposing (Table)
import Result as R
import Tuple as T
import Types
import ZipList as ZL exposing (ZipList)


type Document
    = Document DocData


type alias DocData =
    { cells : Dict Types.LocatedName Cell
    , sheets : ZipList SheetItem
    }


type alias SheetItem =
    { name : String
    , sheet : Sheet
    }


type Sheet
    = GridSheet Grid
    | TableSheet Table
    | PivotTableSheet PivotTable


gridSheet : Sheet
gridSheet =
    GridSheet Grid.init


tableSheet : Sheet
tableSheet =
    TableSheet Table.empty


pivotTableSheet : Sheet
pivotTableSheet =
    PivotTableSheet MyPivotTable.empty


subscriptions : Document -> Sub Msg
subscriptions (Document data) =
    let
        { sheet } =
            ZL.current data.sheets
    in
    case sheet of
        PivotTableSheet pt ->
            Sub.map PivotTableMsg (MyPivotTable.subscriptions pt)

        _ ->
            Sub.none


type Msg
    = GridMsg Grid.Msg
    | TableMsg Table.Msg
    | PivotTableMsg MyPivotTable.Msg


update : Msg -> Document -> ( Document, Cmd Msg )
update msg (Document data) =
    updateData msg data |> Tuple.mapBoth Document identity


updateData : Msg -> DocData -> ( DocData, Cmd Msg )
updateData msg data =
    let
        { sheet, name } =
            ZL.current data.sheets

        updateSheet newSheet d =
            { d
                | sheets = ZL.setCurrent (SheetItem name newSheet) d.sheets
            }

        getSource sourceName =
            ZL.toList data.sheets
                |> List.filter (.name >> (==) sourceName)
                |> List.head
                |> R.fromMaybe (Types.UndefinedSheetError sourceName)
                |> R.andThen (.sheet >> evalSheet data)
    in
    case ( msg, sheet ) of
        ( GridMsg gridMsg, GridSheet grid ) ->
            ( updateGrid (Grid.update gridMsg grid) data
            , Cmd.none
            )

        ( TableMsg tableMsg, TableSheet table ) ->
            ( updateSheet (TableSheet <| Table.update tableMsg table) data
            , Cmd.none
            )

        ( PivotTableMsg ptMsg, PivotTableSheet pt ) ->
            let
                ( newPt, cmd ) =
                    MyPivotTable.update getSource ptMsg pt
            in
            ( updateSheet (PivotTableSheet newPt) data
            , Cmd.map PivotTableMsg cmd
            )

        ( _, _ ) ->
            ( data, Cmd.none )


commitEdit : Document -> Document
commitEdit (Document ({ sheets } as data)) =
    Document <|
        case ZL.current sheets |> .sheet of
            GridSheet grid ->
                updateGrid (Grid.commit grid) data

            _ ->
                data


updateGrid : ( Grid, Grid.Cmd ) -> DocData -> DocData
updateGrid ( newGrid, gridCmd ) data =
    let
        newItem =
            SheetItem (ZL.current data.sheets |> .name) (GridSheet newGrid)

        newData =
            { data | sheets = ZL.setCurrent newItem data.sheets }
    in
    case gridCmd of
        Grid.NoCmd ->
            newData

        Grid.CommitChangesCmd cellName content ->
            insertHelp cellName content newData


init : Types.Name -> Sheet -> Document
init name sheet =
    Document
        { cells = D.empty
        , sheets = ZL.singleton (SheetItem name sheet)
        }


type Position a
    = Before a
    | Current a
    | After a


sheetNames : Document -> List (Position Types.Name)
sheetNames (Document { sheets }) =
    ZL.map .name sheets
        |> ZL.toListWithPosition
            { before = Before
            , current = Current
            , after = After
            }


currentSheetName : DocData -> Types.Name
currentSheetName { sheets } =
    ZL.current sheets |> .name


selectSheet : Types.Name -> Document -> Result Types.Error Document
selectSheet selectedName (Document ({ sheets } as data)) =
    ZL.select (.name >> (==) selectedName) sheets
        |> R.fromMaybe (Types.UndefinedSheetError selectedName)
        |> R.map (\newSheets -> Document { data | sheets = newSheets })


sheetExists : Types.Name -> DocData -> Bool
sheetExists name { sheets } =
    ZL.member name (ZL.map .name sheets)


insertSheet : Types.Name -> Sheet -> Document -> Result Types.Error Document
insertSheet name sheet (Document data) =
    if sheetExists name data then
        Err (Types.DuplicateSheetNameError name)

    else
        Ok <|
            Document
                { data
                    | sheets = ZL.append [ SheetItem name sheet ] data.sheets
                }


removeSheet : Types.Name -> Document -> Result Types.Error Document
removeSheet name (Document d) =
    let
        newSheetsRes =
            if name == currentSheetName d then
                ZL.removeCurrent d.sheets
                    |> R.fromMaybe (Types.RemovingLastSheetError name)

            else if sheetExists name d then
                ZL.filter (.name >> (/=) name) d.sheets
                    |> Maybe.withDefault d.sheets
                    |> Ok

            else
                Err (Types.UndefinedSheetError name)
    in
    newSheetsRes
        |> R.map
            (\sheets ->
                Document
                    { d
                        | sheets = sheets
                        , cells = d.cells |> D.filter (\( sheet, _ ) _ -> sheet /= name)
                    }
            )


renameSheet : Types.Name -> Types.Name -> Document -> Result Types.Error Document
renameSheet oldName newName (Document data) =
    let
        updateName parsedName currentName =
            if currentName == oldName then
                parsedName

            else
                currentName

        updateSheetName : (Types.Name -> Types.Name) -> DocData -> DocData
        updateSheetName rename d =
            let
                updateItemName item =
                    { item
                        | name = rename item.name
                        , sheet = updateReferencesInSheet item.sheet
                    }

                updateReferencesInSheet sheet =
                    case sheet of
                        TableSheet table ->
                            TableSheet (Table.updateReferences rename table)

                        GridSheet _ ->
                            sheet

                        PivotTableSheet _ ->
                            Debug.todo "rename references in pivot tables"

                updateCellRefs ( sheetName, cellName ) cell renamed =
                    D.insert ( rename sheetName, cellName ) (Cell.updateReferences rename cell) renamed
            in
            { d
                | sheets = ZL.map updateItemName d.sheets
                , cells = D.foldr updateCellRefs D.empty d.cells
            }
    in
    if sheetExists oldName data then
        if oldName == newName then
            Ok (Document data)

        else if sheetExists newName data then
            Err (Types.DuplicateSheetNameError newName)

        else
            AST.parseName newName
                |> R.mapError (always Types.InvalidSheetNameError)
                |> R.map (updateName >> updateSheetName)
                |> R.map (\updater -> Document (updater data))

    else
        Err <|
            Types.UndefinedSheetError oldName


insert : Types.Name -> String -> Document -> Document
insert cellName value (Document data) =
    Document <| insertHelp cellName value data


insertHelp : Types.Name -> String -> DocData -> DocData
insertHelp cellName value d =
    case value of
        "" ->
            { d | cells = D.remove ( currentSheetName d, cellName ) d.cells }

        _ ->
            { d
                | cells =
                    D.insert ( currentSheetName d, cellName )
                        (Cell.fromSource value)
                        d.cells
            }


getCell : Types.Name -> Types.Name -> DocData -> Result Types.Error Cell
getCell sheetName cellName data =
    if sheetExists sheetName data then
        D.get ( sheetName, cellName ) data.cells
            |> R.fromMaybe (Types.UndefinedGlobalReferenceError ( sheetName, cellName ))

    else
        Err <| Types.UndefinedSheetError sheetName


cellSource : Types.Name -> Document -> Result Types.Error String
cellSource cellName (Document d) =
    getCell (currentSheetName d) cellName d |> R.map Cell.source


get : Types.Name -> Document -> Types.ValueOrError
get name (Document data) =
    evalCell data [] ( currentSheetName data, name )


evalCell : DocData -> List Types.LocatedName -> Types.LocatedName -> Types.ValueOrError
evalCell data ancestors name =
    let
        resolveAbsolute =
            evalCell data (name :: ancestors)

        context =
            { resolveGlobalReference = resolveAbsolute
            , resolveLocalReference =
                \relativeName ->
                    resolveAbsolute ( T.first name, relativeName )
            }

        go () =
            getCell (T.first name) (T.second name) data
                |> R.andThen (Cell.eval context)
    in
    AST.checkCycle name ancestors go


evalSheet : DocData -> Sheet -> Types.ValueOrError
evalSheet data sheet =
    case sheet of
        TableSheet t ->
            Ok (Table.eval (evalCell data []) t)

        _ ->
            Err (Types.TypeError "Only table sheets can be evaluated")


type alias Config msg =
    { toMsg : Msg -> msg }


view : Config msg -> Document -> Html msg
view { toMsg } ((Document data) as doc) =
    let
        gridConfig =
            { toMsg = GridMsg >> toMsg
            , getCellValue = \name -> get name doc
            , getCellSource = \name -> cellSource name doc |> R.withDefault ""
            }

        tableConfig =
            { toMsg = TableMsg >> toMsg
            , resolveAbsolute =
                \name ->
                    evalCell data [] name
            }
    in
    div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , overflow auto
            ]
        ]
        [ case ZL.current data.sheets |> .sheet of
            GridSheet grid ->
                Grid.view gridConfig grid

            TableSheet table ->
                Table.view tableConfig table

            PivotTableSheet pt ->
                MyPivotTable.view (PivotTableMsg >> toMsg) pt
        ]
