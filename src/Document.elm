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
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetNames
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
import MyTable as Table exposing (Table)
import Result as R
import Tuple as T
import Types exposing (..)
import ZipList as ZL exposing (ZipList)


type Document
    = Document DocData


type alias DocData =
    { cells : Dict LocatedName Cell
    , sheets : ZipList SheetItem
    }


type alias SheetItem =
    { name : String
    , sheet : Sheet
    }


type Sheet
    = GridSheet Grid
    | TableSheet Table


gridSheet : Sheet
gridSheet =
    GridSheet Grid.init


tableSheet : Sheet
tableSheet =
    TableSheet Table.empty


type Msg
    = GridMsg Grid.Msg
    | TableMsg Table.Msg


update : Msg -> Document -> Document
update msg (Document data) =
    Document (updateData msg data)


updateData : Msg -> DocData -> DocData
updateData msg data =
    let
        { sheet, name } =
            ZL.current data.sheets
    in
    case ( msg, sheet ) of
        ( GridMsg gridMsg, GridSheet grid ) ->
            updateGrid (Grid.update gridMsg grid) data

        ( TableMsg tableMsg, TableSheet table ) ->
            let
                newCurrent =
                    SheetItem name (TableSheet <| Table.update tableMsg table)
            in
            { data
                | sheets = ZL.setCurrent newCurrent data.sheets
            }

        ( _, _ ) ->
            data


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


init : Name -> Sheet -> Document
init name sheet =
    Document
        { cells = D.empty
        , sheets = ZL.singleton (SheetItem name sheet)
        }


type Position a
    = Before a
    | Current a
    | After a


sheetNames : Document -> List (Position Name)
sheetNames (Document { sheets }) =
    ZL.map .name sheets
        |> ZL.toListWithPosition
            { before = Before
            , current = Current
            , after = After
            }


currentSheetName : DocData -> Name
currentSheetName { sheets } =
    ZL.current sheets |> .name


selectSheet : Name -> Document -> Result Error Document
selectSheet selectedName (Document ({ sheets } as data)) =
    ZL.select (.name >> (==) selectedName) sheets
        |> R.fromMaybe (UndefinedSheetError selectedName)
        |> R.map (\newSheets -> Document { data | sheets = newSheets })


sheetExists : Name -> DocData -> Bool
sheetExists name { sheets } =
    ZL.member name (ZL.map .name sheets)


insertSheet : Name -> Sheet -> Document -> Result Error Document
insertSheet name sheet (Document data) =
    if sheetExists name data then
        Err (DuplicateSheetNameError name)

    else
        Ok <|
            Document
                { data
                    | sheets = ZL.append [ SheetItem name sheet ] data.sheets
                }


removeSheet : Name -> Document -> Result Error Document
removeSheet name (Document d) =
    let
        newSheetsRes =
            if name == currentSheetName d then
                ZL.removeCurrent d.sheets
                    |> R.fromMaybe (RemovingLastSheetError name)

            else if sheetExists name d then
                ZL.filter (.name >> (/=) name) d.sheets
                    |> Maybe.withDefault d.sheets
                    |> Ok

            else
                Err (UndefinedSheetError name)
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


renameSheet : Name -> Name -> Document -> Result Error Document
renameSheet oldName newName (Document data) =
    let
        updateName parsedName currentName =
            if currentName == oldName then
                parsedName

            else
                currentName

        updateSheetName : (Name -> Name) -> DocData -> DocData
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
                        GridSheet _ -> sheet
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
            Err (DuplicateSheetNameError newName)

        else
            AST.parseName newName
                |> R.mapError (always InvalidSheetNameError)
                |> R.map (updateName >> updateSheetName)
                |> R.map (\updater -> Document (updater data))

    else
        Err <|
            UndefinedSheetError oldName


insert : Name -> String -> Document -> Document
insert cellName value (Document data) =
    Document <| insertHelp cellName value data


insertHelp : Name -> String -> DocData -> DocData
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


getCell : Name -> Name -> DocData -> Result Error Cell
getCell sheetName cellName data =
    if sheetExists sheetName data then
        D.get ( sheetName, cellName ) data.cells
            |> R.fromMaybe (UndefinedGlobalReferenceError ( sheetName, cellName ))

    else
        Err <| UndefinedSheetError sheetName


cellSource : Name -> Document -> Result Error String
cellSource cellName (Document d) =
    getCell (currentSheetName d) cellName d |> R.map Cell.source


get : Name -> Document -> ValueOrError
get name (Document data) =
    evalCell data [] ( currentSheetName data, name )


evalCell : DocData -> List LocatedName -> LocatedName -> ValueOrError
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
        ]
