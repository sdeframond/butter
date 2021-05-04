module Document exposing
    ( Document
    , Msg
    , Position(..)
    , Sheet(..)
    , commitEdit
    , gridSheet
    , init
    , insertSheet
    , pivotTableSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheetName
    , sheetsWithIds
    , subscriptions
    , tableSheet
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import Formula
import Grid exposing (Grid)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import MyPivotTable exposing (PivotTable)
import MyTable as Table exposing (Table)
import Result as R
import Tuple as T
import Types exposing (Name)
import ZipList as ZL exposing (ZipList)



-- DOCUMENT


type Document
    = Document DocData


type alias DocData =
    { sheets : ZipList ( Types.SheetId, Sheet )
    , sheetIds : Dict Types.Name Types.SheetId
    , nextSheetId : Types.SheetId
    }



-- INIT


init : Sheet -> Document
init sheet =
    Document
        { sheets = ZL.singleton ( 0, sheet )
        , sheetIds = D.fromList [ ( sheetName sheet, 0 ) ]
        , nextSheetId = 1
        }



-- SHEETS


type Sheet
    = GridSheet Name Grid
    | TableSheet Name Table
    | PivotTableSheet Name PivotTable


gridSheet : Name -> Sheet
gridSheet name =
    GridSheet name Grid.init


tableSheet : Name -> Sheet
tableSheet name =
    TableSheet name Table.empty


pivotTableSheet : Name -> Types.Table -> Sheet
pivotTableSheet name table =
    PivotTableSheet name (MyPivotTable.init table)


currentSheet : DocData -> Sheet
currentSheet { sheets } =
    ZL.current sheets |> T.second


currentSheetId : DocData -> Types.SheetId
currentSheetId { sheets } =
    ZL.current sheets |> T.first


currentSheetName : DocData -> Types.Name
currentSheetName { sheets } =
    ZL.current sheets |> T.second |> sheetName


sheetName : Sheet -> Name
sheetName sheet =
    case sheet of
        GridSheet name _ ->
            name

        TableSheet name _ ->
            name

        PivotTableSheet name _ ->
            name


sheetsWithIds : Document -> List (Position ( Types.SheetId, Sheet ))
sheetsWithIds (Document { sheets }) =
    sheets
        |> ZL.toListWithPosition
            { before = Before
            , current = Current
            , after = After
            }


type Position a
    = Before a
    | Current a
    | After a


selectSheet : Types.SheetId -> Document -> Result Types.Error Document
selectSheet selectedId (Document ({ sheets } as data)) =
    ZL.select (T.first >> (==) selectedId) sheets
        |> R.fromMaybe (Types.UndefinedSheetError (selectedId |> String.fromInt))
        |> R.map (\newSheets -> Document { data | sheets = newSheets })


sheetExists : Types.Name -> DocData -> Bool
sheetExists name { sheetIds } =
    D.member name sheetIds


getSheet : Types.SheetId -> DocData -> Maybe Sheet
getSheet sheetId data =
    ZL.get (T.first >> (==) sheetId) data.sheets
        |> Maybe.map T.second


getSheetName : DocData -> Types.SheetId -> Maybe Types.Name
getSheetName data sheetId =
    getSheet sheetId data |> Maybe.map sheetName


insertSheet : Sheet -> Document -> Result Types.Error Document
insertSheet sheet (Document data) =
    insertSheetHelp sheet data |> R.map Document


insertSheetHelp : Sheet -> DocData -> Result Types.Error DocData
insertSheetHelp sheet data =
    if sheetExists (sheetName sheet) data then
        Err (Types.DuplicateSheetNameError (sheetName sheet))

    else
        Ok <|
            { data
                | sheets = ZL.append [ ( data.nextSheetId, sheet ) ] data.sheets
                , nextSheetId = data.nextSheetId + 1
                , sheetIds = D.insert (sheetName sheet) data.nextSheetId data.sheetIds
            }


removeSheet : Types.SheetId -> Document -> Result Types.Error Document
removeSheet sheetId (Document d) =
    let
        newSheetsRes =
            if sheetId == currentSheetId d then
                ZL.removeCurrent d.sheets
                    |> Maybe.map
                        (\newSheets ->
                            { newSheets = newSheets
                            , maybeSheetName = Just (currentSheetName d)
                            }
                        )
                    |> R.fromMaybe (Types.RemovingLastSheetError (currentSheetName d))

            else if ZL.map T.first d.sheets |> ZL.member sheetId then
                Ok
                    { newSheets =
                        ZL.filter (T.first >> (/=) sheetId) d.sheets
                            |> Maybe.withDefault d.sheets
                    , maybeSheetName = getSheetName d sheetId
                    }

            else
                Err (Types.UndefinedSheetError (String.fromInt sheetId))
    in
    newSheetsRes
        |> R.map
            (\{ newSheets, maybeSheetName } ->
                Document
                    { d
                        | sheets = newSheets
                        , sheetIds =
                            maybeSheetName
                                |> Maybe.map (\name -> D.remove name d.sheetIds)
                                |> Maybe.withDefault d.sheetIds
                    }
            )


renameSheet : Types.SheetId -> Types.Name -> Document -> Result Types.Error Document
renameSheet sheetId newName (Document data) =
    let
        updateSheetName : ( Name, Name ) -> DocData -> DocData
        updateSheetName ( validNewName, oldName ) d =
            let
                renameSheet_ ( currentId, sheet ) =
                    T.pair currentId <|
                        if currentId == sheetId then
                            case sheet of
                                GridSheet _ sheetData ->
                                    GridSheet validNewName sheetData

                                TableSheet _ sheetData ->
                                    TableSheet validNewName sheetData

                                PivotTableSheet _ sheetData ->
                                    PivotTableSheet validNewName sheetData

                        else
                            sheet
            in
            { d
                | sheets = ZL.map renameSheet_ d.sheets
                , sheetIds =
                    d.sheetIds
                        |> D.remove oldName
                        |> D.insert validNewName sheetId
            }
    in
    if sheetExists newName data then
        Err (Types.DuplicateSheetNameError newName)

    else
        Formula.parseName newName
            |> R.mapError (always Types.InvalidSheetNameError)
            |> R.andThen
                (\validNewName ->
                    getSheetName data sheetId
                        |> Maybe.map (T.pair validNewName)
                        |> R.fromMaybe (Types.UnexpectedError ("Invalid SheetId: " ++ String.fromInt sheetId))
                )
            |> R.map updateSheetName
            |> R.map (\updater -> Document (updater data))



-- SUSCRIPTIONS


subscriptions : Document -> Sub Msg
subscriptions (Document data) =
    case currentSheet data of
        PivotTableSheet _ pt ->
            Sub.map PivotTableMsg (MyPivotTable.subscriptions pt)

        _ ->
            Sub.none



-- UPDATE


type Msg
    = GridMsg Grid.Msg
    | TableMsg Table.Msg
    | PivotTableMsg MyPivotTable.Msg
    | MakePivotTable


update : Msg -> Document -> ( Document, Cmd Msg )
update msg (Document data) =
    updateData msg data |> Tuple.mapBoth Document identity


updateData : Msg -> DocData -> ( DocData, Cmd Msg )
updateData msg data =
    let
        updateSheet newSheet d =
            { d
                | sheets = ZL.setCurrent ( currentSheetId data, newSheet ) d.sheets
            }
    in
    case ( msg, currentSheet data ) of
        ( GridMsg gridMsg, GridSheet name grid ) ->
            ( updateSheet (GridSheet name <| Grid.update (getSheetId data) gridMsg grid) data
            , Cmd.none
            )

        ( TableMsg tableMsg, TableSheet name table ) ->
            ( updateSheet (TableSheet name <| Table.update (getSheetId data) tableMsg table) data
            , Cmd.none
            )

        ( PivotTableMsg ptMsg, PivotTableSheet name pt ) ->
            let
                ( newPt, cmd ) =
                    MyPivotTable.update ptMsg pt
            in
            ( updateSheet (PivotTableSheet name newPt) data
            , Cmd.map PivotTableMsg cmd
            )

        ( MakePivotTable, TableSheet _ table ) ->
            let
                context =
                    { resolveAbsolute = eval data
                    , prefix = currentSheetId data
                    }

                newSheet =
                    pivotTableSheet
                        ("PT" ++ String.fromInt data.nextSheetId)
                        (Table.eval context table)
            in
            ( insertSheetHelp newSheet data
                |> Result.withDefault data
            , Cmd.none
            )

        ( _, _ ) ->
            ( data, Cmd.none )


commitEdit : Document -> Document
commitEdit (Document data) =
    Document <|
        case currentSheet data of
            GridSheet name grid ->
                { data | sheets = ZL.setCurrent ( currentSheetId data, GridSheet name (Grid.commit grid) ) data.sheets }

            _ ->
                data


getSheetId : DocData -> Types.Name -> Maybe Types.SheetId
getSheetId data name =
    D.get name data.sheetIds



-- EVAL


eval : DocData -> Types.LocatedName -> List Types.LocatedName -> Types.ValueOrError
eval data ( sheetId, ref ) ancestors =
    getSheet sheetId data
        |> Result.fromMaybe
            (getSheetName data sheetId
                |> Maybe.map Types.UndefinedSheetError
                |> Maybe.withDefault (Types.UnexpectedError "Found an orphan sheet")
            )
        |> Result.andThen
            (\sheet ->
                case sheet of
                    GridSheet _ grid ->
                        let
                            context : Grid.Context
                            context =
                                { ancestors = ancestors
                                , prefix = sheetId
                                , resolveGlobalReference = eval data
                                , resolveLocalReference = \cellRef -> eval data ( sheetId, cellRef )
                                }
                        in
                        Grid.evalCell grid context ( sheetId, ref )

                    _ ->
                        Err (Types.TypeError "Only grids can be referenced for now")
            )



-- VIEW


type alias Config msg =
    { toMsg : Msg -> msg }


view : Config msg -> Document -> Html msg
view { toMsg } (Document data) =
    let
        gridConfig sheetId =
            { toMsg = GridMsg >> toMsg
            , getSheetName = getSheetName data
            , context =
                { prefix = sheetId
                , ancestors = []
                , resolveGlobalReference = eval data
                , resolveLocalReference = \cellRef -> eval data ( sheetId, cellRef )
                }
            }

        tableConfig sheetId =
            { toMsg = TableMsg >> toMsg
            , getSheetName = getSheetName data
            , makePivotTableMsg = MakePivotTable |> toMsg
            , context =
                { prefix = sheetId
                , resolveAbsolute = eval data
                }
            }
    in
    div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , overflow hidden
            ]
        ]
        [ case ZL.current data.sheets of
            ( id, GridSheet _ grid ) ->
                Grid.view (gridConfig id) grid

            ( id, TableSheet _ table ) ->
                Table.view (tableConfig id) table

            ( _, PivotTableSheet _ pt ) ->
                MyPivotTable.view (PivotTableMsg >> toMsg) pt
        ]
