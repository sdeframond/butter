module Sheet exposing
    ( AllParams
    , Config
    , Context
    , Diff
    , DiffError
    , Msg(..)
    , Params
    , Sheet
    , allParams
    , applyContentFrom
    , applyDiff
    , decoder
    , diffDecoder
    , encode
    , encodeDiff
    , eval
    , fromParams
    , initGrid
    , initPivotTable
    , initTable
    , makeDiff
    , subscriptions
    , update
    , view
    )

import Core.Name exposing (Name)
import Core.Types as Types
import Core.UndoCmd as UndoCmd
import DecodeHelpers
import Grid exposing (Grid)
import Html.Styled exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import MyPivotTable exposing (PivotTable)
import MyTable as Table exposing (Table)


type Sheet
    = GridSheet Grid
    | TableSheet Table
    | PivotTableSheet PivotTable


type Params
    = GridSheetParams
    | TableSheetParams
    | PivotTableSheetParams Types.Table


type alias AllParams =
    { pivotTable : Types.Table -> Params
    , grid : Params
    , table : Params
    }


allParams : AllParams
allParams =
    { pivotTable = PivotTableSheetParams
    , grid = GridSheetParams
    , table = TableSheetParams
    }


fromParams : Params -> Sheet
fromParams params =
    case params of
        GridSheetParams ->
            initGrid

        TableSheetParams ->
            initTable

        PivotTableSheetParams table ->
            initPivotTable table


initGrid : Sheet
initGrid =
    GridSheet Grid.init


initTable : Sheet
initTable =
    TableSheet Table.empty


initPivotTable : Types.Table -> Sheet
initPivotTable table =
    PivotTableSheet (MyPivotTable.init table)


subscriptions : Sheet -> Sub Msg
subscriptions sheet =
    case sheet of
        PivotTableSheet pt ->
            Sub.map PivotTableMsg (MyPivotTable.subscriptions pt)

        _ ->
            Sub.none


applyContentFrom : Sheet -> Sheet -> Sheet
applyContentFrom inSheet currentSheet =
    case ( inSheet, currentSheet ) of
        ( GridSheet inGrid, GridSheet currentGrid ) ->
            GridSheet (Grid.applyContentFrom inGrid currentGrid)

        ( TableSheet inTable, TableSheet currentTable ) ->
            TableSheet (Table.applyContentFrom inTable currentTable)

        ( PivotTableSheet inPivotTable, PivotTableSheet currentPivotTable ) ->
            PivotTableSheet (MyPivotTable.applyContentFrom inPivotTable currentPivotTable)

        ( sheet, _ ) ->
            sheet


type Msg
    = GridMsg Grid.Msg
    | TableMsg Table.Msg
    | PivotTableMsg MyPivotTable.Msg


update : (Name -> Maybe Types.SheetId) -> Msg -> Sheet -> ( Sheet, ( UndoCmd.Cmd, Cmd Msg ) )
update getSheetId msg sheet =
    case ( msg, sheet ) of
        ( GridMsg gridMsg, GridSheet grid ) ->
            let
                ( newGrid, undoCmd ) =
                    Grid.update getSheetId gridMsg grid
            in
            ( GridSheet newGrid
            , ( undoCmd, Cmd.none )
            )

        ( TableMsg tableMsg, TableSheet table ) ->
            Table.update getSheetId tableMsg table
                |> Tuple.mapBoth TableSheet (\undoCmd -> ( undoCmd, Cmd.none ))

        ( PivotTableMsg ptMsg, PivotTableSheet pt ) ->
            let
                ( newPt, undoCmd, cmd ) =
                    MyPivotTable.update ptMsg pt
            in
            ( PivotTableSheet newPt
            , ( undoCmd, Cmd.map PivotTableMsg cmd )
            )

        ( _, _ ) ->
            ( sheet, ( UndoCmd.New, Cmd.none ) )


type alias Context =
    Grid.Context


eval : Name -> Context -> Sheet -> Types.ValueOrError
eval ref context sheet =
    case sheet of
        GridSheet grid ->
            Grid.evalCell grid context ref

        _ ->
            Err (Types.TypeError "Only grids can be referenced for now")


type alias Config msg =
    { toMsg : Msg -> msg
    , insertPivotTable : Types.Table -> msg
    , getSheetName : Types.SheetId -> Maybe Name
    , context : Context
    }


view : Config msg -> Sheet -> Html msg
view config sheet =
    let
        gridConfig =
            { toMsg = GridMsg >> config.toMsg
            , getSheetName = config.getSheetName
            , context = config.context
            }

        tableContext =
            { prefix = config.context.prefix
            , resolveAbsolute = config.context.resolveGlobalReference
            }

        tableConfig =
            { toMsg = TableMsg >> config.toMsg
            , getSheetName = config.getSheetName
            , insertPivotTable =
                Table.eval tableContext
                    >> config.insertPivotTable
            , context = tableContext
            }
    in
    case sheet of
        GridSheet grid ->
            Grid.view gridConfig grid

        TableSheet table ->
            Table.view tableConfig table

        PivotTableSheet pt ->
            MyPivotTable.view (PivotTableMsg >> config.toMsg) pt



-- DIFF


type Diff
    = SheetTypeChangedDiff Sheet
    | GridDiff Grid.Diff


type DiffError
    = DiffDoesNotMatchSheetTypeError
    | GridDiffError Grid.DiffError


makeDiff : Sheet -> Sheet -> Diff
makeDiff new old =
    case ( new, old ) of
        ( GridSheet newGrid, GridSheet oldGrid ) ->
            GridDiff (Grid.makeDiff newGrid oldGrid)

        ( _, _ ) ->
            SheetTypeChangedDiff new


applyDiff : Diff -> Sheet -> Result DiffError Sheet
applyDiff diff sheet =
    case ( diff, sheet ) of
        ( SheetTypeChangedDiff newSheet, _ ) ->
            Ok newSheet

        ( GridDiff gridDiff, GridSheet grid ) ->
            Grid.applyDiff gridDiff grid
                |> Result.map GridSheet
                |> Result.mapError GridDiffError

        ( _, _ ) ->
            Err DiffDoesNotMatchSheetTypeError


diffDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder Diff
diffDecoder getSheetId =
    Decode.field "type" Decode.string
        |> DecodeHelpers.switch "Decoder type invalid"
            [ ( "SheetTypeChangedDiff", decoder getSheetId |> Decode.field "diff" |> Decode.map SheetTypeChangedDiff )
            , ( "GridDiff", Grid.diffDecoder getSheetId |> Decode.field "diff" |> Decode.map GridDiff )
            ]


encodeDiff : (Types.SheetId -> Maybe Name) -> Diff -> Encode.Value
encodeDiff getSheetName diff =
    case diff of
        SheetTypeChangedDiff sheet ->
            Encode.object
                [ ( "type", Encode.string "SheetTypeChangedDiff" )
                , ( "diff", encode getSheetName sheet )
                ]

        GridDiff gridDiff ->
            Encode.object
                [ ( "type", Encode.string "GridDiff" )
                , ( "diff", Grid.encodeDiff getSheetName gridDiff )
                ]



-- JSON


jsonKeys : { name : String, grid : String, table : String, pivotTable : String }
jsonKeys =
    { name = "name"
    , grid = "grid"
    , table = "table"
    , pivotTable = "pivotTable"
    }


decoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder Sheet
decoder getSheetId =
    let
        sheetDecoder makeSheet field valueDecoder =
            Decode.map makeSheet <| Decode.field field valueDecoder
    in
    Decode.oneOf
        [ sheetDecoder GridSheet jsonKeys.grid <| Grid.decoder getSheetId
        , sheetDecoder TableSheet jsonKeys.table <| Table.decoder getSheetId
        , sheetDecoder PivotTableSheet jsonKeys.pivotTable MyPivotTable.decoder
        ]


encode : (Types.SheetId -> Maybe Name) -> Sheet -> Encode.Value
encode getSheetName sheet =
    case sheet of
        GridSheet grid ->
            Encode.object
                [ ( jsonKeys.grid, Grid.encode getSheetName grid ) ]

        TableSheet table ->
            Encode.object
                [ ( jsonKeys.table, Table.encode getSheetName table ) ]

        PivotTableSheet pivotTable ->
            Encode.object
                [ ( jsonKeys.pivotTable, MyPivotTable.encode pivotTable ) ]
