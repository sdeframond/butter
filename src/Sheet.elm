module Sheet exposing
    ( AllParams
    , Config
    , Context
    , Msg(..)
    , Params
    , Sheet
    , allParams
    , commitEdit
    , decoder
    , encode
    , eval
    , fromParams
    , initGrid
    , initPivotTable
    , initTable
    , subscriptions
    , update
    , view
    )

import Grid exposing (Grid)
import Html.Styled exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import MyPivotTable exposing (PivotTable)
import MyTable as Table exposing (Table)
import Name exposing (Name)
import Types


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


type Msg
    = GridMsg Grid.Msg
    | TableMsg Table.Msg
    | PivotTableMsg MyPivotTable.Msg


update : (Name -> Maybe Types.SheetId) -> Msg -> Sheet -> ( Sheet, Cmd Msg )
update getSheetId msg sheet =
    case ( msg, sheet ) of
        ( GridMsg gridMsg, GridSheet grid ) ->
            ( GridSheet <| Grid.update getSheetId gridMsg grid
            , Cmd.none
            )

        ( TableMsg tableMsg, TableSheet table ) ->
            ( TableSheet <| Table.update getSheetId tableMsg table
            , Cmd.none
            )

        ( PivotTableMsg ptMsg, PivotTableSheet pt ) ->
            let
                ( newPt, cmd ) =
                    MyPivotTable.update ptMsg pt
            in
            ( PivotTableSheet newPt
            , Cmd.map PivotTableMsg cmd
            )

        ( _, _ ) ->
            ( sheet, Cmd.none )


commitEdit : Sheet -> Sheet
commitEdit sheet =
    case sheet of
        GridSheet grid ->
            GridSheet (Grid.commit grid)

        _ ->
            sheet


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
