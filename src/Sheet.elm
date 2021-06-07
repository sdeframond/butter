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
    , getName
    , initGrid
    , initPivotTable
    , initTable
    , rename
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
    = GridSheet Name Grid
    | TableSheet Name Table
    | PivotTableSheet Name PivotTable


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


fromParams : Name -> Params -> Sheet
fromParams name params =
    case params of
        GridSheetParams ->
            initGrid name

        TableSheetParams ->
            initTable name

        PivotTableSheetParams table ->
            initPivotTable name table


initGrid : Name -> Sheet
initGrid name =
    GridSheet name Grid.init


initTable : Name -> Sheet
initTable name =
    TableSheet name Table.empty


initPivotTable : Name -> Types.Table -> Sheet
initPivotTable name table =
    PivotTableSheet name (MyPivotTable.init table)


getName : Sheet -> Name
getName sheet =
    case sheet of
        GridSheet name _ ->
            name

        TableSheet name _ ->
            name

        PivotTableSheet name _ ->
            name


rename : Name -> Sheet -> Sheet
rename newName sheet =
    case sheet of
        GridSheet _ sheetData ->
            GridSheet newName sheetData

        TableSheet _ sheetData ->
            TableSheet newName sheetData

        PivotTableSheet _ sheetData ->
            PivotTableSheet newName sheetData


subscriptions : Sheet -> Sub Msg
subscriptions sheet =
    case sheet of
        PivotTableSheet _ pt ->
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
        ( GridMsg gridMsg, GridSheet name grid ) ->
            ( GridSheet name <| Grid.update getSheetId gridMsg grid
            , Cmd.none
            )

        ( TableMsg tableMsg, TableSheet name table ) ->
            ( TableSheet name <| Table.update getSheetId tableMsg table
            , Cmd.none
            )

        ( PivotTableMsg ptMsg, PivotTableSheet name pt ) ->
            let
                ( newPt, cmd ) =
                    MyPivotTable.update ptMsg pt
            in
            ( PivotTableSheet name newPt
            , Cmd.map PivotTableMsg cmd
            )

        ( _, _ ) ->
            ( sheet, Cmd.none )


commitEdit : Sheet -> Sheet
commitEdit sheet =
    case sheet of
        GridSheet name grid ->
            GridSheet name (Grid.commit grid)

        _ ->
            sheet


type alias Context =
    Grid.Context


eval : Name -> Context -> Sheet -> Types.ValueOrError
eval ref context sheet =
    case sheet of
        GridSheet _ grid ->
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
        GridSheet _ grid ->
            Grid.view gridConfig grid

        TableSheet _ table ->
            Table.view tableConfig table

        PivotTableSheet _ pt ->
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
        help makeSheet field sheetDecoder =
            Decode.map2 makeSheet
                (Decode.field jsonKeys.name Name.decoder)
                (Decode.field field sheetDecoder)
    in
    Decode.oneOf
        [ help GridSheet jsonKeys.grid <| Grid.decoder getSheetId
        , help TableSheet jsonKeys.table <| Table.decoder getSheetId
        , help PivotTableSheet jsonKeys.pivotTable MyPivotTable.decoder
        ]


encode : (Types.SheetId -> Maybe Name) -> Sheet -> Encode.Value
encode getSheetName sheet =
    case sheet of
        GridSheet name grid ->
            Encode.object
                [ ( jsonKeys.name, Name.encode name )
                , ( jsonKeys.grid, Grid.encode getSheetName grid )
                ]

        TableSheet name table ->
            Encode.object
                [ ( jsonKeys.name, Name.encode name )
                , ( jsonKeys.table, Table.encode getSheetName table )
                ]

        PivotTableSheet name pivotTable ->
            Encode.object
                [ ( jsonKeys.name, Name.encode name )
                , ( jsonKeys.pivotTable, MyPivotTable.encode pivotTable )
                ]
