module Grid exposing
    ( Config
    , Context
    , Grid
    , Msg(..)
    , decoder
    , encode
    , evalCell
    , init
    , merge
    , update
    , view
    )

import Core.UndoCmd as UndoCmd
import Css exposing (..)
import DecodeHelpers
import Formula exposing (Formula)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List as L
import Name exposing (Name)
import PositiveInt
import Types exposing (DataType(..), Error(..), Value(..))
import Ui



-- MODEL


type Grid
    = Grid GridData


type alias GridData =
    { editState : Maybe ( Name, Cell )
    , cells : Name.Store Cell
    }


type alias UserInput =
    String


type alias Config msg =
    { toMsg : Msg -> msg
    , getSheetName : Types.SheetId -> Maybe Name
    , context : Context
    }


type alias Context =
    { resolveGlobalReference : ( Types.SheetId, Name ) -> List ( Types.SheetId, Name ) -> Types.ValueOrError
    , prefix : Types.SheetId
    , ancestors : List ( Types.SheetId, Name )
    }



--INIT


init : Grid
init =
    Grid { editState = Nothing, cells = Name.empty }


merge : Grid -> Grid -> Grid
merge (Grid inData) (Grid currentData) =
    Grid { inData | editState = currentData.editState }



--CELL


type Cell
    = FormulaCell Formula
    | DataCell Types.DataType UserInput


defaultCell : Cell
defaultCell =
    DataCell Types.StringType ""


evalCell : Grid -> Context -> Name -> Types.ValueOrError
evalCell (Grid data) =
    evalCell_ data


evalCell_ : GridData -> Context -> Name -> Types.ValueOrError
evalCell_ data context cellRef =
    let
        formulaContext : Formula.Context Types.SheetId
        formulaContext =
            { resolveGlobalReference = context.resolveGlobalReference
            , prefix = context.prefix
            , ancestors = context.ancestors
            , resolveLocalReference = \ref -> context.resolveGlobalReference ( context.prefix, ref )
            }

        help cell =
            case cell of
                FormulaCell formula ->
                    Formula.eval formulaContext formula

                DataCell cellType input ->
                    case cellType of
                        Types.StringType ->
                            Types.StringValue input |> Ok

                        Types.IntType ->
                            Formula.parseInt input
                                |> Result.mapError (always Types.ParsingError)
                                |> Result.map Types.IntValue
    in
    getCell cellRef data
        |> Result.andThen help


getCell : Name -> GridData -> Result Types.Error Cell
getCell cellName data =
    Name.get cellName data.cells
        |> Result.fromMaybe (Types.UndefinedLocalReferenceError cellName)



-- UPDATE


type Msg
    = StartEditing Name
    | UpdateEdit UserInput
    | OnClickCellTypeBtn
    | OnClickCellDataTypeBtn


update : (Name -> Maybe Types.SheetId) -> Msg -> Grid -> ( Grid, UndoCmd.Cmd )
update getSheetId msg (Grid data) =
    let
        mapEditedCell fn =
            .editState >> Maybe.map (Tuple.mapSecond fn)
    in
    case msg of
        UpdateEdit input ->
            case data.editState of
                Just ( name, cell ) ->
                    let
                        newCell =
                            case cell of
                                FormulaCell _ ->
                                    FormulaCell (Formula.fromSource getSheetId input)

                                DataCell dataType _ ->
                                    DataCell dataType input
                    in
                    ( Grid { data | editState = Just ( name, newCell ) }, UndoCmd.None )

                Nothing ->
                    ( Grid data, UndoCmd.None )

        StartEditing name ->
            let
                newData =
                    commitData data

                newCell =
                    getCell name data
                        |> Result.withDefault defaultCell
            in
            ( Grid { newData | editState = Just ( name, newCell ) }, UndoCmd.New )

        OnClickCellTypeBtn ->
            let
                switchCellType cell =
                    case cell of
                        FormulaCell formula ->
                            DataCell Types.StringType (Formula.initialInput formula)

                        DataCell _ input ->
                            FormulaCell (Formula.fromSource getSheetId input)
            in
            ( Grid { data | editState = mapEditedCell switchCellType data }, UndoCmd.None )

        OnClickCellDataTypeBtn ->
            let
                mapDataType fn cell =
                    case cell of
                        FormulaCell _ ->
                            cell

                        DataCell dataType input ->
                            DataCell (fn dataType) input

                switchDataType dataType =
                    case dataType of
                        Types.IntType ->
                            Types.StringType

                        Types.StringType ->
                            Types.IntType
            in
            ( Grid { data | editState = mapEditedCell (mapDataType switchDataType) data }, UndoCmd.None )



-- COMMIT


commitData : GridData -> GridData
commitData data =
    data.editState
        |> Maybe.map
            (\( name, cell ) ->
                if cell == defaultCell then
                    { data
                        | cells = Name.remove name data.cells
                        , editState = Nothing
                    }

                else
                    { data
                        | cells = Name.insert name cell data.cells
                        , editState = Nothing
                    }
            )
        |> Maybe.withDefault data



-- VIEW


view : Config msg -> Grid -> Html msg
view { toMsg, getSheetName, context } (Grid ({ editState } as data)) =
    let
        numberOfRow =
            40

        numberOfColumns =
            20

        mapColumns f =
            PositiveInt.range numberOfColumns |> L.map f

        blueGrey =
            rgb 220 220 240

        myTh =
            styled th
                [ backgroundColor blueGrey
                , padding2 (Css.em 0.2) (Css.em 0.4)
                , border3 (px 1) solid (rgb 150 150 150)
                ]

        columnHeaders =
            th [] []
                :: mapColumns
                    (\col ->
                        myTh [ css [ top (px 0) ] ] [ text <| PositiveInt.toLetters col ]
                    )

        rowHeader row =
            myTh [ css [ left (px 0), position sticky ] ] [ row |> PositiveInt.toString |> text ]

        cellSource cell =
            case cell of
                FormulaCell formula ->
                    Formula.sourceView getSheetName formula
                        |> Maybe.withDefault "Critical error"

                DataCell _ input ->
                    input

        cellView row col =
            let
                cellName =
                    Name.fromCoord col row

                defaultCellView =
                    td
                        [ cellCss
                        , StartEditing cellName
                            |> toMsg
                            |> onClick
                        ]
                        [ evalCell_ data context cellName
                            |> Types.valueOrErrorToString
                            |> text
                        ]

                cellCss =
                    css
                        [ border3 (px 1) solid (rgb 230 230 230)
                        , minWidth (Css.em 10)
                        , height (px 20)
                        ]
            in
            case editState of
                Just ( editedCellName, cell ) ->
                    if cellName == editedCellName then
                        td [ cellCss ]
                            [ input
                                [ value (cellSource cell)
                                , onInput (UpdateEdit >> toMsg)
                                ]
                                []
                            ]

                    else
                        defaultCellView

                Nothing ->
                    defaultCellView

        rows =
            PositiveInt.range numberOfRow
                |> L.map
                    (\row ->
                        tr [] <|
                            rowHeader row
                                :: mapColumns (cellView row)
                    )
    in
    Ui.fullRow []
        [ H.table
            [ css
                [ borderCollapse collapse
                , flex2 (int 1) (int 1)
                , overflow auto
                , display block
                ]
            ]
            [ thead [ css [ position sticky, top (px 0) ] ] columnHeaders
            , tbody [] rows
            ]
        , case data.editState of
            Just ( _, cell ) ->
                Ui.column [] [ cellPropertiesView toMsg cell ]

            Nothing ->
                text ""
        ]


cellPropertiesView : (Msg -> msg) -> Cell -> Html msg
cellPropertiesView toMsg cell =
    H.div []
        [ Ui.button
            [ onClick (OnClickCellTypeBtn |> toMsg) ]
            [ case cell of
                FormulaCell _ ->
                    text "Formula"

                DataCell _ _ ->
                    text "Data"
            ]
        , case cell of
            FormulaCell _ ->
                text ""

            DataCell dataType _ ->
                Ui.button
                    [ onClick (OnClickCellDataTypeBtn |> toMsg) ]
                    [ case dataType of
                        Types.IntType ->
                            text "Int"

                        Types.StringType ->
                            text "String"
                    ]
        ]



-- JSON


jsonKeys :
    { cellType : String
    , formula : String
    , data : String
    , dataType : String
    , input : String
    , editState : String
    , name : String
    , cell : String
    , cells : String
    }
jsonKeys =
    { cellType = "type"
    , formula = "formula"
    , data = "data"
    , dataType = "dataType"
    , input = "input"
    , editState = "editState"
    , name = "name"
    , cell = "cell"
    , cells = "cells"
    }


decoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder Grid
decoder getSheetId =
    Decode.map2 GridData
        (Decode.map2 Tuple.pair
            (Decode.field jsonKeys.name Name.decoder)
            (Decode.field jsonKeys.cell <| cellDecoder getSheetId)
            |> Decode.nullable
            |> Decode.field jsonKeys.editState
        )
        (Decode.field jsonKeys.cells (Name.storeDecoder <| cellDecoder getSheetId))
        |> Decode.map Grid


cellDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder Cell
cellDecoder getSheetId =
    Decode.field jsonKeys.cellType Decode.string
        |> DecodeHelpers.switch "Invalid cell type"
            [ ( jsonKeys.formula
              , Decode.map FormulaCell
                    (Decode.field jsonKeys.formula <| Formula.decoder getSheetId)
              )
            , ( jsonKeys.data
              , Decode.map2 DataCell
                    (Decode.field jsonKeys.dataType Types.dataTypeDecoder)
                    (Decode.field jsonKeys.input Decode.string)
              )
            ]


encode : (Types.SheetId -> Maybe Name) -> Grid -> Encode.Value
encode getSheetName (Grid data) =
    Encode.object
        [ ( jsonKeys.editState
          , data.editState
                |> Maybe.map
                    (\( name, cell ) ->
                        Encode.object
                            [ ( jsonKeys.name, Name.encode name )
                            , ( jsonKeys.cell, encodeCell getSheetName cell )
                            ]
                    )
                |> Maybe.withDefault Encode.null
          )
        , ( jsonKeys.cells, Name.encodeStore (encodeCell getSheetName) data.cells )
        ]


encodeCell : (Types.SheetId -> Maybe Name) -> Cell -> Encode.Value
encodeCell getSheetName cell =
    case cell of
        FormulaCell formula ->
            Encode.object
                [ ( jsonKeys.cellType, Encode.string jsonKeys.formula )
                , ( jsonKeys.formula, Formula.encode getSheetName formula )
                ]

        DataCell dataType input ->
            Encode.object
                [ ( jsonKeys.cellType, Encode.string jsonKeys.data )
                , ( jsonKeys.dataType, Types.encodeDataType dataType )
                , ( jsonKeys.input, Encode.string input )
                ]
