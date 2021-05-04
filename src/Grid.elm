module Grid exposing
    ( Config
    , Context
    , Grid
    , Msg
    , commit
    , evalCell
    , init
    , update
    , view
    )

import Css exposing (..)
import Dict exposing (Dict)
import Formula exposing (Formula)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import List as L
import String as S
import Types exposing (DataType(..), Error(..), Name, Value(..))
import Ui



-- MODEL


type Grid
    = Grid GridData


type alias GridData =
    { editState : Maybe ( Name, Cell )
    , cells : Dict Types.Name Cell
    }


type alias UserInput =
    String


type alias Config msg =
    { toMsg : Msg -> msg
    , getSheetName : Types.SheetId -> Maybe Types.Name
    , context : Context
    }


type alias Context =
    Formula.Context Types.SheetId



--INIT


init : Grid
init =
    Grid { editState = Nothing, cells = Dict.empty }



--CELL


type Cell
    = FormulaCell Formula
    | DataCell Types.DataType UserInput


defaultCell : Cell
defaultCell =
    DataCell Types.StringType ""


evalCell : Grid -> Context -> Types.LocatedName -> Types.ValueOrError
evalCell (Grid data) =
    evalCell_ data


evalCell_ : GridData -> Context -> Types.LocatedName -> Types.ValueOrError
evalCell_ data context cellRef =
    let
        help cell =
            case cell of
                FormulaCell formula ->
                    Formula.eval context formula

                DataCell cellType input ->
                    case cellType of
                        Types.StringType ->
                            Types.StringValue input |> Ok

                        Types.IntType ->
                            Formula.parseInt input
                                |> Result.mapError (always Types.ParsingError)
                                |> Result.map Types.IntValue
    in
    getCell (Tuple.second cellRef) data
        |> Result.andThen help


getCell : Types.Name -> GridData -> Result Types.Error Cell
getCell cellName data =
    Dict.get cellName data.cells
        |> Result.fromMaybe (Types.UndefinedLocalReferenceError cellName)



-- UPDATE


type Msg
    = StartEditing Types.Name
    | UpdateEdit UserInput
    | OnClickCellTypeBtn
    | OnClickCellDataTypeBtn


update : (Name -> Maybe Types.SheetId) -> Msg -> Grid -> Grid
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
                    Grid { data | editState = Just ( name, newCell ) }

                Nothing ->
                    Grid data

        StartEditing name ->
            let
                newData =
                    commitData data

                newCell =
                    getCell name data
                        |> Result.withDefault defaultCell
            in
            Grid { newData | editState = Just ( name, newCell ) }

        OnClickCellTypeBtn ->
            let
                switchCellType cell =
                    case cell of
                        FormulaCell formula ->
                            DataCell Types.StringType (Formula.initialInput formula)

                        DataCell _ input ->
                            FormulaCell (Formula.fromSource getSheetId input)
            in
            Grid { data | editState = mapEditedCell switchCellType data }

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
            Grid { data | editState = mapEditedCell (mapDataType switchDataType) data }



-- COMMIT


commit : Grid -> Grid
commit (Grid data) =
    commitData data |> Grid


commitData : GridData -> GridData
commitData data =
    data.editState
        |> Maybe.map
            (\( name, cell ) ->
                if cell == defaultCell then
                    { data
                        | cells = Dict.remove name data.cells
                        , editState = Nothing
                    }

                else
                    { data
                        | cells = Dict.insert name cell data.cells
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

        toLetter i =
            Char.fromCode (i - 1 + Char.toCode 'A') |> S.fromChar

        mapColumns f =
            L.range 1 numberOfColumns |> L.map f

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
                        myTh [ css [ top (px 0) ] ] [ text <| toLetter col ]
                    )

        rowHeader row =
            myTh [ css [ left (px 0), position sticky ] ] [ text <| S.fromInt row ]

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
                    [ col |> toLetter, row |> S.fromInt ]
                        |> S.concat

                defaultCellView =
                    td
                        [ cellCss
                        , StartEditing cellName
                            |> toMsg
                            |> onClick
                        ]
                        [ evalCell_ data context ( context.prefix, cellName )
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
            L.range 1 numberOfRow
                |> L.map
                    (\row ->
                        tr [] <|
                            rowHeader row
                                :: mapColumns (cellView row)
                    )
    in
    Ui.row
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
                Ui.column [ cellPropertiesView toMsg cell ]

            Nothing ->
                text ""
        ]


cellPropertiesView : (Msg -> msg) -> Cell -> Html msg
cellPropertiesView toMsg cell =
    H.div []
        [ H.button [ onClick (OnClickCellTypeBtn |> toMsg) ]
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
                H.button [ onClick (OnClickCellDataTypeBtn |> toMsg) ]
                    [ case dataType of
                        Types.IntType ->
                            text "Int"

                        Types.StringType ->
                            text "String"
                    ]
        ]
