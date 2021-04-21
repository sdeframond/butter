module Grid exposing (Config, Grid, Msg, commit, evalCell, init, update, view)

import Ast
import Css exposing (..)
import Dict exposing (Dict)
import Formula exposing (Formula)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import List as L
import String as S
import Types exposing (Error(..), Name, Value(..))



-- MODEL


type Grid
    = Grid GridData


type alias GridData =
    { editState : Maybe ( Name, String )
    , cells : Dict Types.Name Formula
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , getSheetName : Types.SheetId -> Maybe Types.Name
    , context : Ast.Context Types.SheetId
    }



--INIT


init : Grid
init =
    Grid { editState = Nothing, cells = Dict.empty }



--CELL HELPERS


evalCell : Grid -> Ast.Context Types.SheetId -> Types.LocatedName -> Types.ValueOrError
evalCell (Grid data) =
    evalCell_ data


evalCell_ : GridData -> Ast.Context Types.SheetId -> Types.LocatedName -> Types.ValueOrError
evalCell_ data context cellRef =
    getCell (Tuple.second cellRef) data
        |> Result.andThen (Formula.eval context)


getCell : Types.Name -> GridData -> Result Types.Error Formula
getCell cellName data =
    Dict.get cellName data.cells
        |> Result.fromMaybe (Types.UndefinedLocalReferenceError cellName)



-- UPDATE


type Msg
    = StartEditing String String
    | UpdateEdit String String


update : (Name -> Maybe Types.SheetId) -> Msg -> Grid -> Grid
update getSheetId msg (Grid data) =
    case msg of
        UpdateEdit name content ->
            Grid { data | editState = Just ( name, content ) }

        StartEditing name content ->
            let
                newData =
                    commitData getSheetId data
            in
            Grid { newData | editState = Just ( name, content ) }



-- COMMIT


commit : (Name -> Maybe Types.SheetId) -> Grid -> Grid
commit getSheetId (Grid data) =
    commitData getSheetId data |> Grid


commitData : (Name -> Maybe Types.SheetId) -> GridData -> GridData
commitData getSheetId data =
    let
        insertSource cellName input =
            case input of
                "" ->
                    { data | cells = Dict.remove cellName data.cells }

                _ ->
                    { data
                        | cells =
                            Dict.insert cellName
                                (Formula.fromSource getSheetId input)
                                data.cells
                    }
    in
    data.editState
        |> Maybe.map
            (\( name, input ) ->
                let
                    newData =
                        insertSource name input
                in
                { newData | editState = Nothing }
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

        cellSource cellName =
            getCell cellName data
                |> Result.andThen
                    (Formula.sourceView getSheetName
                        >> Result.fromMaybe (Types.UnexpectedError "Found an orphan sheet id reference")
                    )

        cell row col =
            let
                cellName =
                    [ col |> toLetter, row |> S.fromInt ]
                        |> S.concat

                defaultCellView =
                    td
                        [ cellCss
                        , cellSource cellName
                            |> Result.withDefault ""
                            |> StartEditing cellName
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
                Just ( editedCellName, str ) ->
                    if cellName == editedCellName then
                        td [ cellCss ]
                            [ input
                                [ value str
                                , onInput <| (toMsg << UpdateEdit cellName)
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
                                :: mapColumns (cell row)
                    )
    in
    H.table
        [ css [ borderCollapse collapse ] ]
        [ thead [ css [ position sticky, top (px 0) ] ] columnHeaders
        , tbody [] rows
        ]
