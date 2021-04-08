module MyPivotTable exposing (Msg, PivotTable, empty, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes exposing (css)
import PivotTable as PT
import Types exposing (Name, ValueOrError)


type PivotTable
    = PivotTable State


type alias State =
    { source : Name
    , columns : List Name
    , rows : List Name
    , values : List Name
    }


type alias Row =
    Dict Name ValueOrError


type Msg
    = NoOp


empty : PivotTable
empty =
    PivotTable
        { source = "Sheet1"
        , columns = ["a", "b"]
        , rows = ["c", "d"]
        , values = []
        }


update : Msg -> PivotTable -> PivotTable
update msg pt =
    Debug.todo "PivotTable.update"


type alias Config =
    { get : Name -> ValueOrError }


view : Config -> PivotTable -> Html msg
view config (PivotTable state) =
    H.div
        [ css
            [ displayFlex
            , flexDirection Css.row
            , height (pct 100)
            ]
        ]
        [ tableView config state
        , optionsView
        ]


tableView : Config -> State -> Html msg
tableView config state =
    let
        tableResult : Result Types.Error (PT.Table Row)
        tableResult =
            config.get state.source
                |> Result.andThen
                    (\v ->
                        case v of
                            Types.TableValue t ->
                                Ok (PT.makeTable t.rows)

                            _ ->
                                Err (Types.TypeError "Expecting a Table value")
                    )

        groupFields fieldNames =
            fieldNames
                |> List.map Dict.get
                |> List.map
                    (\f ->
                        f >> Maybe.map Types.valueOrErrorToString >> Maybe.withDefault ""
                    )

        ptConfig =
            { rowGroupFields = groupFields state.rows
            , colGroupFields = groupFields state.columns
            , aggregator = List.length
            , viewRow = H.text >> H.toUnstyled
            , viewCol = H.text >> H.toUnstyled
            , viewAgg = String.fromInt >> H.text >> H.toUnstyled
            }
    in
    H.div
        [ css
            [ flex2 (int 1) (int 1)
            , overflow auto
            ]
        ]
        [ case tableResult of
            Ok table ->
                PT.pivotTableHtml ptConfig table |> H.fromUnstyled

            Err e ->
                H.text (Debug.toString e)
        ]


optionsView : Html msg
optionsView =
    H.div
        [ css
            [ flex3 (int 0) (int 0) (px 100)
            , border3 (px 1) solid (rgb 0 0 0)
            , height (pct 100)
            , display inlineBlock
            , displayFlex
            , flexDirection column
            ]
        ]
        [ H.text "TODO" ]
