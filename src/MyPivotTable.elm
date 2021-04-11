module MyPivotTable exposing (Msg, PivotTable, empty, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import PivotTable as PT
import Types exposing (Name, ValueOrError)


type PivotTable
    = PivotTable State


type alias State =
    { sourceName : Name
    , source : Maybe Types.Table
    , fields : List Field
    }


type Msg
    = OnInputSource String


empty : PivotTable
empty =
    PivotTable
        { sourceName = ""
        , source = Nothing
        , fields = []
        }


type alias Field =
    { group : Group
    , name : Name
    }


type Group
    = ColumnsGroup
    | RowsGroup
    | UnusedGroup


update : (Name -> ValueOrError) -> Msg -> PivotTable -> PivotTable
update getSourceValue msg (PivotTable state) =
    case msg of
        OnInputSource input ->
            let
                source =
                    getSourceValue input
                        |> Result.toMaybe
                        |> Maybe.andThen
                            (\v ->
                                case v of
                                    Types.TableValue t ->
                                        Just t

                                    _ ->
                                        Nothing
                            )
            in
            PivotTable
                { state
                    | sourceName = input
                    , source = source
                    , fields =
                        source
                            |> Maybe.map (.fields >> List.map (Field UnusedGroup))
                            |> Maybe.withDefault []
                }


columnFields : State -> List Field
columnFields state =
    state.fields |> List.filter (.group >> (==) ColumnsGroup)


rowFields : State -> List Field
rowFields state =
    state.fields |> List.filter (.group >> (==) RowsGroup)


unusedFields : State -> List Field
unusedFields state =
    state.fields |> List.filter (.group >> (==) UnusedGroup)


view : (Msg -> msg) -> PivotTable -> Html msg
view toMsg (PivotTable state) =
    H.div
        [ Attr.css
            [ displayFlex
            , flexDirection Css.row
            , height (pct 100)
            ]
        ]
        [ tableView state
        , optionsView toMsg state
        ]


tableView : State -> Html msg
tableView state =
    let
        groupFields fieldNames =
            fieldNames
                |> List.map Dict.get
                |> List.map
                    (\f ->
                        f >> Maybe.map Types.valueOrErrorToString >> Maybe.withDefault ""
                    )

        ptConfig =
            { rowGroupFields = groupFields (rowFields state |> List.map .name)
            , colGroupFields = groupFields (columnFields state |> List.map .name)
            , aggregator = List.length
            , viewRow = H.text >> H.toUnstyled
            , viewCol = H.text >> H.toUnstyled
            , viewAgg = String.fromInt >> H.text >> H.toUnstyled
            }
    in
    H.div
        [ Attr.css
            [ flex2 (int 1) (int 1)
            , overflow auto
            ]
        ]
        [ case state.source of
            Just table ->
                PT.pivotTableHtml ptConfig (PT.makeTable table.rows) |> H.fromUnstyled

            Nothing ->
                H.text "Please select a table"
        ]


optionsView : (Msg -> msg) -> State -> Html msg
optionsView toMsg state =
    H.div
        [ Attr.css
            [ flex3 (int 0) (int 0) (px 100)
            , border3 (px 1) solid (rgb 0 0 0)
            , height (pct 100)
            , display inlineBlock
            , displayFlex
            , flexDirection column
            ]
        ]
        [ H.input [ Events.onInput (OnInputSource >> toMsg), Attr.value state.sourceName ] []
        , H.ul [] (state.fields |> List.map (.name >> H.text >> List.singleton >> H.li []))
        ]
