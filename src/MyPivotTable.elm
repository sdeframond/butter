module MyPivotTable exposing (Msg, PivotTable, empty, subscriptions, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import DnDList
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import PivotTable as PT
import Types exposing (Name, ValueOrError)


dndConfig : DnDList.Config Field
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , movement = DnDList.Free
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    }


dndSystem : DnDList.System Field Msg
dndSystem =
    DnDList.create dndConfig DnDMsg


type PivotTable
    = PivotTable State


type alias State =
    { sourceName : Name
    , source : Maybe Types.Table
    , fields : List Field
    , dnd : DnDList.Model
    }


type Msg
    = OnInputSource String
    | DnDMsg DnDList.Msg


empty : PivotTable
empty =
    PivotTable
        { sourceName = ""
        , source = Nothing
        , fields = []
        , dnd = dndSystem.model
        }


type alias Field =
    { group : Group
    , name : Name
    }


type Group
    = ColumnsGroup
    | RowsGroup
    | UnusedGroup


subscriptions : PivotTable -> Sub Msg
subscriptions (PivotTable state) =
    dndSystem.subscriptions state.dnd


update : (Name -> ValueOrError) -> Msg -> PivotTable -> ( PivotTable, Cmd Msg )
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
            ( PivotTable
                { state
                    | sourceName = input
                    , source = source
                    , fields =
                        source
                            |> Maybe.map (.fields >> List.map (Field UnusedGroup))
                            |> Maybe.withDefault []
                }
            , Cmd.none
            )

        DnDMsg dndMsg ->
            let
                ( dnd, fields ) =
                    dndSystem.update dndMsg state.dnd state.fields
            in
            ( PivotTable { state | fields = fields, dnd = dnd }
            , dndSystem.commands dnd
            )


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
        , H.ul []
            (state.fields |> List.indexedMap (groupFieldView toMsg state.dnd))
        , ghostField toMsg state.dnd state.fields
        ]


groupFieldView : (Msg -> msg) -> DnDList.Model -> Int -> Field -> Html msg
groupFieldView toMsg dnd index field =
    let
        fieldId : String
        fieldId =
            "id-" ++ field.name

        dndEvents events =
            events index fieldId |> List.map (Attr.fromUnstyled >> Attr.map toMsg)
    in
    case dndSystem.info dnd of
        Just { dragIndex } ->
            if dragIndex /= index then
                H.li
                    (Attr.id fieldId :: dndEvents dndSystem.dropEvents)
                    [ field.name |> H.text ]

            else
                H.li [ Attr.id fieldId ] [ H.text "[-----]" ]

        Nothing ->
            H.li
                (Attr.id fieldId :: dndEvents dndSystem.dragEvents)
                [ field.name |> H.text ]


ghostField : (Msg -> msg) -> DnDList.Model -> List Field -> Html msg
ghostField toMsg dnd fields =
    let
        maybeDragItem =
            dndSystem.info dnd |> Maybe.andThen (\{ dragIndex } -> fields |> List.drop dragIndex |> List.head)
    in
    case maybeDragItem of
        Nothing ->
            H.text ""

        Just field ->
            H.li
                (dndSystem.ghostStyles dnd |> List.map (Attr.fromUnstyled >> Attr.map toMsg))
                [ field.name |> H.text ]
