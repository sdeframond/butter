module MyPivotTable exposing (Msg, PivotTable, empty, subscriptions, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import DnDList.Groups as DnDList
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import PivotTable as PT
import Types exposing (Name, ValueOrError)


dndConfig : DnDList.Config Draggable
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , listen = DnDList.OnDrag
    , operation = DnDList.Rotate
    , groups =
        { listen = DnDList.OnDrag
        , operation = DnDList.InsertBefore
        , comparator = \f1 f2 -> f1.group == f2.group
        , setter = \f1 f2 -> { f2 | group = f1.group }
        }
    }


dndSystem : DnDList.System Draggable Msg
dndSystem =
    DnDList.create dndConfig DnDMsg


type PivotTable
    = PivotTable State


type alias State =
    { sourceId : Types.SheetId
    , source : Maybe Types.Table
    , fields : List Draggable
    , dnd : DnDList.Model
    }


type Msg
    = OnInputSource String -- TODO remove this message
    | DnDMsg DnDList.Msg


empty : PivotTable
empty =
    PivotTable
        { sourceId = -1 -- TODO get the ID as a aparameter
        , source = Nothing
        , fields = []
        , dnd = dndSystem.model
        }


type alias Draggable =
    { group : Group
    , maybeName : Maybe Name -- No name for placeholders
    }


type Group
    = ColumnsGroup
    | RowsGroup
    | UnusedGroup


subscriptions : PivotTable -> Sub Msg
subscriptions (PivotTable state) =
    dndSystem.subscriptions state.dnd


update : (Types.SheetId -> Maybe Types.Table) -> Msg -> PivotTable -> ( PivotTable, Cmd Msg )
update getTable msg (PivotTable state) =
    case msg of
        OnInputSource input ->
            let
                source =
                    input |> String.toInt |> Maybe.andThen getTable
            in
            ( PivotTable
                { state
                    | sourceId = input |> String.toInt |> Maybe.withDefault -1
                    , source = source
                    , fields =
                        (source
                            |> Maybe.map (.fields >> List.map (Just >> Draggable UnusedGroup))
                            |> Maybe.withDefault []
                        )
                            ++ [ Draggable UnusedGroup Nothing
                               , Draggable ColumnsGroup Nothing
                               , Draggable RowsGroup Nothing
                               ]
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
        groupFields group =
            state.fields
                |> List.filter (.group >> (==) group)
                |> List.filterMap .maybeName
                |> List.map Dict.get
                |> List.map
                    (\f ->
                        f >> Maybe.map Types.valueOrErrorToString >> Maybe.withDefault ""
                    )

        ptConfig =
            { rowGroupFields = groupFields RowsGroup
            , colGroupFields = groupFields ColumnsGroup
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
    let
        indexedFields =
            state.fields |> List.indexedMap Tuple.pair

        maybeDragItem =
            dndSystem.info state.dnd
                |> Maybe.andThen (\{ dragIndex } -> state.fields |> List.drop dragIndex |> List.head)
    in
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
        [ H.input [ Events.onInput (OnInputSource >> toMsg), Attr.value (state.sourceId |> String.fromInt) ] []
        , groupFieldView toMsg state.dnd maybeDragItem "Fields" UnusedGroup indexedFields
        , groupFieldView toMsg state.dnd maybeDragItem "Columns" ColumnsGroup indexedFields
        , groupFieldView toMsg state.dnd maybeDragItem "Rows" RowsGroup indexedFields
        , ghostField toMsg state.dnd maybeDragItem
        ]


groupFieldView : (Msg -> msg) -> DnDList.Model -> Maybe Draggable -> String -> Group -> List ( Int, Draggable ) -> Html msg
groupFieldView toMsg dnd maybeDragItem label currentGroup indexedFields =
    let
        fieldView ( index, field ) =
            let
                fieldId : String
                fieldId =
                    "id-" ++ String.fromInt index

                dndEvents events =
                    events dndSystem index fieldId |> List.map (Attr.fromUnstyled >> Attr.map toMsg)
            in
            case ( field.maybeName, dndSystem.info dnd ) of
                ( Just name, Just { dragIndex } ) ->
                    if dragIndex /= index then
                        H.li
                            (Attr.id fieldId :: dndEvents .dropEvents)
                            [ H.text name ]

                    else
                        H.li [ Attr.id fieldId ] [ H.text "[-----]" ]

                ( Just name, Nothing ) ->
                    H.li
                        (Attr.id fieldId :: dndEvents .dragEvents)
                        [ H.text name ]

                ( Nothing, Just _ ) ->
                    H.li
                        (Attr.id fieldId
                            :: (case Maybe.map .group maybeDragItem of
                                    Just dragItemGroup ->
                                        if currentGroup /= dragItemGroup then
                                            dndEvents .dropEvents

                                        else
                                            []

                                    Nothing ->
                                        []
                               )
                        )
                        [ H.text "" ]

                ( Nothing, Nothing ) ->
                    H.li
                        [ Attr.id fieldId ]
                        [ H.text "" ]

        filteredFields =
            indexedFields
                |> List.filter (Tuple.second >> .group >> (==) currentGroup)
    in
    H.div []
        [ H.h1 [] [ H.text label ]
        , H.ul [] (filteredFields |> List.map fieldView)
        ]


ghostField : (Msg -> msg) -> DnDList.Model -> Maybe Draggable -> Html msg
ghostField toMsg dnd maybeDragItem =
    case maybeDragItem |> Maybe.andThen .maybeName of
        Nothing ->
            H.text ""

        Just name ->
            H.li
                (dndSystem.ghostStyles dnd |> List.map (Attr.fromUnstyled >> Attr.map toMsg))
                [ H.text name ]
