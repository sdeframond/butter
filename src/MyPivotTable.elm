module MyPivotTable exposing
    ( Msg(..)
    , PivotTable
    , applyContentFrom
    , decoder
    , encode
    , init
    , subscriptions
    , update
    , view
    )

import Core.UndoCmd as UndoCmd
import Css exposing (..)
import DnDList.Groups as DnDList
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr
import Json.Decode as Decode
import Json.Encode as Encode
import Name exposing (Name)
import PivotTable as PT
import Types


dndConfig : DnDList.Config Draggable
dndConfig =
    { beforeUpdate = \_ _ list -> list
    , listen = DnDList.OnDrop
    , operation = DnDList.Rotate
    , groups =
        { listen = DnDList.OnDrop
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
    { dnd : DnDList.Model
    , table : Types.Table
    , unusedFields : List Name
    , rowFields : List Name
    , columnFields : List Name
    }


type Msg
    = DnDMsg DnDList.Msg


init : Types.Table -> PivotTable
init table =
    PivotTable
        { table = table
        , dnd = dndSystem.model
        , unusedFields = table.fields
        , columnFields = []
        , rowFields = []
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


draggableFields : State -> List Draggable
draggableFields state =
    List.concat
        [ List.map (Just >> Draggable UnusedGroup) state.unusedFields
        , [ Draggable UnusedGroup Nothing ]
        , List.map (Just >> Draggable ColumnsGroup) state.columnFields
        , [ Draggable ColumnsGroup Nothing ]
        , List.map (Just >> Draggable RowsGroup) state.rowFields
        , [ Draggable RowsGroup Nothing ]
        ]


update : Msg -> PivotTable -> ( PivotTable, UndoCmd.Cmd, Cmd Msg )
update msg (PivotTable state) =
    case msg of
        DnDMsg dndMsg ->
            let
                draggables =
                    draggableFields state

                ( newState, undoCmd ) =
                    dndSystem.update dndMsg state.dnd draggables
                        |> toState

                toState ( dnd, newDraggables ) =
                    ( { state
                        | dnd = dnd
                        , unusedFields = filterMapByGroup UnusedGroup newDraggables
                        , columnFields = filterMapByGroup ColumnsGroup newDraggables
                        , rowFields = filterMapByGroup RowsGroup newDraggables
                      }
                    , case dndSystem.info dnd of
                        Just _ ->
                            UndoCmd.None

                        Nothing ->
                            UndoCmd.New
                    )

                filterMapByGroup group =
                    List.filter (.group >> (==) group)
                        >> List.filterMap .maybeName
            in
            ( PivotTable newState
            , undoCmd
            , dndSystem.commands newState.dnd
            )


applyContentFrom : PivotTable -> PivotTable -> PivotTable
applyContentFrom (PivotTable remote) (PivotTable local) =
    PivotTable { remote | dnd = local.dnd }



-- VIEW


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
        getValueAsString name row =
            Name.get name row
                |> Maybe.map Types.valueOrErrorToString
                |> Maybe.withDefault ""

        ptConfig =
            { rowGroupFields = state.rowFields |> List.map getValueAsString
            , colGroupFields = state.columnFields |> List.map getValueAsString
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
        [ PT.pivotTableHtml ptConfig (PT.makeTable state.table.rows) |> H.fromUnstyled
        ]


optionsView : (Msg -> msg) -> State -> Html msg
optionsView toMsg state =
    let
        fields =
            draggableFields state

        indexedFields =
            fields |> List.indexedMap Tuple.pair

        maybeDragItem =
            dndSystem.info state.dnd
                |> Maybe.andThen (\{ dragIndex } -> fields |> List.drop dragIndex |> List.head)
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
        [ groupFieldView toMsg state.dnd maybeDragItem "Fields" UnusedGroup indexedFields
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
                            [ H.text (Name.toString name) ]

                    else
                        H.li [ Attr.id fieldId ] [ H.text "[-----]" ]

                ( Just name, Nothing ) ->
                    H.li
                        (Attr.id fieldId :: dndEvents .dragEvents)
                        [ H.text (Name.toString name) ]

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
                [ H.text (Name.toString name) ]



-- JSON


jsonKeys :
    { table : String
    , unusedFields : String
    , columnFields : String
    , rowFields : String
    }
jsonKeys =
    { unusedFields = "unusedFields"
    , columnFields = "columnFields"
    , rowFields = "rowFields"
    , table = "table"
    }


decoder : Decode.Decoder PivotTable
decoder =
    Decode.map PivotTable stateDecoder


stateDecoder : Decode.Decoder State
stateDecoder =
    Decode.map4 (State dndSystem.model)
        (Decode.field jsonKeys.table Types.tableDecoder)
        (Decode.field jsonKeys.unusedFields <| Decode.list Name.decoder)
        (Decode.field jsonKeys.columnFields <| Decode.list Name.decoder)
        (Decode.field jsonKeys.rowFields <| Decode.list Name.decoder)


encode : PivotTable -> Encode.Value
encode (PivotTable state) =
    Encode.object
        [ ( jsonKeys.table, Types.encodeTable state.table )
        , ( jsonKeys.unusedFields, Encode.list Name.encode state.unusedFields )
        , ( jsonKeys.columnFields, Encode.list Name.encode state.columnFields )
        , ( jsonKeys.rowFields, Encode.list Name.encode state.rowFields )
        ]
