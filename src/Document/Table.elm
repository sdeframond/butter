module Document.Table exposing
    ( Msg
    , Table
    , empty
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import Html.Styled as H
    exposing
        ( Html
        , input
        , span
        , td
        , text
        , th
        )
import Html.Styled.Attributes as Attr exposing (css, value)
import Html.Styled.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Decode
import List as L
import Maybe as M
import Table as T exposing (defaultCustomizations)


type Table
    = Table TableData


type alias TableData =
    { fields : List Field
    , rows : List Row
    , state : T.State
    , nextId : Int
    , editedCell : Maybe ( ( Int, String ), String )
    , newColumnName : String
    }


type alias Field =
    { name : String, edit : String }


type alias Row =
    { id : Int, cells : Dict String String }


type Msg
    = SetTableState T.State
    | UpdateEdit String String
    | KeyDown Int
    | EditCell Int String String
    | UpdateEditedCell String
    | AddColumnClicked
    | OnNewColumnNameInput String
    | RemoveColumnButtonClicked String


empty : Table
empty =
    Table
        { fields = []
        , rows = []
        , state = T.initialSort "Field1"
        , nextId = 1
        , editedCell = Nothing
        , newColumnName = ""
        }


update : Msg -> Table -> Table
update msg (Table data) =
    Table <| updateData msg data


updateData : Msg -> TableData -> TableData
updateData msg data =
    let
        commit d =
            case d.editedCell of
                Nothing ->
                    d

                Just ( ( rowId, fieldName ), content ) ->
                    { d
                        | editedCell = Nothing
                        , rows =
                            L.map
                                (\row ->
                                    if row.id == rowId then
                                        { row | cells = D.insert fieldName content row.cells }

                                    else
                                        row
                                )
                                d.rows
                    }
    in
    case msg of
        SetTableState state ->
            { data | state = state }

        UpdateEdit fieldName fieldValue ->
            let
                updateField field =
                    if field.name == fieldName then
                        { field | edit = fieldValue }

                    else
                        field
            in
            { data | fields = L.map updateField data.fields }

        KeyDown code ->
            let
                resetEdit field =
                    { field | edit = "" }

                editsToRow =
                    { id = data.nextId
                    , cells =
                        data.fields
                            |> L.map (\f -> ( f.name, f.edit ))
                            |> D.fromList
                    }
            in
            if code == 13 then
                -- Enter key
                { data
                    | fields = L.map resetEdit data.fields
                    , rows = editsToRow :: data.rows
                    , nextId = data.nextId + 1
                }

            else
                data

        EditCell rowId fieldName content ->
            let
                commited =
                    commit data
            in
            { commited | editedCell = Just ( ( rowId, fieldName ), content ) }

        UpdateEditedCell content ->
            case data.editedCell of
                Nothing ->
                    data

                Just ( cellRef, _ ) ->
                    { data | editedCell = Just ( cellRef, content ) }

        AddColumnClicked ->
            if data.newColumnName /= "" then
                { data
                    | newColumnName = ""
                    , fields = data.fields ++ [ { name = data.newColumnName, edit = "" } ]
                }

            else
                data

        OnNewColumnNameInput name ->
            { data | newColumnName = name }

        RemoveColumnButtonClicked name ->
            { data
                | fields = L.filter (.name >> (/=) name) data.fields
                , rows = L.map (\r -> { r | cells = D.remove name r.cells }) data.rows
            }


view : (Msg -> msg) -> Table -> Html msg
view toMsg (Table ({ rows, state } as data)) =
    T.view (config toMsg data) state rows |> H.fromUnstyled


config : (Msg -> msg) -> TableData -> T.Config Row msg
config toMsg { fields, editedCell, newColumnName } =
    let
        toColumn { name } =
            T.veryCustomColumn
                { name = name
                , sorter = T.decreasingOrIncreasingBy (.cells >> D.get name >> M.withDefault "")
                , viewData = cellView name editedCell toMsg
                }

        customizations =
            { defaultCustomizations
                | tfoot = Just (tfoot toMsg fields)
                , thead = thead newColumnName fields toMsg
            }
    in
    T.customConfig
        { toId = .id >> String.fromInt
        , toMsg = SetTableState >> toMsg
        , columns = fields |> L.map toColumn
        , customizations = customizations
        }


cellView : String -> Maybe ( ( Int, String ), String ) -> (Msg -> msg) -> Row -> T.HtmlDetails msg
cellView name editedCell toMsg { id, cells } =
    let
        defaultCellView =
            D.get name cells
                |> M.withDefault ""
                |> text
                |> L.singleton
                |> span
                    [ onClick
                        (D.get name cells
                            |> M.withDefault ""
                            |> EditCell id name
                            |> toMsg
                        )
                    ]
                |> H.toUnstyled
    in
    T.HtmlDetails []
        [ case editedCell of
            Just ( cellRef, content ) ->
                if cellRef == ( id, name ) then
                    H.toUnstyled <|
                        H.input
                            [ onInput (UpdateEditedCell >> toMsg)
                            , value content
                            ]
                            []

                else
                    defaultCellView

            Nothing ->
                defaultCellView
        ]


tfoot toMsg fields =
    let
        onKeyDown tagger =
            on "keydown" (Decode.map tagger keyCode)
    in
    T.HtmlDetails []
        (fields
            |> L.map
                (\f ->
                    td [ css [ position sticky, bottom (px 0) ] ]
                        [ input
                            [ value f.edit
                            , onInput (UpdateEdit f.name >> toMsg)
                            , onKeyDown (KeyDown >> toMsg)
                            ]
                            []
                        ]
                        |> H.toUnstyled
                )
        )


thead newColumnName fields toMsg headers =
    let
        onKeyDown tagger =
            on "keydown" (Decode.map tagger keyCode)
    in
    T.HtmlDetails [] <|
        L.map H.toUnstyled
            [ H.tr []
                (List.map (theadHelp toMsg) headers
                    ++ [ input [ onInput (OnNewColumnNameInput >> toMsg), value newColumnName ] []
                       , span [ onClick (toMsg AddColumnClicked) ] [ text "add" ]
                       ]
                )
            , H.tr []
                (fields
                    |> L.map
                        (\f ->
                            td [ css [ position sticky, bottom (px 0) ] ]
                                [ input
                                    [ value f.edit
                                    , onInput (UpdateEdit f.name >> toMsg)
                                    , onKeyDown (KeyDown >> toMsg)
                                    ]
                                    []
                                ]
                        )
                )
            ]


theadHelp toMsg ( name, status, onClick_ ) =
    let
        sortOnClick =
            span [ onClick_ |> Attr.fromUnstyled ]

        sortArrow base =
            case status of
                T.Unsortable ->
                    base

                T.Sortable selected ->
                    (if selected then
                        sortOnClick [ text "↓" ]

                     else
                        sortOnClick [ text "↓" ]
                    )
                        :: base

                T.Reversible Nothing ->
                    sortOnClick [ text "↕" ] :: base

                T.Reversible (Just isReversed) ->
                    sortOnClick
                        [ text
                            (if isReversed then
                                "↑"

                             else
                                "↓"
                            )
                        ]
                        :: base

        deleteButton =
            span [ onClick (RemoveColumnButtonClicked name |> toMsg) ] [ text "X" ]
    in
    th [ css [ position sticky, top (px 0) ] ] (text name :: sortArrow [ deleteButton ])
