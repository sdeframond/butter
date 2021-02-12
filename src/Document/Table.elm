module Document.Table exposing
    ( Msg
    , Table
    , empty
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import Document.Cell exposing (Cell)
import Document.Types exposing (Value)
import Html.Styled as H
    exposing
        ( Html
        , input
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


empty : Table
empty =
    Table
        { fields =
            [ { name = "Field1", edit = "" }
            , { name = "Field2", edit = "" }
            ]
        , rows = []
        , state = T.initialSort "Field1"
        , nextId = 1
        , editedCell = Nothing
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


view : (Msg -> msg) -> Table -> Html msg
view toMsg (Table ({ rows, state } as data)) =
    T.view (config toMsg data) state rows |> H.fromUnstyled


config : (Msg -> msg) -> TableData -> T.Config Row msg
config toMsg { fields, editedCell } =
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
                , thead = thead
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
                |> H.span
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


thead headers =
    T.HtmlDetails [] (List.map theadHelp headers)


theadHelp ( name, status, onClick_ ) =
    let
        content =
            case status of
                T.Unsortable ->
                    [ text name ]

                T.Sortable selected ->
                    [ text name
                    , if selected then
                        text "↓"

                      else
                        text "↓"
                    ]

                T.Reversible Nothing ->
                    [ text name
                    , text "↕"
                    ]

                T.Reversible (Just isReversed) ->
                    [ text name
                    , text
                        (if isReversed then
                            "↑"

                         else
                            "↓"
                        )
                    ]
    in
    th [ onClick_ |> Attr.fromUnstyled, css [ position sticky, top (px 0) ] ] content |> H.toUnstyled
