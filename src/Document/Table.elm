module Document.Table exposing
    ( Msg
    , Table
    , empty
    , update
    , view
    )

import Dict as D exposing (Dict)
import Document.Cell exposing (Cell)
import Document.Types exposing (Value)
import Html exposing (Html, input, td, text)
import Html.Attributes exposing (value)
import Html.Events exposing (keyCode, on, onInput)
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
    }


type alias Field =
    { name : String, edit : String }


type alias Row =
    { id : Int, cells : Dict String String }


type Msg
    = SetTableState T.State
    | UpdateEdit String String
    | KeyDown Int


empty : Table
empty =
    Table
        { fields =
            [ { name = "Field1", edit = "" }
            , { name = "Field2", edit = "" }
            ]
        , rows = [ { id = 1, cells = D.empty } ]
        , state = T.initialSort "Field1"
        , nextId = 2
        }


update : Msg -> Table -> Table
update msg (Table data) =
    Table <| updateData msg data


updateData : Msg -> TableData -> TableData
updateData msg data =
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


view : (Msg -> msg) -> Table -> Html msg
view toMsg (Table { fields, rows, state }) =
    T.view (config toMsg fields) state rows


config : (Msg -> msg) -> List Field -> T.Config Row msg
config toMsg fields =
    let
        toColumn { name } =
            T.stringColumn name (.cells >> D.get name >> M.withDefault "")

        customizations =
            { defaultCustomizations
                | tfoot = Just tfoot
            }

        onKeyDown tagger =
            on "keydown" (Decode.map tagger keyCode)

        tfoot =
            { attributes = []
            , children =
                fields
                    |> L.map
                        (\f ->
                            td []
                                [ input
                                    [ value f.edit
                                    , onInput (UpdateEdit f.name >> toMsg)
                                    , onKeyDown (KeyDown >> toMsg)
                                    ]
                                    []
                                ]
                        )
            }
    in
    T.customConfig
        { toId = .id >> String.fromInt
        , toMsg = SetTableState >> toMsg
        , columns = fields |> L.map toColumn
        , customizations = customizations
        }
