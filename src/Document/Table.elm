module Document.Table exposing
    (  State
       --, addField
       --, addRow
       --, removeField
       --, updateRow

    , Table
    , empty
    , update
    , view
    )

import Dict as D exposing (Dict)
import Document.Cell exposing (Cell)
import Document.Types exposing (Value)
import Html exposing (Html)
import List as L
import Maybe as M
import Table as T


type Table
    = Table TableData


type alias TableData =
    { fields : List Field
    , rows : List { id : Int, cells : Dict String String }
    , state : T.State
    }


type alias Field =
    { name : String }


type alias Row =
    { id : Int, cells : Dict String String }



--type Error
--    = DuplicateFieldError String
--intField : String -> Field
--intField name =
--    Field name IntField
--stringField : String -> Field
--stringField name =
--    Field name StringField


type alias State =
    T.State


empty : Table
empty =
    Table <|
        TableData
            [ { name = "Field1" } ]
            [ { id = 1, cells = D.empty } ]
            (T.initialSort "Field1")


update : State -> Table -> Table
update s (Table data) =
    Table { data | state = s }


view : (State -> msg) -> Table -> Html msg
view toMsg (Table { fields, rows, state }) =
    T.view (config toMsg fields) state rows


config : (T.State -> msg) -> List Field -> T.Config Row msg
config toMsg fields =
    let
        toColumn { name } =
            T.stringColumn name (.cells >> D.get name >> M.withDefault "")
    in
    T.config
        { toId = .id >> String.fromInt
        , toMsg = toMsg
        , columns = fields |> L.map toColumn
        }



--addField : Field -> Table -> Table
--addField field (Table data) =
