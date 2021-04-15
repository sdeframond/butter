module MyTable exposing
    ( Msg
    , Table
    , empty
    , eval
    , update
    , view
    )

import AST
import Cell exposing (Cell)
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
import Types exposing (DataType(..), Error(..), Name, ValueOrError)


type Table
    = Table TableData


type alias TableData =
    { fields : List Field
    , rows : List Row
    , state : T.State
    , nextId : Int
    , editedCell : Maybe ( ( Int, String ), String )
    , newField : Field
    }


type alias Field =
    { name : String
    , edit : String
    , fieldType : FieldType
    }


type FieldType
    = DataField DataType
    | FormulaField Cell


emptyField : Field
emptyField =
    { name = "", edit = "", fieldType = DataField StringType }


isValidField : Field -> Bool
isValidField { name, fieldType } =
    name
        /= ""
        && (case fieldType of
                DataField _ ->
                    True

                FormulaField cell ->
                    Cell.isValid cell
           )


type alias Row =
    { id : Int, cells : Dict String String }


type Msg
    = SetTableState T.State
    | UpdateNewRowField Name String
    | KeyDown Int
    | EditCell Int String String
    | UpdateEditedCell String
    | OnClickAddFieldBtn
    | OnInputNewFieldName String
    | OnClickRemoveColumnBtn String
    | OnClickNewFieldTypeBtn
    | OnClickNewFieldDataTypeBtn
    | OnInputNewFieldFormula String


empty : Table
empty =
    Table
        { fields = []
        , rows = []
        , state = T.initialSort ""
        , nextId = 1
        , editedCell = Nothing
        , newField = emptyField
        }


update : (Types.Name -> Maybe Types.SheetId) -> Msg -> Table -> Table
update getSheetId msg (Table data) =
    Table <| updateData getSheetId msg data


updateData : (Types.Name -> Maybe Types.SheetId) -> Msg -> TableData -> TableData
updateData getSheetId msg data =
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

        setNewFieldType type_ d =
            { d | newField = setFieldType type_ data.newField }

        setFieldType type_ field =
            { field | fieldType = type_ }
    in
    case msg of
        SetTableState state ->
            { data | state = state }

        UpdateNewRowField fieldName fieldValue ->
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
                -- Enter key, insert a new row
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

        OnClickAddFieldBtn ->
            if isValidField data.newField then
                { data
                    | newField = emptyField
                    , fields = data.fields ++ [ data.newField ]
                }

            else
                data

        OnInputNewFieldName name ->
            let
                newField =
                    data.newField
            in
            { data | newField = { newField | name = name } }

        OnClickRemoveColumnBtn name ->
            { data
                | fields = L.filter (.name >> (/=) name) data.fields
                , rows = L.map (\r -> { r | cells = D.remove name r.cells }) data.rows
            }

        OnClickNewFieldTypeBtn ->
            commit data
                |> (setNewFieldType <|
                        case data.newField.fieldType of
                            DataField _ ->
                                FormulaField (Cell.fromSource getSheetId "")

                            FormulaField _ ->
                                DataField StringType
                   )

        OnClickNewFieldDataTypeBtn ->
            commit data
                |> (case data.newField.fieldType of
                        FormulaField _ ->
                            identity

                        DataField dataType ->
                            setNewFieldType <|
                                case dataType of
                                    IntType ->
                                        DataField StringType

                                    StringType ->
                                        DataField IntType
                   )

        OnInputNewFieldFormula input ->
            setNewFieldType (FormulaField (Cell.fromSource getSheetId input)) data


view : Config msg -> Table -> Html msg
view config (Table ({ newField } as data)) =
    H.div
        [ css
            [ displayFlex
            , flexDirection Css.row
            , height (pct 100)
            ]
        ]
        [ tableView config data
        , newFieldView newField config.getSheetName config.toMsg
        ]


tableView : Config msg -> TableData -> Html msg
tableView config ({ rows, state } as data) =
    H.div
        [ css
            [ flex2 (int 1) (int 1)
            , overflow auto
            ]
        ]
        [ T.view (sortableTableConfig config data) state rows |> H.fromUnstyled ]


newFieldView : Field -> (Types.SheetId -> Maybe Types.Name) -> (Msg -> msg) -> Html msg
newFieldView newField getSheetName toMsg =
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
        [ input [ value newField.name, onInput (OnInputNewFieldName >> toMsg) ] []
        , H.button [ onClick (OnClickNewFieldTypeBtn |> toMsg) ]
            [ case newField.fieldType of
                FormulaField _ ->
                    text "Formula"

                DataField _ ->
                    text "Data"
            ]
        , case newField.fieldType of
            FormulaField cell ->
                input
                    [ value
                        (Cell.sourceView getSheetName cell
                            -- This case should be logged elswhere than in a view.
                            -- Here, we show the error loud and clear to prevent
                            -- corrupting more data.
                            |> Maybe.withDefault "error : found an orphan sheet id reference"
                        )
                    , onInput (OnInputNewFieldFormula >> toMsg)
                    ]
                    []

            DataField dataType ->
                H.button [ onClick (OnClickNewFieldDataTypeBtn |> toMsg) ]
                    [ case dataType of
                        IntType ->
                            text "Int"

                        StringType ->
                            text "String"
                    ]
        , H.button [ onClick (OnClickAddFieldBtn |> toMsg) ] [ text "add" ]
        ]


type alias Resolver =
    Types.LocatedName -> ValueOrError


type alias Config msg =
    { toMsg : Msg -> msg
    , resolveAbsolute : Resolver
    , getSheetName : Types.SheetId -> Maybe Types.Name
    }


sortableTableConfig : Config msg -> TableData -> T.Config Row msg
sortableTableConfig { toMsg, resolveAbsolute } { fields, editedCell } =
    let
        toColumn field =
            T.veryCustomColumn
                { name = field.name
                , sorter = T.decreasingOrIncreasingBy (.cells >> D.get field.name >> M.withDefault "")
                , viewData = fieldView field
                }

        defaultCellView name { id, cells } =
            text
                >> L.singleton
                >> span
                    [ onClick
                        (D.get name cells
                            |> M.withDefault ""
                            |> EditCell id name
                            |> toMsg
                        )
                    ]

        cellInput content =
            H.input
                [ onInput (UpdateEditedCell >> toMsg)
                , value content
                ]
                []

        fieldView : Field -> Row -> T.HtmlDetails msg
        fieldView field row =
            evalField resolveAbsolute fields [] field row
                |> Types.valueOrErrorToString
                |> (case field.fieldType of
                        DataField _ ->
                            case editedCell of
                                Just ( cellRef, content ) ->
                                    if cellRef == ( row.id, field.name ) then
                                        always (cellInput content)

                                    else
                                        defaultCellView field.name row

                                Nothing ->
                                    defaultCellView field.name row

                        FormulaField _ ->
                            text
                                >> L.singleton
                                >> span []
                   )
                |> H.toUnstyled
                |> L.singleton
                |> T.HtmlDetails []

        customizations =
            { defaultCustomizations
                | tfoot = Just (tfoot toMsg fields)
                , thead = thead fields toMsg
            }
    in
    T.customConfig
        { toId = .id >> String.fromInt
        , toMsg = SetTableState >> toMsg
        , columns = fields |> L.map toColumn
        , customizations = customizations
        }


eval : Resolver -> Table -> Types.Table
eval resolveAbsolute (Table data) =
    let
        evalRow row =
            data.fields
                |> L.map (\f -> ( f.name, evalField resolveAbsolute data.fields [] f row ))
                |> D.fromList
    in
    { fields = data.fields |> List.map .name
    , rows = data.rows |> L.map evalRow
    }


evalField : Resolver -> List Field -> List Types.LocatedName -> Field -> Row -> ValueOrError
evalField resolveAbsolute fields ancestors field row =
    let
        resolveRelative : Name -> ValueOrError
        resolveRelative name =
            fields
                |> L.filter (.name >> (==) name)
                |> L.head
                |> Result.fromMaybe (Types.UndefinedLocalReferenceError name)
                |> Result.andThen (\f -> evalField resolveAbsolute fields (fieldRef :: ancestors) f row)

        context : AST.Context Types.SheetId
        context =
            { resolveGlobalReference = resolveAbsolute
            , resolveLocalReference = resolveRelative
            }

        evalDataField dataType =
            D.get field.name row.cells
                |> M.withDefault ""
                |> (case dataType of
                        StringType ->
                            Types.StringValue >> Ok

                        IntType ->
                            AST.parseInt
                                >> Result.mapError (always Types.ParsingError)
                                >> Result.map Types.IntValue
                   )

        fieldRef =
            ( -1
              -- TODO find a better way to reference local values.
            , field.name
            )

        go () =
            case field.fieldType of
                DataField dataType ->
                    evalDataField dataType

                FormulaField cell ->
                    Cell.eval context cell
    in
    AST.checkCycle fieldRef ancestors go


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
                            , onInput (UpdateNewRowField f.name >> toMsg)
                            , onKeyDown (KeyDown >> toMsg)
                            ]
                            []
                        ]
                        |> H.toUnstyled
                )
        )


thead fields toMsg headers =
    let
        onKeyDown tagger =
            on "keydown" (Decode.map tagger keyCode)
    in
    T.HtmlDetails [] <|
        L.map H.toUnstyled
            [ H.tr [] (List.map (theadHelp toMsg) headers)
            , H.tr []
                (fields
                    |> L.map
                        (\f ->
                            td [ css [ position sticky, bottom (px 0) ] ]
                                [ input
                                    [ value f.edit
                                    , onInput (UpdateNewRowField f.name >> toMsg)
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
            span [ onClick (OnClickRemoveColumnBtn name |> toMsg) ] [ text "X" ]
    in
    th [ css [ position sticky, top (px 0) ] ] (text name :: sortArrow [ deleteButton ])
