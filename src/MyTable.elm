module MyTable exposing
    ( Msg
    , Table
    , empty
    , eval
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import Formula exposing (Formula)
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
import Ui



-- MODEL


type Table
    = Table TableData


type alias TableData =
    { fields : List FieldDefinition
    , rows : List Row
    , state : T.State
    , nextId : Int
    , editedCell : Maybe ( ( Int, String ), String )
    , newField : FieldDefinition
    }


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


type alias FieldDefinition =
    { name : String
    , edit : String
    , fieldType : FieldType
    }


type FieldType
    = DataField DataType
    | FormulaField Formula


emptyField : FieldDefinition
emptyField =
    { name = "", edit = "", fieldType = DataField StringType }


isValidField : FieldDefinition -> Bool
isValidField { name, fieldType } =
    name
        /= ""
        && (case fieldType of
                DataField _ ->
                    True

                FormulaField formula ->
                    Formula.isValid formula
           )


type alias Row =
    { id : Int, data : Dict String String }



--UPDATE


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
                                        { row | data = D.insert fieldName content row.data }

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
                    , data =
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
                , rows = L.map (\r -> { r | data = D.remove name r.data }) data.rows
            }

        OnClickNewFieldTypeBtn ->
            commit data
                |> (setNewFieldType <|
                        case data.newField.fieldType of
                            DataField _ ->
                                FormulaField (Formula.fromSource getSheetId "")

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
            setNewFieldType (FormulaField (Formula.fromSource getSheetId input)) data



-- VIEW


view : Config msg -> Table -> Html msg
view config (Table ({ newField } as data)) =
    Ui.row
        [ tableView config data
        , Ui.column
            [ newFieldView newField config.getSheetName config.toMsg
            , addPivotTableButton config.makePivotTableMsg
            ]
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


newFieldView : FieldDefinition -> (Types.SheetId -> Maybe Types.Name) -> (Msg -> msg) -> Html msg
newFieldView newField getSheetName toMsg =
    H.div []
        [ input [ value newField.name, onInput (OnInputNewFieldName >> toMsg) ] []
        , H.button [ onClick (OnClickNewFieldTypeBtn |> toMsg) ]
            [ case newField.fieldType of
                FormulaField _ ->
                    text "Formula"

                DataField _ ->
                    text "Data"
            ]
        , case newField.fieldType of
            FormulaField formula ->
                input
                    [ value
                        (Formula.sourceView getSheetName formula
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


addPivotTableButton : msg -> Html msg
addPivotTableButton makePivotTableMsg =
    H.button [ onClick makePivotTableMsg ] [ text "Make PivotTable" ]


type alias Context =
    { resolveAbsolute : Types.LocatedName -> List Types.LocatedName -> ValueOrError
    , prefix : Types.SheetId
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , context : Context
    , getSheetName : Types.SheetId -> Maybe Types.Name
    , makePivotTableMsg : msg
    }


sortableTableConfig : Config msg -> TableData -> T.Config Row msg
sortableTableConfig { toMsg, context } { fields, editedCell } =
    let
        toColumn field =
            T.veryCustomColumn
                { name = field.name
                , sorter = T.decreasingOrIncreasingBy (.data >> D.get field.name >> M.withDefault "")
                , viewData = fieldView field
                }

        defaultFieldView name { id, data } =
            text
                >> L.singleton
                >> span
                    [ onClick
                        (D.get name data
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

        fieldView : FieldDefinition -> Row -> T.HtmlDetails msg
        fieldView field row =
            evalField context fields [] field row
                |> Types.valueOrErrorToString
                |> (case field.fieldType of
                        DataField _ ->
                            case editedCell of
                                Just ( cellRef, content ) ->
                                    if cellRef == ( row.id, field.name ) then
                                        always (cellInput content)

                                    else
                                        defaultFieldView field.name row

                                Nothing ->
                                    defaultFieldView field.name row

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



-- EVAL


eval : Context -> Table -> Types.Table
eval context (Table data) =
    let
        evalRow row =
            data.fields
                |> L.map (\f -> ( f.name, evalField context data.fields [] f row ))
                |> D.fromList
    in
    { fields = data.fields |> List.map .name
    , rows = data.rows |> L.map evalRow
    }


evalField : Context -> List FieldDefinition -> List Types.LocatedName -> FieldDefinition -> Row -> ValueOrError
evalField context fields ancestors field row =
    let
        resolveRelative : Types.Name -> List ( Types.SheetId, Types.Name ) -> ValueOrError
        resolveRelative name ancestors_ =
            fields
                |> L.filter (.name >> (==) name)
                |> L.head
                |> Result.fromMaybe (Types.UndefinedLocalReferenceError name)
                |> Result.andThen (\f -> evalField context fields ancestors_ f row)

        formulaContext : Formula.Context Types.SheetId
        formulaContext =
            { resolveGlobalReference = context.resolveAbsolute
            , resolveLocalReference = resolveRelative
            , prefix = context.prefix
            , ancestors = ancestors
            }

        evalDataField dataType =
            D.get field.name row.data
                |> M.withDefault ""
                |> (case dataType of
                        StringType ->
                            Types.StringValue >> Ok

                        IntType ->
                            Formula.parseInt
                                >> Result.mapError (always Types.ParsingError)
                                >> Result.map Types.IntValue
                   )
    in
    case field.fieldType of
        DataField dataType ->
            evalDataField dataType

        FormulaField formula ->
            Formula.eval formulaContext formula
