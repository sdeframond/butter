module MyTable exposing
    ( Msg
    , Table
    , empty
    , update
    , view
    )

import Css exposing (..)
import Dict as D exposing (Dict)
import AST exposing (Memo)
import Types exposing (DataType(..), Error(..), LocatedName, Name, ValueOrError)
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
    , newField : Field
    }


type alias Field =
    { name : String
    , edit : String
    , fieldType : FieldType
    }


type FieldType
    = DataField DataType
    | FormulaField String


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

                FormulaField formula ->
                    formula /= ""
           )


type alias Row =
    { id : Int, cells : Dict String String }


type Msg
    = SetTableState T.State
    | UpdateEdit String String
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

        setNewFieldType type_ d =
            { d | newField = setFieldType type_ data.newField }

        setFieldType type_ field =
            { field | fieldType = type_ }
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
                                FormulaField ""

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
            setNewFieldType (FormulaField input) data


view : (Msg -> msg) -> Table -> Html msg
view toMsg (Table ({ newField } as data)) =
    H.div
        [ css
            [ displayFlex
            , flexDirection Css.row
            , height (pct 100)
            ]
        ]
        [ tableView toMsg data
        , newFieldView newField toMsg
        ]


tableView : (Msg -> msg) -> TableData -> Html msg
tableView toMsg ({ rows, state } as data) =
    H.div
        [ css
            [ flex2 (int 1) (int 1)
            , overflow auto
            ]
        ]
        [ T.view (tableConfig toMsg data) state rows |> H.fromUnstyled ]


newFieldView : Field -> (Msg -> msg) -> Html msg
newFieldView newField toMsg =
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
            FormulaField formula ->
                input
                    [ value formula
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


tableConfig : (Msg -> msg) -> TableData -> T.Config Row msg
tableConfig toMsg { fields, editedCell } =
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
            evalField fields [] D.empty field row
                |> Tuple.first
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


evalField : List Field -> List LocatedName -> AST.Memo -> Field -> Row -> ( ValueOrError, AST.Memo )
evalField fields ancestors memo field row =
    let
        resolveRelative : AST.Memo -> Name -> ( ValueOrError, AST.Memo )
        resolveRelative memo_ name =
            D.get name (fields |> L.map (\f -> ( f.name, f )) |> D.fromList)
                |> Result.fromMaybe (Types.UndefinedNameError ( "", name ))
                |> Result.map (\f -> evalField fields (( "", name ) :: ancestors) memo_ f row)
                |> (\result ->
                        case result of
                            Err e ->
                                ( Err e, memo_ )

                            Ok v ->
                                v
                   )

        context : AST.Context
        context =
            { resolveAbsolute = \memo_ ( _, name ) -> resolveRelative memo_ name
            , resolveRelative = resolveRelative
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

        evalFormulaField formula =
            AST.parseCell formula
                |> Result.mapError (always Types.ParsingError)
                |> Result.map (AST.eval context memo)
                |> (\result ->
                        case result of
                            Err e ->
                                ( Err e, memo )

                            Ok tuple ->
                                tuple
                   )

        go () =
            case field.fieldType of
                DataField dataType ->
                    evalDataField dataType |> (\vOrE -> ( vOrE, memo ))

                FormulaField formula ->
                    evalFormulaField formula
    in
    AST.useMemoAndCheckCycle ( "", field.name ) memo ancestors go


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
            span [ onClick (OnClickRemoveColumnBtn name |> toMsg) ] [ text "X" ]
    in
    th [ css [ position sticky, top (px 0) ] ] (text name :: sortArrow [ deleteButton ])
