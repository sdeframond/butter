module MyTable exposing
    ( Msg(..)
    , Table
    , applyContentFrom
    , decoder
    , empty
    , encode
    , eval
    , update
    , view
    )

import Core.Formula as Formula exposing (Formula)
import Core.Name as Name exposing (Name)
import Core.PositiveInt as PositiveInt exposing (PositiveInt)
import Core.Types as Types exposing (DataType(..), Error(..), ValueOrError)
import Core.UndoCmd as UndoCmd
import Css exposing (..)
import DecodeHelpers
import Html
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
import Json.Encode as Encode
import List as L
import Maybe as M
import Table as T exposing (defaultCustomizations)
import Ui



-- MODEL


type Table
    = Table TableData


type alias TableData =
    { state : T.State
    , editedCell : Maybe ( ( PositiveInt, Name ), String )
    , fields : List FieldDefinition
    , rows : List Row
    , nextId : PositiveInt
    , fieldForm : FieldForm
    }


empty : Table
empty =
    Table
        { fields = []
        , rows = []
        , state = T.initialSort ""
        , nextId = PositiveInt.one
        , editedCell = Nothing
        , fieldForm = emptyFieldForm
        }


type alias FieldDefinition =
    { name : Name
    , edit : String
    , fieldType : FieldType
    }


type alias FieldForm =
    { name : Maybe Name
    , fieldType : FieldType
    }


formToField : FieldForm -> Maybe FieldDefinition
formToField form =
    form.name
        |> Maybe.map FieldDefinition
        |> Maybe.map (\f -> f "" form.fieldType)


type FieldType
    = DataField DataType
    | FormulaField Formula


emptyFieldForm : FieldForm
emptyFieldForm =
    -- { name = Nothing, fieldType = DataField StringType }
    { name = Nothing, fieldType = FormulaField (Formula.fromSource (always Nothing) "") }


type alias Row =
    { id : PositiveInt, data : Name.Store String }


applyContentFrom : Table -> Table -> Table
applyContentFrom (Table inData) (Table currentData) =
    Table { inData | fieldForm = currentData.fieldForm }



--UPDATE


type Msg
    = SetTableState T.State
    | UpdateNewRowField Name String
    | KeyDown Int
    | EditCell PositiveInt Name String
    | UpdateEditedCell String
    | OnClickAddFieldBtn
    | OnInputNewFieldName String
    | OnClickRemoveColumnBtn String
    | OnClickNewFieldTypeBtn
    | OnClickNewFieldDataTypeBtn
    | OnInputNewFieldFormula String


update : (Name -> Maybe Types.SheetId) -> Msg -> Table -> ( Table, UndoCmd.Cmd )
update getSheetId msg (Table data) =
    updateData getSheetId msg data
        |> Tuple.mapFirst Table


updateData : (Name -> Maybe Types.SheetId) -> Msg -> TableData -> ( TableData, UndoCmd.Cmd )
updateData getSheetId msg data =
    let
        commit : TableData -> ( TableData, UndoCmd.Cmd )
        commit d =
            case d.editedCell of
                Nothing ->
                    ( d, UndoCmd.None )

                Just ( ( rowId, fieldName ), content ) ->
                    ( { d
                        | editedCell = Nothing
                        , rows =
                            L.map
                                (\row ->
                                    if row.id == rowId then
                                        { row | data = Name.insert fieldName content row.data }

                                    else
                                        row
                                )
                                d.rows
                      }
                    , UndoCmd.New
                    )

        setNewFieldType type_ d =
            { d | fieldForm = setFieldType type_ data.fieldForm }

        setFieldType type_ field =
            { field | fieldType = type_ }
    in
    case msg of
        SetTableState state ->
            ( { data | state = state }, UndoCmd.New )

        UpdateNewRowField fieldName fieldValue ->
            let
                updateField field =
                    if field.name == fieldName then
                        { field | edit = fieldValue }

                    else
                        field
            in
            ( { data | fields = L.map updateField data.fields }, UndoCmd.None )

        KeyDown code ->
            let
                resetEdit field =
                    { field | edit = "" }

                editsToRow =
                    { id = data.nextId
                    , data =
                        data.fields
                            |> L.map (\f -> ( f.name, f.edit ))
                            |> Name.fromList
                    }
            in
            if code == 13 then
                -- Enter key, insert a new row
                ( { data
                    | fields = L.map resetEdit data.fields
                    , rows = editsToRow :: data.rows
                    , nextId = PositiveInt.next data.nextId
                  }
                , UndoCmd.New
                )

            else
                ( data, UndoCmd.None )

        EditCell rowId fieldName content ->
            let
                ( commited, undoCmd ) =
                    commit data
            in
            ( { commited | editedCell = Just ( ( rowId, fieldName ), content ) }, undoCmd )

        UpdateEditedCell content ->
            case data.editedCell of
                Nothing ->
                    ( data, UndoCmd.None )

                Just ( cellRef, _ ) ->
                    ( { data | editedCell = Just ( cellRef, content ) }, UndoCmd.None )

        OnClickAddFieldBtn ->
            formToField data.fieldForm
                |> Maybe.map
                    (\field ->
                        { data
                            | fieldForm = emptyFieldForm
                            , fields = data.fields ++ [ field ]
                        }
                    )
                |> Maybe.withDefault data
                |> (\d -> ( d, UndoCmd.New ))

        OnInputNewFieldName input ->
            let
                form =
                    data.fieldForm
            in
            ( { data | fieldForm = { form | name = Name.fromString input } }, UndoCmd.None )

        OnClickRemoveColumnBtn nameStr ->
            ( { data
                | fields = L.filter (.name >> Name.matchesString nameStr >> not) data.fields
                , rows = L.map (\r -> { r | data = Name.removeString nameStr r.data }) data.rows
              }
            , UndoCmd.New
            )

        OnClickNewFieldTypeBtn ->
            commit data
                |> Tuple.mapFirst
                    (setNewFieldType <|
                        case data.fieldForm.fieldType of
                            DataField _ ->
                                FormulaField (Formula.fromSource getSheetId "")

                            FormulaField _ ->
                                DataField StringType
                    )

        OnClickNewFieldDataTypeBtn ->
            commit data
                |> Tuple.mapFirst
                    (case data.fieldForm.fieldType of
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
            ( setNewFieldType (FormulaField (Formula.fromSource getSheetId input)) data, UndoCmd.None )



-- VIEW


view : Config msg -> Table -> Html msg
view config ((Table data) as table) =
    Ui.fullRow []
        [ tableView config data
        , Ui.column []
            [ formView data.fieldForm config.getSheetName config.toMsg
            , addPivotTableButton table config.insertPivotTable
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


formView : FieldForm -> (Types.SheetId -> Maybe Name) -> (Msg -> msg) -> Html msg
formView form getSheetName toMsg =
    H.div []
        [ input
            [ form.name |> Maybe.map Name.toString |> Maybe.withDefault "" |> value
            , onInput (OnInputNewFieldName >> toMsg)
            ]
            []
        , Ui.button [ onClick (OnClickNewFieldTypeBtn |> toMsg) ]
            [ case form.fieldType of
                FormulaField _ ->
                    text "Formula"

                DataField _ ->
                    text "Data"
            ]
        , case form.fieldType of
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
                Ui.button [ onClick (OnClickNewFieldDataTypeBtn |> toMsg) ]
                    [ case dataType of
                        IntType ->
                            text "Int"

                        StringType ->
                            text "String"
                    ]
        , Ui.button [ onClick (OnClickAddFieldBtn |> toMsg) ] [ text "add" ]
        ]


addPivotTableButton : Table -> (Table -> msg) -> Html msg
addPivotTableButton table makePivotTable =
    let
        lazyOnClick : (() -> msg) -> H.Attribute msg
        lazyOnClick tagger =
            on "click" (Decode.succeed () |> Decode.map tagger)
    in
    Ui.button [ lazyOnClick (\_ -> makePivotTable table) ] [ text "Make PivotTable" ]


type alias Context =
    { resolveAbsolute : Types.LocatedName -> List Types.LocatedName -> ValueOrError
    , prefix : Types.SheetId
    }


type alias Config msg =
    { toMsg : Msg -> msg
    , context : Context
    , getSheetName : Types.SheetId -> Maybe Name
    , insertPivotTable : Table -> msg
    }


sortableTableConfig : Config msg -> TableData -> T.Config Row msg
sortableTableConfig { toMsg, context } { fields, editedCell } =
    let
        toColumn field =
            T.veryCustomColumn
                { name = Name.toString field.name
                , sorter =
                    T.decreasingOrIncreasingBy
                        (.data
                            >> Name.get field.name
                            >> M.withDefault ""
                        )
                , viewData = fieldView field
                }

        defaultFieldView name { id, data } =
            text
                >> L.singleton
                >> span
                    [ onClick
                        (Name.get name data
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
        { toId = .id >> PositiveInt.toString
        , toMsg = SetTableState >> toMsg
        , columns = fields |> L.map toColumn
        , customizations = customizations
        }


tfoot : (Msg -> msg) -> List FieldDefinition -> T.HtmlDetails msg
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


thead : List FieldDefinition -> (Msg -> msg) -> List ( String, T.Status, Html.Attribute msg ) -> T.HtmlDetails msg
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


theadHelp : (Msg -> msg) -> ( String, T.Status, Html.Attribute msg ) -> Html msg
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
                |> Name.fromList
    in
    { fields = data.fields |> List.map .name
    , rows = data.rows |> L.map evalRow
    }


evalField : Context -> List FieldDefinition -> List Types.LocatedName -> FieldDefinition -> Row -> ValueOrError
evalField context fields ancestors field row =
    let
        resolveRelative : Name -> List ( Types.SheetId, Name ) -> ValueOrError
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
            Name.get field.name row.data
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



-- JSON


jsonKeys :
    { data : String
    , dataType : String
    , edit : String
    , fieldForm : String
    , fields : String
    , formula : String
    , id : String
    , name : String
    , nextId : String
    , rows : String
    , fieldType : String
    , editedCell : String
    }
jsonKeys =
    { editedCell = "editedCell"
    , data = "data"
    , dataType = "dataType"
    , edit = "edit"
    , fieldForm = "fieldForm"
    , fields = "fields"
    , formula = "formula"
    , id = "id"
    , name = "name"
    , nextId = "nextId"
    , rows = "rows"
    , fieldType = "type"
    }


decoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder Table
decoder getSheetId =
    Decode.map Table (tableDataDecoder getSheetId)


tableDataDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder TableData
tableDataDecoder getSheetId =
    Decode.map5 (TableData (T.initialSort ""))
        (Decode.field jsonKeys.editedCell <| Decode.nullable editedCellDecoder)
        (Decode.field jsonKeys.fields <| Decode.list (fieldDefinitionDecoder getSheetId))
        (Decode.field jsonKeys.rows <| Decode.list rowDecoder)
        (Decode.field jsonKeys.nextId PositiveInt.decoder)
        (Decode.field jsonKeys.fieldForm <| fieldFormDecoder getSheetId)


editedCellDecoder : Decode.Decoder ( ( PositiveInt, Name ), String )
editedCellDecoder =
    Decode.map2 Tuple.pair
        (Decode.map2 Tuple.pair
            (Decode.field jsonKeys.id PositiveInt.decoder)
            (Decode.field jsonKeys.name Name.decoder)
        )
        (Decode.field jsonKeys.edit Decode.string)


fieldDefinitionDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder FieldDefinition
fieldDefinitionDecoder getSheetId =
    Decode.map3 FieldDefinition
        (Decode.field jsonKeys.name Name.decoder)
        (Decode.field jsonKeys.edit Decode.string)
        |> fieldTypeSwitch getSheetId


fieldTypeSwitch : (Name -> Maybe Types.SheetId) -> (Decode.Decoder FieldType -> Decode.Decoder a) -> Decode.Decoder a
fieldTypeSwitch getSheetId makeField =
    Decode.field jsonKeys.fieldType Decode.string
        |> DecodeHelpers.switch "Invalid field type"
            [ ( jsonKeys.formula
              , Formula.decoder getSheetId
                    |> Decode.map FormulaField
                    |> Decode.field jsonKeys.formula
              )
            , ( jsonKeys.data
              , Types.dataTypeDecoder
                    |> Decode.map DataField
                    |> Decode.field jsonKeys.dataType
              )
            ]
        |> makeField


rowDecoder : Decode.Decoder Row
rowDecoder =
    Decode.map2 Row
        (Decode.field jsonKeys.id PositiveInt.decoder)
        (Decode.field jsonKeys.data <| Name.storeDecoder Decode.string)


fieldFormDecoder : (Name -> Maybe Types.SheetId) -> Decode.Decoder FieldForm
fieldFormDecoder getSheetId =
    Decode.map2 FieldForm
        (Decode.field jsonKeys.name (Decode.nullable Name.decoder))
        |> fieldTypeSwitch getSheetId


encode : (Types.SheetId -> Maybe Name) -> Table -> Encode.Value
encode getSheetName (Table data) =
    Encode.object
        [ ( jsonKeys.editedCell, data.editedCell |> Maybe.map encodeEditedCell |> Maybe.withDefault Encode.null )
        , ( jsonKeys.fields, Encode.list (encodeFieldDefinition getSheetName) data.fields )
        , ( jsonKeys.rows, Encode.list encodeRow data.rows )
        , ( jsonKeys.nextId, PositiveInt.encode data.nextId )
        , ( jsonKeys.fieldForm, encodeFieldForm getSheetName data.fieldForm )
        ]


encodeEditedCell : ( ( PositiveInt, Name ), String ) -> Encode.Value
encodeEditedCell ( ( rowId, fieldName ), input ) =
    Encode.object
        [ ( jsonKeys.id, PositiveInt.encode rowId )
        , ( jsonKeys.name, Name.encode fieldName )
        , ( jsonKeys.edit, Encode.string input )
        ]


encodeFieldDefinition : (Types.SheetId -> Maybe Name) -> FieldDefinition -> Encode.Value
encodeFieldDefinition getSheetName definition =
    Encode.object
        ([ ( jsonKeys.name, Name.encode definition.name )
         , ( jsonKeys.edit, Encode.string definition.edit )
         ]
            ++ encodeFieldType getSheetName definition.fieldType
        )


encodeFieldType : (Types.SheetId -> Maybe Name) -> FieldType -> List ( String, Encode.Value )
encodeFieldType getSheetName fieldType =
    [ ( jsonKeys.fieldType
      , Encode.string <|
            case fieldType of
                DataField _ ->
                    jsonKeys.data

                FormulaField _ ->
                    jsonKeys.formula
      )
    , case fieldType of
        DataField dataType ->
            ( jsonKeys.dataType, Types.encodeDataType dataType )

        FormulaField formula ->
            ( jsonKeys.formula, Formula.encode getSheetName formula )
    ]


encodeRow : Row -> Encode.Value
encodeRow row =
    Encode.object
        [ ( jsonKeys.id, PositiveInt.encode row.id )
        , ( jsonKeys.data, Name.encodeStore Encode.string row.data )
        ]


encodeFieldForm : (Types.SheetId -> Maybe Name) -> FieldForm -> Encode.Value
encodeFieldForm getSheetName form =
    Encode.object
        (( jsonKeys.name
         , form.name
            |> Maybe.map Name.encode
            |> Maybe.withDefault Encode.null
         )
            :: encodeFieldType getSheetName form.fieldType
        )
