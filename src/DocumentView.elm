module DocumentView exposing
    ( Model
    , Msg
    , applyContentFrom
    , decoder
    , encode
    , fromBytes
    , init
    , redo
    , subscriptions
    , toBytes
    , undo
    , update
    , view
    )

import Bytes exposing (Bytes)
import Core.DocumentWithUndo as Document
import Core.Name as Name
import Core.Types as Types
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Ports
import Sheet
import Ui


type alias Model =
    Document.Model


init : Model
init =
    Document.init


applyContentFrom : Model -> Model -> Model
applyContentFrom =
    Document.applyContentFrom


toBytes : Model -> Bytes
toBytes =
    Document.toBytes


fromBytes : Bytes -> Maybe Model
fromBytes =
    Document.fromBytes


undo : Model -> ( Model, Cmd Msg )
undo =
    Document.undo


redo : Model -> ( Model, Cmd Msg )
redo =
    Document.redo


decoder : Decoder Model
decoder =
    Document.decoder


encode : Model -> Value
encode =
    Document.encode



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Document.getCurrentSheet model
            |> Sheet.subscriptions
            |> Sub.map SheetMsg
        , Ports.docDiffReceiver DiffReceived
        ]



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
    | InsertSheet Sheet.Params
    | RemoveSheet Types.SheetId
    | EditCurrentSheetName
    | UpdateEditedSheetName String
    | DiffReceived Json.Decode.Value
    | SetModel ( Model, Cmd Msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetMsg sheetMsg ->
            let
                updateSheet getSheetId sheet =
                    Sheet.update getSheetId sheetMsg sheet
            in
            Document.updateCurrentSheet SheetMsg updateSheet model

        InsertSheet params ->
            Document.insertSheet params model

        RemoveSheet sheetId ->
            Document.removeSheet sheetId model

        EditCurrentSheetName ->
            ( Document.editCurrentSheetName model, Cmd.none )

        UpdateEditedSheetName input ->
            ( Document.updateCurrentEditedSheetName input model, Cmd.none )

        DiffReceived value ->
            let
                applyDiff diff =
                    ( Document.applyDiff diff model |> Result.withDefault model, Cmd.none )
            in
            Json.Decode.decodeValue (Document.diffDecoder model) value
                |> Result.map applyDiff
                |> Result.withDefault ( model, Cmd.none )

        SetModel m ->
            m



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ displayFlex
            , flexDirection column
            , height (pct 100)
            , width (pct 100)
            , overflow hidden
            ]
        ]
        [ viewCurrentSheet model
        , Ui.row
            [ css
                [ borderTop3 (px 1) solid (rgb 0 0 0)
                , justifyContent spaceBetween
                , padding2 (px 10) (px 10)
                ]
            ]
            [ sheetSelector model
            ]
        ]


viewCurrentSheet : Model -> Html Msg
viewCurrentSheet model =
    let
        sheetConfig : Sheet.Config Msg
        sheetConfig =
            { toMsg = SheetMsg
            , insertPivotTable =
                Sheet.allParams.pivotTable
                    >> InsertSheet
            , getSheetName = \id -> Document.getSheetNameById id model
            , context =
                { prefix = Document.getCurrentSheetId model
                , ancestors = []
                , resolveGlobalReference = Document.eval model
                }
            }
    in
    div
        [ css
            [ width (pct 100)
            , height (pct 100)
            , overflow hidden
            ]
        ]
        [ Sheet.view sheetConfig (Document.getCurrentSheet model)
        ]


sheetSelector : Model -> Html Msg
sheetSelector model =
    let
        isEditing =
            Document.getCurrentSheetEditStatus model /= Nothing

        sheetItem : Model -> Bool -> Html Msg
        sheetItem zippedModel isCurrent =
            Ui.editableListItem
                { onSelect =
                    \_ ->
                        SetModel <|
                            if isEditing then
                                zippedModel |> Document.commitEditedSheetNames

                            else
                                ( zippedModel, Cmd.none )
                , onEdit = \_ -> EditCurrentSheetName
                , onRemove = \_ -> zippedModel |> Document.getCurrentSheetId |> RemoveSheet
                , onUpdate = UpdateEditedSheetName
                }
                { name = Document.getCurrentSheetName zippedModel |> Name.toString
                , isCurrent = isCurrent
                , editStatus = Document.getCurrentSheetEditStatus zippedModel
                }

        addSheet msg label =
            Ui.button
                [ Events.onClick msg
                ]
                [ text label ]
    in
    Ui.row []
        (addSheet (InsertSheet Sheet.allParams.table) "+table"
            :: addSheet (InsertSheet Sheet.allParams.grid) "+grid"
            :: Document.zipMapSheets sheetItem model
        )
