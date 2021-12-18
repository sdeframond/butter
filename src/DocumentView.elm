module DocumentView exposing
    ( Model
    , Msg
    , decoder
    , encode
    , fromBytes
    , init
    , merge
    , redo
    , subscriptions
    , toBytes
    , undo
    , update
    , view
    )

import Bytes exposing (Bytes)
import Core.Document as Document exposing (Model)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Name
import Sheet
import Types
import Ui


type alias Model =
    Document.Model


init : Model
init =
    Document.init


merge : Model -> Model -> Model
merge =
    Document.merge


toBytes : Model -> Bytes
toBytes =
    Document.toBytes


fromBytes : Bytes -> Maybe Model
fromBytes =
    Document.fromBytes


undo : a
undo =
    Debug.todo "undo"


redo : a
redo =
    Debug.todo "redo"


decoder : Decoder Model
decoder =
    Document.decoder


encode : Model -> Value
encode =
    Document.encode



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Document.getCurrentSheet model
        |> Sheet.subscriptions
        |> Sub.map SheetMsg



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
    | InsertSheet Sheet.Params
    | RemoveSheet Types.SheetId
    | EditCurrentSheetName
    | UpdateEditedSheetName String
    | SetModel Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SheetMsg sheetMsg ->
            Document.updateCurrentSheet sheetMsg model
                |> Tuple.mapSecond (Cmd.map SheetMsg)

        InsertSheet params ->
            ( Document.insertSheet params model, Cmd.none )

        RemoveSheet sheetId ->
            ( Document.removeSheet sheetId model, Cmd.none )

        EditCurrentSheetName ->
            ( Document.editCurrentSheetName model, Cmd.none )

        UpdateEditedSheetName input ->
            ( Document.updateCurrentEditedSheetName input model, Cmd.none )

        SetModel m ->
            ( m, Cmd.none )



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
        sheetItem : Model -> Bool -> Html Msg
        sheetItem zippedModel isCurrent =
            Ui.editableListItem
                { onSelect = \_ -> zippedModel |> Document.commitEditedSheetNames |> SetModel
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
