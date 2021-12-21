module Core.DocumentWithUndo exposing
    ( Model
    , commitEditedSheetNames
    , decoder
    , editCurrentSheetName
    , encode
    , eval
    , fromBytes
    , getCurrentSheet
    , getCurrentSheetEditStatus
    , getCurrentSheetId
    , getCurrentSheetName
    , getSheetNameById
    , init
    , insertSheet
    , merge
    , redo
    , removeSheet
    , toBytes
    , undo
    , updateCurrentEditedSheetName
    , updateCurrentSheet
    , zipMapSheets
    )

import Bytes exposing (Bytes)
import Core.Document as Document
import Json.Decode exposing (Decoder, Value)
import Name exposing (Name)
import Sheet
import Types
import UndoList exposing (UndoList)
import UndoList.Decode
import UndoList.Encode


type alias Model =
    UndoList Document.Model


init : Model
init =
    UndoList.fresh Document.init


merge : Model -> Model -> Model
merge newList { present } =
    UndoList.map (\d -> Document.merge d present) newList


toBytes : Model -> Bytes
toBytes { present } =
    Document.toBytes present


fromBytes : Bytes -> Maybe Model
fromBytes =
    Document.fromBytes >> Maybe.map UndoList.fresh


undo : Model -> Model
undo =
    UndoList.undo


redo : Model -> Model
redo =
    UndoList.redo


new : Document.Model -> UndoList Document.Model -> UndoList Document.Model
new doc undoList =
    UndoList.new doc
        (UndoList.mapPresent Document.cancelEdits undoList)


decoder : Decoder Model
decoder =
    UndoList.Decode.undolist Document.decoder


encode : Model -> Value
encode =
    UndoList.map Document.encode >> UndoList.Encode.undolist


getCurrentSheet : Model -> Sheet.Sheet
getCurrentSheet =
    .present >> Document.getCurrentSheet


updateCurrentSheet : ((Name -> Maybe Types.SheetId) -> Sheet.Sheet -> ( Sheet.Sheet, cmd )) -> Model -> ( Model, cmd )
updateCurrentSheet func model =
    let
        ( present, cmd ) =
            Document.updateCurrentSheet func model.present
    in
    ( { model | present = present }, cmd )


insertSheet : Sheet.Params -> Model -> Model
insertSheet params model =
    new (Document.insertSheet params model.present) model


removeSheet : Types.SheetId -> Model -> Model
removeSheet sheetId model =
    new (Document.removeSheet sheetId model.present) model


editCurrentSheetName : Model -> Model
editCurrentSheetName =
    UndoList.mapPresent Document.editCurrentSheetName


commitEditedSheetNames : Model -> Model
commitEditedSheetNames model =
    let
        cancelled =
            UndoList.mapPresent Document.cancelEdits model

        committedDoc =
            Document.commitEditedSheetNames model.present
    in
    if committedDoc == cancelled.present then
        cancelled

    else
        new committedDoc cancelled


updateCurrentEditedSheetName : String -> Model -> Model
updateCurrentEditedSheetName str model =
    UndoList.mapPresent (Document.updateCurrentEditedSheetName str) model


getCurrentSheetEditStatus : Model -> Maybe String
getCurrentSheetEditStatus { present } =
    Document.getCurrentSheetEditStatus present


getSheetNameById : Types.SheetId -> Model -> Maybe Name
getSheetNameById id { present } =
    Document.getSheetNameById id present


getCurrentSheetId : Model -> Types.SheetId
getCurrentSheetId { present } =
    Document.getCurrentSheetId present


getCurrentSheetName : Model -> Name
getCurrentSheetName { present } =
    Document.getCurrentSheetName present


eval : Model -> Types.LocatedName -> List Types.LocatedName -> Types.ValueOrError
eval { present } =
    Document.eval present


zipMapSheets : (Model -> Bool -> a) -> Model -> List a
zipMapSheets f model =
    let
        wrapped present isCurrent =
            f { model | present = present } isCurrent
    in
    Document.zipMapSheets wrapped model.present
