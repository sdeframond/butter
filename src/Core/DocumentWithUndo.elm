module Core.DocumentWithUndo exposing
    ( Model
    , applyContentFrom
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
    , past
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
import Core.UndoCmd as UndoCmd
import Json.Decode exposing (Decoder, Value)
import Json.Encode
import Name exposing (Name)
import Sheet
import Types
import UndoList exposing (UndoList)
import UndoList.Decode
import UndoList.Encode


type Model
    = Model Data


type alias Data =
    { undoList : UndoList Document.Model
    , workingCopy : Document.Model
    }


init : Model
init =
    Model
        { undoList = UndoList.fresh Document.init
        , workingCopy = Document.init
        }


applyContentFrom : Model -> Model -> Model
applyContentFrom (Model remote) (Model origin) =
    Model { remote | workingCopy = Document.applyContentFrom remote.workingCopy origin.workingCopy }


toBytes : Model -> Bytes
toBytes (Model model) =
    Document.toBytes model.workingCopy


fromBytes : Bytes -> Maybe Model
fromBytes =
    Document.fromBytes >> Maybe.map (\doc -> Model { undoList = UndoList.fresh doc, workingCopy = doc })


undo : Model -> Model
undo (Model model) =
    UndoList.undo model.undoList
        |> (\undoList -> Model { undoList = undoList, workingCopy = undoList.present })


redo : Model -> Model
redo (Model model) =
    UndoList.redo model.undoList
        |> (\undoList -> Model { undoList = undoList, workingCopy = undoList.present })


past : Model -> List Document.Model
past (Model data) =
    data.undoList.past


commitDocToUndoList : Document.Model -> Data -> Model
commitDocToUndoList doc model =
    let
        contentHasChanged =
            model.undoList.present /= Document.applyContentFrom doc model.undoList.present
    in
    if contentHasChanged then
        Model
            { undoList = UndoList.new doc model.undoList
            , workingCopy = doc
            }

    else
        Model { model | workingCopy = doc }


commitWorkingCopy : Model -> Model
commitWorkingCopy (Model data) =
    commitDocToUndoList data.workingCopy data


getCurrentSheet : Model -> Sheet.Sheet
getCurrentSheet (Model model) =
    model.workingCopy |> Document.getCurrentSheet


updateCurrentSheet : ((Name -> Maybe Types.SheetId) -> Sheet.Sheet -> ( Sheet.Sheet, ( UndoCmd.Cmd, cmd ) )) -> Model -> ( Model, cmd )
updateCurrentSheet func (Model model) =
    let
        ( newDoc, ( undoCmd, sheetCmd ) ) =
            Document.updateCurrentSheet func model.workingCopy
    in
    case undoCmd of
        UndoCmd.New ->
            ( commitDocToUndoList newDoc model, sheetCmd )

        UndoCmd.None ->
            ( Model { model | workingCopy = newDoc }, sheetCmd )


insertSheet : Sheet.Params -> Model -> Model
insertSheet params model =
    model |> mapWorkingCopy (Document.insertSheet params) |> commitWorkingCopy


removeSheet : Types.SheetId -> Model -> Model
removeSheet sheetId (Model model) =
    commitDocToUndoList (Document.removeSheet sheetId model.workingCopy) model


mapWorkingCopy : (Document.Model -> Document.Model) -> Model -> Model
mapWorkingCopy func (Model data) =
    Model { data | workingCopy = func data.workingCopy }


editCurrentSheetName : Model -> Model
editCurrentSheetName model =
    mapWorkingCopy Document.editCurrentSheetName model


commitEditedSheetNames : Model -> Model
commitEditedSheetNames (Model model) =
    let
        committedDoc =
            Document.commitEditedSheetNames model.workingCopy
    in
    commitDocToUndoList committedDoc model


updateCurrentEditedSheetName : String -> Model -> Model
updateCurrentEditedSheetName str model =
    mapWorkingCopy (Document.updateCurrentEditedSheetName str) model


getCurrentSheetEditStatus : Model -> Maybe String
getCurrentSheetEditStatus (Model { workingCopy }) =
    Document.getCurrentSheetEditStatus workingCopy


getSheetNameById : Types.SheetId -> Model -> Maybe Name
getSheetNameById id (Model { workingCopy }) =
    Document.getSheetNameById id workingCopy


getCurrentSheetId : Model -> Types.SheetId
getCurrentSheetId (Model { workingCopy }) =
    Document.getCurrentSheetId workingCopy


getCurrentSheetName : Model -> Name
getCurrentSheetName (Model { workingCopy }) =
    Document.getCurrentSheetName workingCopy


eval : Model -> Types.LocatedName -> List Types.LocatedName -> Types.ValueOrError
eval (Model { workingCopy }) =
    Document.eval workingCopy


zipMapSheets : (Model -> Bool -> a) -> Model -> List a
zipMapSheets f (Model model) =
    let
        wrapped workingCopy isCurrent =
            f (Model { model | workingCopy = workingCopy }) isCurrent
    in
    Document.zipMapSheets wrapped model.workingCopy


decoder : Decoder Model
decoder =
    Json.Decode.map2 Data
        (Json.Decode.field "undoList" <| UndoList.Decode.undolist Document.decoder)
        (Json.Decode.field "workingCopy" <| Document.decoder)
        |> Json.Decode.map Model


encode : Model -> Value
encode (Model model) =
    Json.Encode.object
        [ ( "undoList", model.undoList |> UndoList.map Document.encode |> UndoList.Encode.undolist )
        , ( "workingCopy", Document.encode model.workingCopy )
        ]
