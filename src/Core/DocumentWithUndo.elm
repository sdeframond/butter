module Core.DocumentWithUndo exposing
    ( Diff
    , Model
    , applyContentFrom
    , applyDiff
    , commitEditedSheetNames
    , decoder
    , diffDecoder
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
import Core.Diff as Diff
import Core.Document as Document
import Core.Name exposing (Name)
import Core.Types as Types
import Core.UndoCmd as UndoCmd
import Json.Decode exposing (Decoder, Value)
import Json.Encode
import Ports
import Sheet
import UndoList exposing (UndoList)
import UndoList.Decode
import UndoList.Encode


type Model
    = Model Data


type alias Data =
    { undoList : UndoList Document.Model
    , workingCopy : Document.Model
    }


type alias Diff =
    Document.Diff


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


undo : Model -> ( Model, Cmd msg )
undo (Model model) =
    UndoList.undo model.undoList
        |> (\undoList -> Model { undoList = undoList, workingCopy = undoList.present })
        |> withSendDocDiffCmd (Model model)


redo : Model -> ( Model, Cmd msg )
redo (Model model) =
    UndoList.redo model.undoList
        |> (\undoList -> Model { undoList = undoList, workingCopy = undoList.present })
        |> withSendDocDiffCmd (Model model)


sendDocDiffCmd : Document.Model -> Document.Model -> Cmd msg
sendDocDiffCmd old new =
    if old == new then
        Cmd.none

    else
        Document.makeDiff new old |> Document.encodeDiff new |> Ports.sendDocDiff


withSendDocDiffCmd : Model -> Model -> ( Model, Cmd msg )
withSendDocDiffCmd (Model old) (Model new) =
    ( Model new, sendDocDiffCmd old.workingCopy new.workingCopy )


{-| Used for tests only.
TODO : find a better way to test ?
-}
past : Model -> List Document.Model
past (Model data) =
    data.undoList.past


commitDocToUndoList : Document.Model -> Data -> ( Model, Cmd msg )
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
            |> withSendDocDiffCmd (Model model)

    else
        ( Model { model | workingCopy = doc }, Cmd.none )


getCurrentSheet : Model -> Sheet.Sheet
getCurrentSheet (Model model) =
    model.workingCopy |> Document.getCurrentSheet


updateCurrentSheet : (sheetMsg -> msg) -> ((Name -> Maybe Types.SheetId) -> Sheet.Sheet -> ( Sheet.Sheet, ( UndoCmd.Cmd, Cmd sheetMsg ) )) -> Model -> ( Model, Cmd msg )
updateCurrentSheet toMsg func (Model model) =
    let
        ( newDoc, ( undoCmd, sheetCmd ) ) =
            Document.updateCurrentSheet func model.workingCopy
    in
    case undoCmd of
        UndoCmd.New ->
            let
                ( newModel, cmd ) =
                    commitDocToUndoList newDoc model
            in
            ( newModel, Cmd.batch [ Cmd.map toMsg sheetCmd, cmd ] )

        UndoCmd.None ->
            ( Model { model | workingCopy = newDoc }, Cmd.map toMsg sheetCmd )


insertSheet : Sheet.Params -> Model -> ( Model, Cmd msg )
insertSheet params (Model model) =
    Document.insertSheet params model.workingCopy
        |> (\doc -> commitDocToUndoList doc model)


removeSheet : Types.SheetId -> Model -> ( Model, Cmd msg )
removeSheet sheetId (Model model) =
    commitDocToUndoList (Document.removeSheet sheetId model.workingCopy) model


mapWorkingCopy : (Document.Model -> Document.Model) -> Model -> Model
mapWorkingCopy func (Model data) =
    Model { data | workingCopy = func data.workingCopy }


editCurrentSheetName : Model -> Model
editCurrentSheetName model =
    mapWorkingCopy Document.editCurrentSheetName model


commitEditedSheetNames : Model -> ( Model, Cmd msg )
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


makeDiff : Model -> Model -> Document.Diff
makeDiff (Model new) (Model old) =
    Document.makeDiff new.workingCopy old.workingCopy


applyDiff : Document.Diff -> Model -> Result (Diff.Error Never) Model
applyDiff diff (Model model) =
    let
        setWorkingCopy copy =
            Model { model | workingCopy = copy }
    in
    Document.applyDiff diff model.workingCopy
        |> Result.map setWorkingCopy



-- JSON


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


diffDecoder : Model -> Decoder Diff
diffDecoder (Model model) =
    Document.diffDecoder model.workingCopy


encodeDiff : Model -> Diff -> Value
encodeDiff (Model model) =
    Document.encodeDiff model.workingCopy
