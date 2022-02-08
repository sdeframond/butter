module Core.Document exposing
    ( Diff
    , Model
    , applyContentFrom
    , applyDiff
    , cancelEdits
    , commitEditedSheetNames
    , decoder
    , diffDecoder
    , editCurrentSheetName
    , encode
    , encodeDiff
    , eval
    , fromBytes
    , getCurrentSheet
    , getCurrentSheetEditStatus
    , getCurrentSheetId
    , getCurrentSheetName
    , getSheetNameById
    , init
    , insertSheet
    , makeDiff
    , removeSheet
    , toBytes
    , updateCurrentEditedSheetName
    , updateCurrentSheet
    , zipMapSheets
    )

import Bytes exposing (Bytes)
import Bytes.Encode
import Core.DictDiff as Diff
import Core.Frac as Frac exposing (Frac)
import Core.FracStore as Store exposing (Store)
import Core.IdDict as IdDict
import Core.Name as Name exposing (Name)
import Core.Types as Types
import File.Download exposing (bytes)
import Json.Decode as Decode
import Json.Encode as Encode
import Sheet exposing (Sheet)
import Time
import Zip
import Zip.Entry as Entry


type Model
    = Model ModelData


type alias ModelData =
    { store : Store Sheet
    }


defaultSheetName : Name
defaultSheetName =
    Name.sanitize "Sheet"


init : Model
init =
    Model
        { store =
            Sheet.initTable
                |> Store.init defaultSheetName
        }


cancelEdits : Model -> Model
cancelEdits (Model data) =
    Model { data | store = Store.cancelEdits data.store }


applyContentFrom : Model -> Model -> Model
applyContentFrom (Model remote) (Model origin) =
    Model
        { remote
            | store =
                Store.applyContentFrom Sheet.applyContentFrom remote.store origin.store
        }


getCurrentSheet : Model -> Sheet
getCurrentSheet (Model { store }) =
    Store.current store


commitEditedSheetNames : Model -> Model
commitEditedSheetNames (Model data) =
    Model { data | store = Store.commitEdits data.store }


updateCurrentSheet : ((Name -> Maybe Types.SheetId) -> Sheet.Sheet -> ( Sheet.Sheet, cmd )) -> Model -> ( Model, cmd )
updateCurrentSheet func model_ =
    let
        ((Model data) as model) =
            commitEditedSheetNames model_

        setCurrent sheet =
            Model { data | store = Store.setCurrent data.store sheet }
    in
    func (Store.getIdByName data.store) (getCurrentSheet model)
        |> Tuple.mapFirst setCurrent


insertSheet : Sheet.Params -> Model -> Model
insertSheet params (Model data) =
    Model
        { data
            | store = Store.createAfterCurrent defaultSheetName (Sheet.fromParams params) data.store
        }


removeSheet : Types.SheetId -> Model -> Model
removeSheet sheetId (Model data) =
    Model { data | store = Store.remove sheetId data.store }


editCurrentSheetName : Model -> Model
editCurrentSheetName (Model data) =
    Model { data | store = Store.editCurrentName data.store }


updateCurrentEditedSheetName : String -> Model -> Model
updateCurrentEditedSheetName input (Model data) =
    Model { data | store = Store.updateCurrentEditedName input data.store }


getCurrentSheetId : Model -> Types.SheetId
getCurrentSheetId (Model data) =
    Store.currentId data.store


getCurrentSheetName : Model -> Name
getCurrentSheetName (Model data) =
    Store.currentName data.store


getSheetNameById : Types.SheetId -> Model -> Maybe Name
getSheetNameById id (Model data) =
    Store.getNameById data.store id


getCurrentSheetEditStatus : Model -> Maybe String
getCurrentSheetEditStatus (Model data) =
    Store.getCurrentEditStatus data.store


zipMapSheets : (Model -> Bool -> a) -> Model -> List a
zipMapSheets f (Model data) =
    let
        go store isCurrent =
            f (Model { data | store = store }) isCurrent
    in
    Store.zipMap go data.store



-- DIFF


type alias Diff =
    IdDict.IdDict (Diff.ValueDiff (Store.Item Sheet) ItemDiff)


type alias ItemDiff =
    { name : Maybe Name
    , position : Maybe Frac
    , value : Sheet.Diff
    }


makeItemDiff : Store.Item Sheet -> Store.Item Sheet -> ItemDiff
makeItemDiff new old =
    let
        newOrNothing a b =
            if a /= b then
                Just a

            else
                Nothing
    in
    { name = newOrNothing new.name old.name
    , position = newOrNothing new.position old.position
    , value = Sheet.makeDiff new.value old.value
    }


applyItemDiff : ItemDiff -> Store.Item Sheet -> Result Sheet.DiffError (Store.Item Sheet)
applyItemDiff diff item =
    let
        toItem value =
            { name = diff.name |> Maybe.withDefault item.name
            , position = diff.position |> Maybe.withDefault item.position
            , editStatus = item.editStatus
            , value = value
            }
    in
    Sheet.applyDiff diff.value item.value |> Result.map toItem


makeDiff : Model -> Model -> Diff
makeDiff (Model new) (Model old) =
    Diff.makeDiff IdDict.empty
        IdDict.insert
        Store.merge
        makeItemDiff
        new.store
        old.store


applyDiff : Diff -> Model -> Result (Diff.Error Sheet.DiffError) Model
applyDiff diff (Model { store }) =
    let
        merge ls bs rs diff_ dict =
            IdDict.merge ls bs rs diff_ (Store.toIdDict dict)
    in
    Diff.applyDiff merge Store.insertItem Store.remove applyItemDiff diff store
        |> Result.map (\s -> Model { store = s })


diffDecoder : Model -> Decode.Decoder Diff
diffDecoder (Model model) =
    let
        itemDecoder =
            Store.getIdByName model.store |> Sheet.decoder |> Store.itemDecoder

        itemDiffDecoder =
            Decode.map3 ItemDiff
                (Decode.field "name" <| Decode.nullable Name.decoder)
                (Decode.field "position" <| Decode.nullable Frac.decoder)
                (Decode.field "value" <| Sheet.diffDecoder (Store.getIdByName model.store))
    in
    IdDict.decoder (Diff.diffValueDecoder itemDecoder itemDiffDecoder)


encodeDiff : Model -> Diff -> Encode.Value
encodeDiff (Model model) diff =
    let
        encodeItem =
            Store.getNameById model.store |> Sheet.encode |> Store.encodeItem

        encodeMaybe encodeValue m =
            case m of
                Just v ->
                    encodeValue v

                Nothing ->
                    Encode.null

        encodeItemDiff itemDiff =
            Encode.object
                [ ( "name", encodeMaybe Name.encode itemDiff.name )
                , ( "position", encodeMaybe Frac.encode itemDiff.position )
                , ( "value", Sheet.encodeDiff (Store.getNameById model.store) itemDiff.value )
                ]
    in
    IdDict.encode (Diff.encodeDiffValue encodeItem encodeItemDiff) diff



-- EVAL


eval : Model -> Types.LocatedName -> List Types.LocatedName -> Types.ValueOrError
eval ((Model data) as model) ( sheetId, ref ) ancestors =
    let
        context : Sheet.Context
        context =
            { ancestors = ancestors
            , prefix = sheetId
            , resolveGlobalReference = eval model
            }
    in
    Store.getById sheetId data.store
        |> Result.fromMaybe
            -- Is it realy unexpected though ? Eg what happens when some sheet is removed ?
            (Types.UnexpectedError "Found an orphan sheet")
        |> Result.andThen (Sheet.eval ref context)



-- ZIP


toBytes : Model -> Bytes
toBytes doc =
    Encode.encode 2 (encode doc)
        |> (Bytes.Encode.encode << Bytes.Encode.string)
        |> Entry.compress
            { path = "/main.json"

            -- We don't care about this.
            , lastModified = ( Time.utc, Time.millisToPosix 0 )
            , comment = Nothing
            }
        |> List.singleton
        |> Zip.fromEntries
        |> Zip.toBytes


fromBytes : Bytes -> Maybe Model
fromBytes bytes =
    Zip.fromBytes bytes
        |> Maybe.andThen (Zip.getEntry "/main.json")
        |> Maybe.andThen
            (\entry ->
                case Entry.toString entry of
                    Ok string ->
                        Decode.decodeString decoder string
                            |> Result.toMaybe

                    Err _ ->
                        Nothing
            )



-- JSON


jsonKeys :
    { store : String
    }
jsonKeys =
    { store = "store"
    }


decoder : Decode.Decoder Model
decoder =
    Decode.map ModelData
        (Decode.field jsonKeys.store (Store.decoder Sheet.decoder))
        |> Decode.map Model


encode : Model -> Encode.Value
encode (Model data) =
    Encode.object
        [ ( jsonKeys.store, Store.encode Sheet.encode data.store )
        ]
