module Core.Document exposing
    ( Model
    , applyContentFrom
    , cancelEdits
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
    , removeSheet
    , toBytes
    , updateCurrentEditedSheetName
    , updateCurrentSheet
    , zipMapSheets
    )

import Bytes exposing (Bytes)
import Bytes.Encode
import Core.FracStore as Store exposing (Store)
import Core.Name as Name exposing (Name)
import Core.Types as Types
import File.Download exposing (bytes)
import Json.Decode
import Json.Encode
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
            | store = Store.insert defaultSheetName (Sheet.fromParams params) data.store
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
    Json.Encode.encode 2 (encode doc)
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
                        Json.Decode.decodeString decoder string
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


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map ModelData
        (Json.Decode.field jsonKeys.store (Store.decoder Sheet.decoder))
        |> Json.Decode.map Model


encode : Model -> Json.Encode.Value
encode (Model data) =
    Json.Encode.object
        [ ( jsonKeys.store, Store.encode Sheet.encode data.store )
        ]
