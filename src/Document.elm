module Document exposing
    ( Model
    , Msg(..)
    , cancelEdits
    , decoder
    , editCurrentSheetName
    , encode
    , fromBytes
    , getCurrentSheet
    , getCurrentSheetEditStatus
    , getCurrentSheetId
    , getCurrentSheetName
    , getSheetNameById
    , getSheets
    , init
    , insertSheet
    , merge
    , removeSheet
    , selectSheet
    , subscriptions
    , toBytes
    , undo
    , update
    , updateCurrentSheet
    , updateEditedSheetName
    , view
    )

import Bytes exposing (Bytes)
import Bytes.Encode
import Css exposing (..)
import File.Download exposing (bytes)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode
import Json.Encode
import Name exposing (Name)
import NamedAndOrderedStore as Store exposing (NamedAndOrderedStore, currentName)
import PositiveInt
import Sheet exposing (Sheet)
import Time
import Types
import Ui
import Zip
import Zip.Entry as Entry


type alias Store a =
    NamedAndOrderedStore a


type Model
    = Model ModelData


type alias ModelData =
    { store : Store Sheet
    , sheetEditStatus : Maybe String
    , undo : List Action
    , redo : List Action
    }


type Action
    = RenameSheetAction { sheetId : Types.SheetId, old : Name, new : Name }



-- | RemoveSheetAction Types.SheetId Sheet
-- | InsertSheetAction Types.SheetId Sheet.Params


defaultSheetName : Name
defaultSheetName =
    Name.sanitize "Sheet"


init : Model
init =
    Model
        { store =
            Sheet.initTable
                |> Store.init defaultSheetName
        , undo = []
        , redo = []
        , sheetEditStatus = Nothing
        }


pushHistory : Action -> Model -> Model
pushHistory action (Model data) =
    Model { data | undo = action :: data.undo }


applyAction : Action -> Model -> Result Types.Error Model
applyAction action (Model data) =
    case action of
        RenameSheetAction { sheetId, new } ->
            Store.setName sheetId new data.store
                |> Result.map (\store -> Model { data | store = store })


unApplyAction : Action -> Model -> Result Types.Error Model
unApplyAction action (Model data) =
    case action of
        RenameSheetAction { sheetId, old } ->
            Store.setName sheetId old data.store
                |> Result.map (\store -> Model { data | store = store })


undo : Model -> Result Types.Error Model
undo (Model data) =
    case data.undo of
        action :: previous ->
            Model
                { data
                    | undo = previous
                    , redo = action :: data.redo
                }
                |> unApplyAction action

        _ ->
            Model data |> Result.Ok


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


cancelEdits : Model -> Model
cancelEdits (Model data) =
    Model { data | sheetEditStatus = Nothing }


merge : Model -> Model -> Model
merge (Model inData) (Model currentData) =
    Model
        { inData
            | store =
                Store.merge Sheet.merge inData.store currentData.store
        }


getCurrentSheet : Model -> Sheet
getCurrentSheet (Model { store }) =
    Store.current store


setSheetName : Types.SheetId -> String -> Model -> Result Types.Error Model
setSheetName sheetId str model =
    let
        makeAction newName =
            RenameSheetAction
                { sheetId = sheetId
                , old = getCurrentSheetName model
                , new = newName
                }

        apply action =
            applyAction action model |> Result.map (pushHistory action)
    in
    Name.fromString str
        |> Result.fromMaybe Types.InvalidSheetNameError
        |> Result.map makeAction
        |> Result.andThen apply


commitEditedSheetName : Model -> Model
commitEditedSheetName model =
    case getCurrentSheetEditStatus model of
        Just str ->
            setSheetName (getCurrentSheetId model) str model
                |> Result.withDefault model
                |> cancelEdits

        Nothing ->
            model


updateCurrentSheet : Sheet.Msg -> Model -> ( Model, Cmd Sheet.Msg )
updateCurrentSheet msg model_ =
    let
        ((Model data) as model) =
            commitEditedSheetName model_

        setCurrent sheet =
            Model
                { data
                    | store =
                        sheet
                            |> Store.setCurrent data.store
                }
    in
    Sheet.update (Store.getIdByName data.store) msg (getCurrentSheet model)
        |> Tuple.mapFirst setCurrent


insertSheet : Sheet.Params -> Model -> Model
insertSheet params (Model data) =
    Model
        { data
            | store = Store.insert defaultSheetName (Sheet.fromParams params) data.store
        }


selectSheet : Types.SheetId -> Model -> Maybe Model
selectSheet sheetId model =
    let
        doSelect (Model data) =
            data.store
                |> Store.selectById Sheet.commitEdit sheetId
                |> Maybe.map (\store -> Model { data | store = store })
    in
    model
        |> commitEditedSheetName
        |> doSelect


removeSheet : Types.SheetId -> Model -> Model
removeSheet sheetId (Model data) =
    Model { data | store = Store.remove sheetId data.store }


editCurrentSheetName : Model -> Model
editCurrentSheetName (Model data) =
    Model { data | sheetEditStatus = currentName data.store |> Name.toString |> Just }


updateEditedSheetName : String -> Model -> Model
updateEditedSheetName input (Model data) =
    Model { data | sheetEditStatus = Just input }


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
    data.sheetEditStatus


getSheets : Model -> List { id : Types.SheetId, name : Name, value : Sheet }
getSheets (Model data) =
    Store.toItemList data.store



-- SUSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    getCurrentSheet model
        |> Sheet.subscriptions
        |> Sub.map SheetMsg



-- UPDATE


type Msg
    = SheetMsg Sheet.Msg
    | InsertSheet Sheet.Params
    | SelectSheet Types.SheetId
    | RemoveSheet Types.SheetId
    | EditCurrentSheetName
    | UpdateEditedSheetName String


update : (String -> Cmd Msg) -> Msg -> Model -> ( Model, Cmd Msg )
update logError msg model =
    case msg of
        SheetMsg sheetMsg ->
            updateCurrentSheet sheetMsg model
                |> Tuple.mapSecond (Cmd.map SheetMsg)

        InsertSheet params ->
            ( insertSheet params model, Cmd.none )

        SelectSheet sheetId ->
            case selectSheet sheetId model of
                Just newModel ->
                    ( newModel, Cmd.none )

                Nothing ->
                    ( model, logError <| "Selected sheet does not exist. SheetId = " ++ PositiveInt.toString sheetId )

        RemoveSheet sheetId ->
            ( removeSheet sheetId model, Cmd.none )

        EditCurrentSheetName ->
            ( editCurrentSheetName model, Cmd.none )

        UpdateEditedSheetName input ->
            ( updateEditedSheetName input model, Cmd.none )



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
            , getSheetName = \id -> getSheetNameById id model
            , context =
                { prefix = getCurrentSheetId model
                , ancestors = []
                , resolveGlobalReference = eval model
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
        [ Sheet.view sheetConfig (getCurrentSheet model)
        ]


sheetSelector : Model -> Html Msg
sheetSelector model =
    let
        sheetItem item =
            Ui.editableListItem
                { onSelect = SelectSheet
                , onEdit = EditCurrentSheetName
                , onRemove = RemoveSheet
                , onUpdate = UpdateEditedSheetName
                }
                { id = item.id
                , name = Name.toString item.name
                , isCurrent = getCurrentSheetId model == item.id
                , editStatus = getCurrentSheetEditStatus model
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
            :: (getSheets model |> List.map sheetItem)
        )


jsonKeys :
    { store : String
    , undo : String
    , redo : String
    }
jsonKeys =
    { store = "store"
    , undo = "undo"
    , redo = "redo"
    }


decoder : Json.Decode.Decoder Model
decoder =
    Json.Decode.map4 ModelData
        (Json.Decode.field jsonKeys.store (Store.decoder Sheet.decoder))
        (Json.Decode.succeed Nothing)
        (Json.Decode.succeed [])
        (Json.Decode.succeed [])
        |> Json.Decode.map Model


encode : Model -> Json.Encode.Value
encode (Model data) =
    Json.Encode.object
        [ ( jsonKeys.store, Store.encode Sheet.encode data.store )
        ]
