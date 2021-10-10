port module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..), onVisibilityChange)
import Bytes exposing (Bytes)
import Css
import Css.Global as Global
import Document
import File exposing (File)
import File.Download
import File.Select
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard
import Name exposing (Name)
import NamedAndOrderedStore as Store
import Task
import Types
import Ui



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Store a =
    Store.NamedAndOrderedStore a


type alias DocumentStore =
    Store Document.Model


type alias Model =
    { documents : DocumentStore
    , docNameEditStatus : Maybe String
    , pressedKeys : List Keyboard.Key
    }


setDocumentStore : Model -> DocumentStore -> Model
setDocumentStore model docs =
    { model | documents = docs }



-- INIT


defaultDocumentName : Name
defaultDocumentName =
    Name.sanitize "Document"


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    initStore flags
        |> Tuple.mapBoth
            (\store -> { documents = store, pressedKeys = [], docNameEditStatus = Nothing })
            (Cmd.map DocumentStoreMsg)


initStore : Encode.Value -> ( DocumentStore, Cmd DocumentStoreMsg )
initStore jsonValue =
    case Decode.decodeValue documentStoreDecoder jsonValue of
        Ok store ->
            ( store, Cmd.none )

        Err error ->
            ( Store.init defaultDocumentName Document.init
            , logError <| Decode.errorToString error
            )



-- PORTS


port setStorage : Encode.Value -> Cmd msg


port updateState : (Encode.Value -> msg) -> Sub msg


port logError : String -> Cmd msg


port blurs : (Encode.Value -> msg) -> Sub msg



-- UPDATE


type Msg
    = DocumentStoreMsg DocumentStoreMsg
    | KeyboardMsg Keyboard.Msg
    | VisibilityChangedMsg Visibility
    | SetDocumentStoreMsg DocumentStore


type DocumentStoreMsg
    = DocumentMsg Document.Msg
    | SelectDocument Store.Id
    | EditDocumentName
    | RemoveDocument Store.Id
    | UpdateDocumentName String
    | AddDocument
    | OpenDocument
    | DocumentLoaded File
    | DocumentsBytesLoaded String Bytes
    | DownloadDocument


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setStorageCmd ( newModel, cmds ) =
            ( newModel, Cmd.batch [ setStorage (encodeDocumentStore newModel.documents), cmds ] )
    in
    case msg of
        DocumentStoreMsg storeMsg ->
            updateDocumentStore storeMsg model
                |> Tuple.mapSecond (Cmd.map DocumentStoreMsg)
                |> setStorageCmd

        KeyboardMsg kbdMsg ->
            handleKeyboardMsg kbdMsg model
                |> setStorageCmd

        VisibilityChangedMsg visibility ->
            case visibility of
                Hidden ->
                    ( { model | pressedKeys = [] }, Cmd.none )

                Visible ->
                    ( model, Cmd.none )

        SetDocumentStoreMsg newStore ->
            ( Store.merge Document.merge newStore model.documents
                |> setDocumentStore model
            , Cmd.none
            )


handleKeyboardMsg : Keyboard.Msg -> Model -> ( Model, Cmd msg )
handleKeyboardMsg kbdMsg model =
    let
        mapDocs f docs =
            Store.current docs
                |> f
                |> Result.map (Store.setCurrent docs)

        logDocsError docResult =
            case docResult of
                Ok docs ->
                    ( { newModel | documents = docs }, Cmd.none )

                Err error ->
                    ( newModel, error |> Types.errorToString |> logError )

        ( pressedKeys, changed ) =
            Keyboard.updateWithKeyChange Keyboard.anyKeyUpper kbdMsg model.pressedKeys

        isPressed key =
            List.member key pressedKeys

        ( control, alt, shift ) =
            ( isPressed Keyboard.Control
            , isPressed Keyboard.Alt
            , isPressed Keyboard.Shift
            )

        newModel =
            { model | pressedKeys = Keyboard.update kbdMsg model.pressedKeys }

        onlyKeyDown keyEvent =
            case keyEvent of
                Keyboard.KeyDown key ->
                    Just key

                Keyboard.KeyUp _ ->
                    Nothing

        default =
            Ok newModel.documents

        handleKey key =
            case key of
                Keyboard.Character "Z" ->
                    if control && not alt && not shift then
                        mapDocs Document.undo newModel.documents

                    else if control && not alt && shift then
                        mapDocs Document.redo newModel.documents

                    else
                        default

                Keyboard.Character "Y" ->
                    if control && not alt && not shift then
                        mapDocs Document.redo newModel.documents

                    else
                        default

                _ ->
                    default
    in
    changed
        |> Maybe.andThen onlyKeyDown
        |> Maybe.map handleKey
        |> Maybe.withDefault default
        |> logDocsError


updateDocumentStore : DocumentStoreMsg -> Model -> ( Model, Cmd DocumentStoreMsg )
updateDocumentStore msg model =
    let
        selectDocumentById id model_ =
            Store.selectById identity id model_.documents
                |> Maybe.withDefault model_.documents
                |> setDocumentStore model_

        commitEdit model_ =
            model_.docNameEditStatus
                |> Maybe.andThen Name.fromString
                |> Maybe.withDefault (Store.currentName model_.documents)
                |> (\name -> Store.setName (Store.currentId model_.documents) name model_.documents)
                |> Result.map (setDocumentStore { model_ | docNameEditStatus = Nothing })

        withDefaultAndLogError default cmd r =
            case r of
                Ok m ->
                    ( m, cmd )

                Err e ->
                    ( default, Cmd.batch [ cmd, e |> Types.errorToString |> logError ] )
    in
    case msg of
        DocumentMsg docMsg ->
            let
                logDocumentError str =
                    (Store.currentName model.documents |> Name.toString) ++ ": " ++ str |> logError
            in
            Store.current model.documents
                |> Document.update logDocumentError docMsg
                |> Tuple.mapBoth
                    (Store.setCurrent model.documents >> setDocumentStore model)
                    (Cmd.map DocumentMsg)

        SelectDocument id ->
            commitEdit model
                |> withDefaultAndLogError model Cmd.none
                |> Tuple.mapFirst (selectDocumentById id)

        EditDocumentName ->
            ( { model | docNameEditStatus = Just (Store.currentName model.documents |> Name.toString) }
            , Cmd.none
            )

        RemoveDocument id ->
            ( Store.remove id model.documents |> setDocumentStore model
            , Cmd.none
            )

        UpdateDocumentName nameStr ->
            ( { model | docNameEditStatus = Just nameStr }
            , Cmd.none
            )

        AddDocument ->
            ( Store.insert defaultDocumentName Document.init model.documents
                |> setDocumentStore model
            , Cmd.none
            )

        OpenDocument ->
            ( model
            , File.Select.file [ "application/butter" ] DocumentLoaded
            )

        DocumentLoaded file ->
            let
                fileName =
                    File.name file

                docName =
                    fileName |> String.split "." |> List.head |> Maybe.withDefault fileName
            in
            ( model
            , Task.perform (DocumentsBytesLoaded docName) (File.toBytes file)
            )

        DocumentsBytesLoaded nameStr bytes ->
            ( Document.fromBytes bytes
                |> Maybe.map (\doc -> Store.insert (Name.sanitize nameStr) doc model.documents)
                |> Maybe.withDefault model.documents
                |> setDocumentStore model
            , Cmd.none
            )

        DownloadDocument ->
            let
                docName =
                    Store.currentName model.documents
            in
            ( model
            , Store.current model.documents
                |> Document.toBytes
                |> File.Download.bytes (Name.toString docName ++ ".butter") "application/butter"
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ storeSubscriptions model.documents |> Sub.map DocumentStoreMsg
        , Keyboard.subscriptions |> Sub.map KeyboardMsg
        , onVisibilityChange VisibilityChangedMsg
        , blurs (always <| VisibilityChangedMsg Hidden)
        , updateState
            (Decode.decodeValue documentStoreDecoder
                >> Result.withDefault model.documents
                >> SetDocumentStoreMsg
            )
        ]


storeSubscriptions : DocumentStore -> Sub DocumentStoreMsg
storeSubscriptions model =
    Sub.batch
        [ Store.current model
            |> Document.subscriptions
            |> Sub.map DocumentMsg
        ]



--VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Butter Spreadsheet"
    , body =
        viewBody model
            |> List.map (Html.map DocumentStoreMsg)
            |> List.map Html.toUnstyled
    }


viewBody : Model -> List (Html DocumentStoreMsg)
viewBody model =
    let
        docItem item =
            Ui.editableListItem
                { onSelect = SelectDocument
                , onEdit = EditDocumentName
                , onRemove = RemoveDocument
                , onUpdate = UpdateDocumentName
                }
                { id = item.id
                , name = Name.toString item.name
                , isCurrent = Store.isCurrentId item.id model.documents
                , editStatus = model.docNameEditStatus
                }
    in
    [ Global.global
        [ Global.html [ Css.height (Css.pct 100) ]
        , Global.body [ Css.height (Css.pct 100) ]
        ]
    , Ui.row
        [ css
            [ Css.height (Css.pct 100)
            , Css.width (Css.pct 100)
            , Css.overflow Css.hidden
            ]
        ]
        [ Ui.column
            [ css
                [ Css.minWidth (Css.px 200)
                , Css.overflow Css.hidden
                ]
            ]
            (Ui.row [ css [ Css.justifyContent Css.spaceBetween ] ]
                [ Ui.button [ onClick AddDocument ] [ Html.text "New" ]
                , Ui.button [ onClick OpenDocument ] [ Html.text "Upload..." ]
                , Ui.button [ onClick DownloadDocument ] [ Html.text "Download" ]
                ]
                :: (Store.toItemList model.documents
                        |> List.map docItem
                   )
            )
        , Store.current model.documents
            |> Document.view
            |> Html.map DocumentMsg
        ]
    ]



-- JSON


documentStoreDecoder : Decode.Decoder DocumentStore
documentStoreDecoder =
    Store.decoder (always Document.decoder)


encodeDocumentStore : DocumentStore -> Encode.Value
encodeDocumentStore model =
    Store.encode (always Document.encode) model
