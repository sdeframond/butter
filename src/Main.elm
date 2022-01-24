module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..), onVisibilityChange)
import Bytes exposing (Bytes)
import Core.FracStore as Store exposing (Store)
import Core.Name as Name exposing (Name)
import Css
import Css.Global as Global
import DocumentView as Document
import File exposing (File)
import File.Download
import File.Select
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard
import Ports
import Task
import Ui



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , update =
            \msg model ->
                update msg model |> setStorageCmd msg
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias DocumentStore =
    Store Document.Model


type alias Model =
    { documents : DocumentStore
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
        |> Tuple.mapFirst
            (\store -> { documents = store, pressedKeys = [] })


initStore : Encode.Value -> ( DocumentStore, Cmd Msg )
initStore jsonValue =
    case Decode.decodeValue documentStoreDecoder jsonValue of
        Ok store ->
            ( store, Cmd.none )

        Err error ->
            ( Store.init defaultDocumentName Document.init
            , Ports.logError <| Decode.errorToString error
            )



-- UPDATE


type Msg
    = KeyboardMsg Keyboard.Msg
    | VisibilityChangedMsg Visibility
    | MergeDocumentStoreMsg DocumentStore
    | SetDocumentStore DocumentStore
    | DocumentMsg Document.Msg
    | RemoveDocument Store.Id
    | AddDocument
    | OpenDocument
    | DocumentLoaded File
    | DocumentsBytesLoaded String Bytes
    | DownloadDocument


setStorageCmd : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setStorageCmd msg ( newModel, cmds ) =
    case msg of
        MergeDocumentStoreMsg _ ->
            ( newModel, cmds )

        DownloadDocument ->
            ( newModel, cmds )

        _ ->
            ( newModel
            , Cmd.batch [ Ports.setStorage (encodeDocumentStore newModel.documents), cmds ]
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case Debug.log "msg" msg of
    case msg of
        KeyboardMsg kbdMsg ->
            handleKeyboardMsg kbdMsg model

        VisibilityChangedMsg visibility ->
            case visibility of
                Hidden ->
                    ( { model | pressedKeys = [] }, Cmd.none )

                Visible ->
                    ( model, Cmd.none )

        MergeDocumentStoreMsg remoteStore ->
            ( model.documents
                |> Store.applyContentFrom Document.applyContentFrom remoteStore
                |> setDocumentStore model
            , Cmd.none
            )

        SetDocumentStore documents ->
            ( setDocumentStore model documents
            , Cmd.none
            )

        DocumentMsg docMsg ->
            Store.current model.documents
                |> Document.update docMsg
                |> Tuple.mapBoth
                    (Store.setCurrent model.documents >> setDocumentStore model)
                    (Cmd.map DocumentMsg)

        RemoveDocument id ->
            ( Store.remove id model.documents |> setDocumentStore model
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


handleKeyboardMsg : Keyboard.Msg -> Model -> ( Model, Cmd Msg )
handleKeyboardMsg kbdMsg model =
    let
        mapDocs f docs =
            Store.current docs
                |> f
                |> Tuple.mapFirst (Store.setCurrent docs)

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
            ( newModel.documents, Cmd.none )

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
        |> Tuple.mapBoth
            (\docs -> { newModel | documents = docs })
            (Cmd.map DocumentMsg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ storeSubscriptions model.documents
        , Keyboard.subscriptions |> Sub.map KeyboardMsg
        , onVisibilityChange VisibilityChangedMsg
        , Ports.blurs (always <| VisibilityChangedMsg Hidden)
        , Ports.updateState
            (Decode.decodeValue documentStoreDecoder
                >> Result.withDefault model.documents
                >> MergeDocumentStoreMsg
            )
        ]


storeSubscriptions : DocumentStore -> Sub Msg
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
            |> List.map Html.toUnstyled
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    let
        docItem documents isCurrent =
            Ui.editableListItem
                { onSelect = \_ -> documents |> Store.commitEdits |> SetDocumentStore
                , onEdit = \_ -> documents |> Store.editCurrentName |> SetDocumentStore
                , onRemove = \_ -> documents |> Store.currentId |> RemoveDocument
                , onUpdate = (\str -> Store.updateCurrentEditedName str documents) >> SetDocumentStore
                }
                { name = Store.currentName documents |> Name.toString
                , isCurrent = isCurrent
                , editStatus = Store.getCurrentEditStatus documents
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
                :: Store.zipMap docItem model.documents
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
