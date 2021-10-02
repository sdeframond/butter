port module Main exposing (main)

import Browser
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
    , pressedKeys : List Keyboard.Key
    }



-- INIT


defaultDocumentName : Name
defaultDocumentName =
    Name.sanitize "Document"


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    initStore flags
        |> Tuple.mapBoth
            (\store -> { documents = store, pressedKeys = [] })
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



-- UPDATE


type Msg
    = DocumentStoreMsg DocumentStoreMsg
    | KeyboardMsg Keyboard.Msg


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
    | SetState DocumentStore


updateAndSetStorage : DocumentStoreMsg -> DocumentStore -> ( DocumentStore, Cmd DocumentStoreMsg )
updateAndSetStorage msg model =
    let
        ( newStore, cmds ) =
            updateDocumentStore msg model
    in
    ( newStore
    , case msg of
        SetState _ ->
            cmds

        _ ->
            Cmd.batch [ setStorage (encodeDocumentStore newStore), cmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        undo docs =
            Store.current docs
                |> Document.undo
                |> Result.map (Store.setCurrent docs)
    in
    case msg of
        DocumentStoreMsg storeMsg ->
            updateAndSetStorage storeMsg model.documents
                |> Tuple.mapBoth
                    (\store -> { model | documents = store })
                    (Cmd.map DocumentStoreMsg)

        KeyboardMsg kbdMsg ->
            let
                ( pressedKeys, changed ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal kbdMsg model.pressedKeys

                newModel =
                    { model | pressedKeys = Keyboard.update kbdMsg model.pressedKeys }
            in
            case ( List.member Keyboard.Control pressedKeys, changed ) of
                ( True, Just (Keyboard.KeyDown (Keyboard.Character "z")) ) ->
                    case undo newModel.documents of
                        Ok undone ->
                            ( { newModel | documents = undone }, Cmd.none )

                        Err error ->
                            ( newModel, error |> Types.errorToString |> logError )

                _ ->
                    ( newModel, Cmd.none )


updateDocumentStore : DocumentStoreMsg -> DocumentStore -> ( DocumentStore, Cmd DocumentStoreMsg )
updateDocumentStore msg documents =
    case msg of
        DocumentMsg docMsg ->
            let
                logDocumentError str =
                    (Store.currentName documents |> Name.toString) ++ ": " ++ str |> logError
            in
            Store.current documents
                |> Document.update logDocumentError docMsg
                |> Tuple.mapFirst (Store.setCurrent documents)
                |> Tuple.mapSecond (Cmd.map DocumentMsg)

        SelectDocument id ->
            ( Store.selectById identity id documents
                |> Maybe.withDefault documents
            , Cmd.none
            )

        EditDocumentName ->
            ( Store.editCurrentName identity documents
            , Cmd.none
            )

        RemoveDocument id ->
            ( Store.remove id documents
            , Cmd.none
            )

        UpdateDocumentName nameStr ->
            ( Store.updateEdit nameStr documents
            , Cmd.none
            )

        AddDocument ->
            ( Store.insert defaultDocumentName Document.init documents
            , Cmd.none
            )

        OpenDocument ->
            ( documents
            , File.Select.file [ "application/butter" ] DocumentLoaded
            )

        DocumentLoaded file ->
            let
                fileName =
                    File.name file

                docName =
                    fileName |> String.split "." |> List.head |> Maybe.withDefault fileName
            in
            ( documents
            , Task.perform (DocumentsBytesLoaded docName) (File.toBytes file)
            )

        DocumentsBytesLoaded nameStr bytes ->
            ( Document.fromBytes bytes
                |> Maybe.map (\doc -> Store.insert (Name.sanitize nameStr) doc documents)
                |> Maybe.withDefault documents
            , Cmd.none
            )

        DownloadDocument ->
            let
                docName =
                    Store.currentName documents
            in
            ( documents
            , Store.current documents
                |> Document.toBytes
                |> File.Download.bytes (Name.toString docName ++ ".butter") "application/butter"
            )

        SetState newStore ->
            ( Store.merge Document.merge newStore documents, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ storeSubscriptions model.documents |> Sub.map DocumentStoreMsg
        , Keyboard.subscriptions |> Sub.map KeyboardMsg
        ]


storeSubscriptions : DocumentStore -> Sub DocumentStoreMsg
storeSubscriptions model =
    Sub.batch
        [ Store.current model
            |> Document.subscriptions
            |> Sub.map DocumentMsg
        , updateState
            (Decode.decodeValue documentStoreDecoder
                >> Result.withDefault model
                >> SetState
            )
        ]



--VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Butter Spreadsheet"
    , body =
        viewBody model.documents
            |> List.map (Html.map DocumentStoreMsg)
            |> List.map Html.toUnstyled
    }


viewBody : DocumentStore -> List (Html DocumentStoreMsg)
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
                , isCurrent = Store.isCurrentId item.id model
                , editStatus = Store.editStatus model
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
                :: (Store.toItemList model
                        |> List.map docItem
                   )
            )
        , Store.current model
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
