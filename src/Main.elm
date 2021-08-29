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
import Name exposing (Name)
import NamedAndOrderedStore as Store
import Task
import Ui



-- MAIN


main : Program Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , update = updateAndSetStorage
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Store a =
    Store.NamedAndOrderedStore a


type alias Model =
    Store Document.Model


defaultDocumentName : Name
defaultDocumentName =
    Name.sanitize "Document"


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Decode.decodeValue decoder flags of
        Ok model ->
            ( model, Cmd.none )

        Err error ->
            ( Store.init defaultDocumentName Document.init, logError <| Decode.errorToString error )



-- PORTS


port setStorage : Encode.Value -> Cmd msg


port updateState : (Encode.Value -> msg) -> Sub msg


port logError : String -> Cmd msg



-- UPDATE


type Msg
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
    | SetState Model


updateAndSetStorage : Msg -> Model -> ( Model, Cmd Msg )
updateAndSetStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , case msg of
        SetState _ ->
            cmds

        _ ->
            Cmd.batch [ setStorage (encode newModel), cmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DocumentMsg docMsg ->
            Store.current model
                |> Document.update docMsg
                |> Tuple.mapFirst (Store.setCurrent model)
                |> Tuple.mapSecond (Cmd.map DocumentMsg)

        SelectDocument id ->
            ( Store.selectById identity id model
                |> Maybe.withDefault model
            , Cmd.none
            )

        EditDocumentName ->
            ( Store.editCurrentName identity model
            , Cmd.none
            )

        RemoveDocument id ->
            ( Store.remove id model
            , Cmd.none
            )

        UpdateDocumentName nameStr ->
            ( Store.updateEdit nameStr model
            , Cmd.none
            )

        AddDocument ->
            ( Store.insert defaultDocumentName Document.init model
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
                |> Maybe.map (\doc -> Store.insert (Name.sanitize nameStr) doc model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        DownloadDocument ->
            let
                docName =
                    Store.currentName model
            in
            ( model
            , Store.current model
                |> Document.toBytes
                |> File.Download.bytes (Name.toString docName ++ ".butter") "application/butter"
            )

        SetState newModel ->
            ( newModel, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Store.current model
            |> Document.subscriptions
            |> Sub.map DocumentMsg
        , updateState (Decode.decodeValue decoder >> Result.withDefault model >> SetState)
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


decoder : Decode.Decoder Model
decoder =
    Store.decoder (always Document.decoder)


encode : Model -> Encode.Value
encode model =
    Store.encode (always Document.encode) model
