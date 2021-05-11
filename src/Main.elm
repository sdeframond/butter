port module Main exposing (main)

import Browser
import Document
import Json.Decode as Decode
import Json.Encode as Encode



-- MAIN


main : Program Encode.Value Document.Model Document.Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = Document.subscriptions
        }



-- INIT


init : Encode.Value -> ( Document.Model, Cmd Document.Msg )
init flags =
    case Decode.decodeValue Document.decoder flags of
        Ok model ->
            ( model, Cmd.none )

        Err error ->
            ( Document.init, logError <| Decode.errorToString error )



-- PORTS


port setStorage : Encode.Value -> Cmd msg


port logError : String -> Cmd msg



-- UPDATE


update : Document.Msg -> Document.Model -> ( Document.Model, Cmd Document.Msg )
update msg oldModel =
    let
        ( newModel, cmds ) =
            Document.update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (Document.encode newModel), cmds ]
    )



--VIEW


view : Document.Model -> Browser.Document Document.Msg
view model =
    { title = "Butter Spreadsheet"
    , body = Document.view model
    }
