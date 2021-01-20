module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Document as Doc exposing (Value(..), ValueOrError)
import Html exposing (Html, text)
import Maybe as M
import Result as R
import Set exposing (Set)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------


type alias Model =
    Doc.Document



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


init : {} -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    Doc.empty



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-------------------------------------------------------------------------------
-- SUBS
-------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Browser.Document Msg
view model =
    { title = "Butter Spreadsheet"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    let
        valueToString val =
            case val of
                Err e ->
                    Debug.toString e

                Ok v ->
                    case v of
                        IntValue i ->
                            String.fromInt i

                        StringValue s ->
                            s

        formattedValue =
            Doc.eval "A1" Dict.empty model |> Tuple.first |> valueToString
    in
    text formattedValue
