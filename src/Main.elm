module Main exposing (..)

--import Html exposing (Html, table, td, text, th, tr)

import Browser
import Css exposing (..)
import Dict as D exposing (Dict)
import Document as Doc exposing (Name, Value(..), ValueOrError)
import Html
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import List as L
import Maybe as M
import Result as R
import Set exposing (Set)
import String as S


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
    { doc : Doc.Document
    , edit : Maybe ( Col, Row )
    }


type alias Col =
    Int


type alias Row =
    Int



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


init : {} -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { doc =
        Doc.fromList
            [ ( "A1", "=1+\"a\"" )
            , ( "A2", "=A1+1" )
            , ( "A3", "=A4" )
            , ( "A4", "=A3" )
            , ( "A5", "=A32" )
            ]
    , edit = Nothing
    }



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = EditCell Col Row
    | UpdateSource Name String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case Debug.log "update msg" msg of
        EditCell col row ->
            { model | edit = Just ( col, row ) }

        UpdateSource name src ->
            { model | doc = Doc.insert name src model.doc }



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
    , body = [ viewBody model |> toUnstyled ]
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

        results =
            model.doc
                |> Doc.evalAll

        toLetter i =
            Char.fromCode (i - 1 + Char.toCode 'A') |> S.fromChar

        mapColumns f =
            L.range 1 10 |> L.map f

        blueGrey =
            rgb 220 220 240

        columnHeaders =
            tr [ css [ backgroundColor blueGrey ] ] (th [] [] :: mapColumns (\col -> th [] [ text <| toLetter col ]))

        rowHeader row =
            th [ css [ backgroundColor blueGrey, padding2 (Css.em 0.2) (Css.em 0.4) ] ] [ text <| S.fromInt row ]

        cell row col =
            let
                name =
                    [ col |> toLetter, row |> String.fromInt ]
                        |> S.concat

                defaultCell =
                    D.get name results
                        |> M.map valueToString
                        |> M.withDefault ""
                        |> text

                editCell =
                    input
                        [ value (Doc.source name model.doc |> M.withDefault "")
                        , onInput <| UpdateSource name
                        ]
                        []
            in
            td
                [ css
                    [ border3 (px 1) solid (rgb 230 230 230)
                    , minWidth (Css.em 10)
                    , height (px 20)
                    ]
                , onClick <| EditCell col row
                ]
                [ case model.edit of
                    Nothing ->
                        defaultCell

                    Just coord ->
                        if coord == ( col, row ) then
                            editCell

                        else
                            defaultCell
                ]

        rows =
            L.range 1 20
                |> L.map
                    (\row ->
                        tr [] <|
                            rowHeader row
                                :: mapColumns (cell row)
                    )
    in
    H.table
        [ css [ borderCollapse collapse ] ]
        (columnHeaders :: rows)
