module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Global as Global
import Document as Doc
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
import List as L
import Result as R
import Sheet exposing (Sheet)
import Types exposing (Name)


main : Program {} Model Msg
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
    , edit : EditStatus
    , sheetCounter : Int
    }


type EditStatus
    = NotEditing
    | EditingSheetName Types.SheetId Name



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


init : {} -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { doc = Doc.init (Sheet.initTable "Sheet1")
    , sheetCounter = 2
    , edit = NotEditing
    }



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = InsertGridSheet
    | InsertTableSheet
    | SelectSheet Types.SheetId
    | RemoveSheet Types.SheetId
    | EditSheet ( Types.SheetId, Sheet )
    | UpdateSheetName Name
    | DocMsg Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateModel msg model


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    let
        commitDoc m =
            { m | doc = Doc.commitEdit m.doc }

        commitSheetName m =
            case m.edit of
                EditingSheetName sheetId newName ->
                    { m
                        | edit = NotEditing
                        , doc =
                            Doc.renameSheet sheetId newName m.doc
                                -- TODO log errors
                                |> R.withDefault m.doc
                    }

                NotEditing ->
                    m

        insertSheet makeSheet =
            { model
                | doc =
                    R.withDefault model.doc <|
                        Doc.insertSheet
                            (makeSheet <| "Sheet" ++ String.fromInt model.sheetCounter)
                            model.doc
                , sheetCounter = model.sheetCounter + 1
            }
    in
    case Debug.log "update msg" msg of
        InsertGridSheet ->
            ( insertSheet Sheet.initGrid, Cmd.none )

        InsertTableSheet ->
            ( insertSheet Sheet.initTable, Cmd.none )

        SelectSheet sheetId ->
            ( { model
                | doc =
                    Doc.selectSheet sheetId model.doc
                        |> R.withDefault model.doc
              }
                |> commitDoc
                |> commitSheetName
            , Cmd.none
            )

        RemoveSheet sheetId ->
            ( commitDoc
                { model
                    | doc =
                        Doc.removeSheet sheetId model.doc
                            |> R.withDefault model.doc
                }
            , Cmd.none
            )

        EditSheet ( sheetId, sheet ) ->
            ( commitDoc { model | edit = EditingSheetName sheetId (Sheet.getName sheet) }
            , Cmd.none
            )

        UpdateSheetName name ->
            ( { model
                | edit =
                    case model.edit of
                        EditingSheetName oldName _ ->
                            EditingSheetName oldName name

                        _ ->
                            model.edit
              }
            , Cmd.none
            )

        DocMsg docMsg ->
            let
                ( newDoc, cmd ) =
                    Doc.update docMsg model.doc
            in
            ( commitSheetName { model | doc = newDoc }, Cmd.map DocMsg cmd )



-------------------------------------------------------------------------------
-- SUBS
-------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Doc.subscriptions model.doc |> Sub.map DocMsg



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Browser.Document Msg
view model =
    let
        docConfig =
            { toMsg = DocMsg
            }
    in
    { title = "Butter Spreadsheet"
    , body =
        L.map toUnstyled
            [ Global.global
                [ Global.html [ height (pct 100) ]
                , Global.body
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    ]
                ]
            , Doc.view docConfig model.doc
            , sheetSelector model
            ]
    }


sheetSelector : Model -> Html Msg
sheetSelector model =
    let
        itemCss =
            css
                [ border3 (px 1) solid (rgb 100 100 100)
                , display inlineBlock
                , padding2 (px 5) (px 5)
                ]

        sheetItem positionedName =
            let
                defaultItem ( sheetId, sheet ) isCurrent =
                    li
                        [ itemCss
                        , css
                            [ if isCurrent then
                                fontWeight bold

                              else
                                fontWeight normal
                            ]
                        , onClick <| SelectSheet sheetId
                        , onDoubleClick <| EditSheet ( sheetId, sheet )
                        ]
                        [ text (Sheet.getName sheet)
                        , span [ onClick <| RemoveSheet sheetId ]
                            [ text "[x]" ]
                        ]
            in
            case ( positionedName, model.edit ) of
                ( Doc.Current _, EditingSheetName _ newName ) ->
                    li [ itemCss ]
                        [ input
                            [ value newName
                            , onInput UpdateSheetName
                            ]
                            []
                        ]

                ( Doc.Current sheetWithId, _ ) ->
                    defaultItem sheetWithId True

                ( Doc.Before sheetWithId, _ ) ->
                    defaultItem sheetWithId False

                ( Doc.After sheetWithId, _ ) ->
                    defaultItem sheetWithId False

        addSheet msg label =
            li
                [ itemCss
                , onClick msg
                ]
                [ text label ]
    in
    ul
        [ css
            [ borderTop3 (px 1) solid (rgb 0 0 0)
            , margin (px 0)
            , padding2 (px 10) (px 10)
            ]
        ]
        (addSheet InsertTableSheet "+table"
            :: addSheet InsertGridSheet "+grid"
            :: (Doc.sheetsWithIds model.doc |> L.map sheetItem)
        )
