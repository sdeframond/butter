module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Global as Global
import Dict as D exposing (Dict)
import Document as Doc exposing (Sheet(..))
import Document.Grid as Grid exposing (Grid)
import Document.Table as Table
import Document.Types exposing (Error(..), Name, Position(..), Value(..), ValueOrError)
import Html
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
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
    , edit : EditStatus
    , sheetCounter : Int
    }


type EditStatus
    = NotEditing
    | EditingSheetName Name Name


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
    { doc = Doc.singleSheet "Sheet1"
    , sheetCounter = 2
    , edit = NotEditing
    }



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type
    Msg
    --= EditCell Name String
    --| UpdateEdit EditStatus
    = InsertGridSheet
    | InsertTableSheet
    | SelectSheet Name
    | RemoveSheet Name
    | EditSheet Name
    | UpdateSheetName Name
    | SetTableState Table.State
    | GridMsg Grid
    | GridCommitMsg Grid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    let
        commited =
            case model.edit of
                EditingSheetName oldName newName ->
                    { model
                        | edit = NotEditing
                        , doc =
                            Doc.renameSheet oldName newName model.doc
                                |> R.withDefault model.doc
                                |> Doc.commitEdit
                    }

                NotEditing ->
                    { model | doc = Doc.commitEdit model.doc }

        insertSheet constructor =
            { model
                | doc =
                    R.withDefault model.doc <|
                        Doc.insertSheet
                            (constructor <| "Sheet" ++ String.fromInt model.sheetCounter)
                            model.doc
                , sheetCounter = model.sheetCounter + 1
            }
    in
    case Debug.log "update msg" msg of
        InsertGridSheet ->
            insertSheet Doc.gridSheetItem

        InsertTableSheet ->
            insertSheet Doc.tableSheetItem

        SelectSheet name ->
            { commited
                | doc =
                    Doc.selectSheet name commited.doc
                        |> R.withDefault commited.doc
            }

        RemoveSheet name ->
            let
                doc =
                    Doc.removeSheet name commited.doc
            in
            { commited
                | doc =
                    Doc.removeSheet name commited.doc
                        |> R.withDefault commited.doc
            }

        EditSheet name ->
            { commited | edit = EditingSheetName name name }

        UpdateSheetName name ->
            { model
                | edit =
                    case model.edit of
                        EditingSheetName oldName _ ->
                            EditingSheetName oldName name

                        _ ->
                            model.edit
            }

        SetTableState s ->
            { model | doc = Doc.updateTable s model.doc }

        GridMsg grid ->
            { model | doc = Doc.updateGrid grid model.doc }

        GridCommitMsg grid ->
            { commited | doc = Doc.updateGrid grid commited.doc }



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
    let
        gridConfig =
            { toMsg = GridMsg
            , commitMsg = GridCommitMsg
            , getCellValue = \name -> Doc.get name model.doc
            , getCellSource = \name -> Doc.cellSource name model.doc |> R.withDefault ""
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
            , div
                [ css
                    [ width (pct 100)
                    , height (pct 100)
                    , overflow hidden
                    ]
                ]
                [ case Doc.currentSheet model.doc of
                    GridSheet grid ->
                        Grid.view gridConfig grid

                    TableSheet table ->
                        fromUnstyled (Table.view SetTableState table)
                ]
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

        sheetItem item =
            let
                defaultItem name isCurrent =
                    li
                        [ itemCss
                        , css
                            [ if isCurrent then
                                fontWeight bold

                              else
                                fontWeight normal
                            ]
                        , onClick <| SelectSheet name
                        , onDoubleClick <| EditSheet name
                        ]
                        [ text name
                        , span [ onClick <| RemoveSheet name ]
                            [ text "[x]" ]
                        ]
            in
            case ( item, model.edit ) of
                ( Current { name }, EditingSheetName oldName newName ) ->
                    li [ itemCss ]
                        [ input
                            [ value newName
                            , onInput UpdateSheetName
                            ]
                            []
                        ]

                ( Current { name }, _ ) ->
                    defaultItem name True

                ( Before { name }, _ ) ->
                    defaultItem name False

                ( After { name }, _ ) ->
                    defaultItem name False

        addGridSheet =
            li
                [ itemCss
                , onClick InsertGridSheet
                ]
                [ text "+grid" ]

        addTableSheet =
            li
                [ itemCss
                , onClick InsertTableSheet
                ]
                [ text "+table" ]
    in
    ul
        [ css
            [ borderTop3 (px 1) solid (rgb 0 0 0)
            , margin (px 0)
            , padding2 (px 10) (px 10)
            ]
        ]
        (addTableSheet
            :: addGridSheet
            :: (Doc.sheets model.doc |> L.map sheetItem)
        )
