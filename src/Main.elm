module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Global as Global
import Document as Doc exposing (insert)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
import List as L
import Result as R
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
    | EditingSheetName Name Name



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


init : {} -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { doc = Doc.init "Sheet1" Doc.tableSheet
    , sheetCounter = 2
    , edit = NotEditing
    }



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = InsertGridSheet
    | InsertTableSheet
    | InsertPivotTableSheet
    | SelectSheet Name
    | RemoveSheet Name
    | EditSheet Name
    | UpdateSheetName Name
    | DocMsg Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    let
        commitDoc m =
            { m | doc = Doc.commitEdit m.doc }

        commitSheetName m =
            case m.edit of
                EditingSheetName oldName newName ->
                    { m
                        | edit = NotEditing
                        , doc =
                            Doc.renameSheet oldName newName m.doc
                                |> R.withDefault m.doc
                    }

                NotEditing ->
                    m

        insertSheet sheet =
            { model
                | doc =
                    R.withDefault model.doc <|
                        Doc.insertSheet
                            ("Sheet" ++ String.fromInt model.sheetCounter)
                            sheet
                            model.doc
                , sheetCounter = model.sheetCounter + 1
            }
    in
    case Debug.log "update msg" msg of
        InsertGridSheet ->
            insertSheet Doc.gridSheet

        InsertTableSheet ->
            insertSheet Doc.tableSheet

        InsertPivotTableSheet ->
            insertSheet Doc.pivotTableSheet

        SelectSheet name ->
            { model
                | doc =
                    Doc.selectSheet name model.doc
                        |> R.withDefault model.doc
            }
                |> commitDoc
                |> commitSheetName

        RemoveSheet name ->
            commitDoc
                { model
                    | doc =
                        Doc.removeSheet name model.doc
                            |> R.withDefault model.doc
                }

        EditSheet name ->
            commitDoc { model | edit = EditingSheetName name name }

        UpdateSheetName name ->
            { model
                | edit =
                    case model.edit of
                        EditingSheetName oldName _ ->
                            EditingSheetName oldName name

                        _ ->
                            model.edit
            }

        DocMsg docMsg ->
            commitSheetName { model | doc = Doc.update docMsg model.doc }



-------------------------------------------------------------------------------
-- SUBS
-------------------------------------------------------------------------------


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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
            case ( positionedName, model.edit ) of
                ( Doc.Current _, EditingSheetName _ newName ) ->
                    li [ itemCss ]
                        [ input
                            [ value newName
                            , onInput UpdateSheetName
                            ]
                            []
                        ]

                ( Doc.Current name, _ ) ->
                    defaultItem name True

                ( Doc.Before name, _ ) ->
                    defaultItem name False

                ( Doc.After name, _ ) ->
                    defaultItem name False

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
            :: addSheet InsertPivotTableSheet "+pivot table"
            :: (Doc.sheetNames model.doc |> L.map sheetItem)
        )
