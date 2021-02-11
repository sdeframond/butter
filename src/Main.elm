module Main exposing (..)

import Browser
import Css exposing (..)
import Css.Global as Global
import Document as Doc
import Document.Types exposing (Name, Position(..))
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onDoubleClick, onInput)
import List as L
import Result as R


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
    | SelectSheet Name
    | RemoveSheet Name
    | EditSheet Name
    | UpdateSheetName Name
    | DocMsg Doc.Msg
    | DocCommitMsg Doc.Msg


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

        DocMsg docMsg ->
            { model | doc = Doc.update docMsg model.doc }

        DocCommitMsg docMsg ->
            { commited | doc = Doc.update docMsg commited.doc }



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
        docConfig =
            { toMsg = DocMsg
            , toCommitMsg = DocCommitMsg
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
                ( Current name, EditingSheetName oldName newName ) ->
                    li [ itemCss ]
                        [ input
                            [ value newName
                            , onInput UpdateSheetName
                            ]
                            []
                        ]

                ( Current name, _ ) ->
                    defaultItem name True

                ( Before name, _ ) ->
                    defaultItem name False

                ( After name, _ ) ->
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
            :: (Doc.sheetNames model.doc |> L.map sheetItem)
        )
