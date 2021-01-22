module Main exposing (..)

--import Html exposing (Html, table, td, text, th, tr)

import Browser
import Css exposing (..)
import Css.Global as Global
import Dict as D exposing (Dict)
import Document as Doc exposing (Name, Value(..), ValueOrError)
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
    , currentSheet : Name
    , sheetCounter : Int
    }


type EditStatus
    = NotEditing
    | EditingSheet Name String
    | EditingCell ( Col, Row )


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
    , currentSheet = "Sheet1"
    , sheetCounter = 2
    , edit = NotEditing
    }



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


type Msg
    = EditCell Col Row
    | UpdateSource Name String
    | InsertSheet
    | SelectSheet Name
    | RemoveSheet Name
    | EditSheet Name
    | UpdateSheetName Name


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    let
        commitEdit =
            case model.edit of
                EditingSheet oldName newName ->
                    { model
                        | edit = NotEditing
                        , doc =
                            Doc.renameSheet oldName newName model.doc
                                |> R.withDefault model.doc
                    }

                _ ->
                    model
    in
    case Debug.log "update msg" msg of
        EditCell col row ->
            { commitEdit | edit = EditingCell ( col, row ) }

        UpdateSource name src ->
            { model | doc = Doc.insert model.currentSheet name src model.doc }

        InsertSheet ->
            { model
                | doc =
                    Doc.insertSheet
                        ("Sheet" ++ String.fromInt model.sheetCounter)
                        model.doc
                , sheetCounter = model.sheetCounter + 1
            }

        SelectSheet name ->
            if L.member name (Doc.sheets model.doc) then
                { commitEdit | currentSheet = name }

            else
                model

        RemoveSheet name ->
            let
                doc =
                    Doc.removeSheet name commitEdit.doc
            in
            case Doc.sheets doc of
                [] ->
                    model

                head :: rest ->
                    Debug.log ""
                        { commitEdit
                            | doc = doc
                            , currentSheet =
                                if L.member commitEdit.currentSheet rest then
                                    commitEdit.currentSheet

                                else
                                    head
                        }

        EditSheet name ->
            { commitEdit | edit = EditingSheet name name }

        UpdateSheetName name ->
            { model
                | edit =
                    case model.edit of
                        EditingSheet oldName _ ->
                            EditingSheet oldName name

                        _ ->
                            model.edit
            }



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
    , body =
        [ Global.global
            [ Global.html [ height (pct 100) ]
            , Global.body
                [ displayFlex
                , flexDirection column
                , height (pct 100)
                ]
            ]
            |> toUnstyled
        , tableView model |> toUnstyled
        , sheetSelector model |> toUnstyled
        ]
    }


tableView : Model -> Html Msg
tableView model =
    let
        numberOfRow =
            40

        numberOfColumns =
            10

        valueToString val =
            case val of
                Err e ->
                    case e of
                        Doc.UndefinedNameError _ ->
                            ""

                        r ->
                            Debug.toString r

                Ok v ->
                    case v of
                        IntValue i ->
                            String.fromInt i

                        StringValue s ->
                            s

        toLetter i =
            Char.fromCode (i - 1 + Char.toCode 'A') |> S.fromChar

        mapColumns f =
            L.range 1 numberOfColumns |> L.map f

        blueGrey =
            rgb 220 220 240

        myTh =
            styled th
                [ backgroundColor blueGrey
                , padding2 (Css.em 0.2) (Css.em 0.4)
                , border3 (px 1) solid (rgb 150 150 150)
                , position sticky
                ]

        columnHeaders =
            tr []
                (th [] []
                    :: mapColumns
                        (\col ->
                            myTh [ css [ top (px 0) ] ] [ text <| toLetter col ]
                        )
                )

        rowHeader row =
            myTh [ css [ left (px 0) ] ] [ text <| S.fromInt row ]

        cell row col =
            let
                name =
                    [ col |> toLetter, row |> String.fromInt ]
                        |> S.concat

                defaultCell =
                    Doc.get model.currentSheet name model.doc
                        |> valueToString
                        |> text

                editCell =
                    input
                        [ value (Doc.source model.currentSheet name model.doc |> M.withDefault "")
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
                    EditingCell coord ->
                        if coord == ( col, row ) then
                            editCell

                        else
                            defaultCell

                    _ ->
                        defaultCell
                ]

        rows =
            L.range 1 numberOfRow
                |> L.map
                    (\row ->
                        tr [] <|
                            rowHeader row
                                :: mapColumns (cell row)
                    )
    in
    H.table
        [ css [ borderCollapse collapse, width (pct 100), display block, overflow scroll ] ]
        (columnHeaders :: rows)


sheetSelector : Model -> Html Msg
sheetSelector model =
    let
        itemCss =
            css
                [ border3 (px 1) solid (rgb 100 100 100)
                , display inlineBlock
                , padding2 (px 5) (px 5)
                ]

        sheetItem name =
            let
                defaultItem =
                    li
                        [ itemCss
                        , css
                            [ if name == model.currentSheet then
                                fontWeight bold

                              else
                                fontWeight normal
                            ]
                        , onClick <| SelectSheet name
                        , onDoubleClick <| EditSheet name
                        ]
                        [ text name
                        , span [ onClick <| RemoveSheet name ] [ text "[x]" ]
                        ]
            in
            case model.edit of
                EditingSheet oldName newName ->
                    if oldName == name then
                        li [ itemCss ]
                            [ input [ value newName, onInput UpdateSheetName ] [] ]

                    else
                        defaultItem

                _ ->
                    defaultItem

        addSheet =
            li
                [ itemCss
                , onClick InsertSheet
                ]
                [ text " + " ]
    in
    ul [] (addSheet :: (Doc.sheets model.doc |> L.map sheetItem))
