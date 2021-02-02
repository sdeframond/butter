module Main exposing (..)

--import Html exposing (Html, table, td, text, th, tr)

import Browser
import Css exposing (..)
import Css.Global as Global
import Dict as D exposing (Dict)
import Document as Doc
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
    | EditingSheet Name Name
    | EditingCell Name String


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


type Msg
    = EditCell Name String
    | UpdateEdit EditStatus
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

                EditingCell name str ->
                    { model
                        | edit = NotEditing
                        , doc = Doc.insert name str model.doc
                    }

                NotEditing ->
                    model
    in
    case Debug.log "update msg" msg of
        EditCell name str ->
            { commitEdit | edit = EditingCell name str }

        UpdateEdit edit ->
            { model | edit = edit }

        InsertSheet ->
            { model
                | doc =
                    R.withDefault model.doc <|
                        Doc.insertSheet
                            ("Sheet" ++ String.fromInt model.sheetCounter)
                            model.doc
                , sheetCounter = model.sheetCounter + 1
            }

        SelectSheet name ->
            { commitEdit
                | doc =
                    Doc.selectSheet name commitEdit.doc
                        |> R.withDefault commitEdit.doc
            }

        RemoveSheet name ->
            let
                doc =
                    Doc.removeSheet name commitEdit.doc
            in
            { commitEdit
                | doc =
                    Doc.removeSheet name commitEdit.doc
                        |> R.withDefault commitEdit.doc
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
        L.map toUnstyled
            [ Global.global
                [ Global.html [ height (pct 100) ]
                , Global.body
                    [ displayFlex
                    , flexDirection column
                    , height (pct 100)
                    ]
                ]
            , tableView model
            , sheetSelector model
            ]
    }


tableView : Model -> Html Msg
tableView model =
    let
        numberOfRow =
            40

        numberOfColumns =
            20

        valueToString val =
            case val of
                Err e ->
                    case e of
                        UndefinedNameError _ ->
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
                    td
                        [ css_
                        , onClick <|
                            EditCell name
                                (Doc.cellSource name model.doc |> R.withDefault "")
                        ]
                        [ Doc.get name model.doc
                            |> valueToString
                            |> text
                        ]

                css_ =
                    css
                        [ border3 (px 1) solid (rgb 230 230 230)
                        , minWidth (Css.em 10)
                        , height (px 20)
                        ]
            in
            case model.edit of
                EditingCell name_ str ->
                    if name == name_ then
                        td [ css_ ]
                            [ input
                                [ value str
                                , onInput <| (EditingCell name >> UpdateEdit)
                                ]
                                []
                            ]

                    else
                        defaultCell

                _ ->
                    defaultCell

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
        [ css
            [ borderCollapse collapse
            , width (pct 100)
            , display block
            , overflow scroll
            ]
        ]
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
                ( Current name, EditingSheet oldName newName ) ->
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

        addSheet =
            li
                [ itemCss
                , onClick InsertSheet
                ]
                [ text " + " ]
    in
    ul [] (addSheet :: (Doc.sheets model.doc |> L.map sheetItem))
