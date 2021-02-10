module Document.Grid exposing (Config, Grid, commit, init, view)

import Css exposing (..)
import Document.Types exposing (Error(..), Name, Value(..), ValueOrError)
import Html.Styled as H exposing (..)
import Html.Styled.Attributes exposing (css, value)
import Html.Styled.Events exposing (onClick, onInput)
import List as L
import String as S


type Grid
    = Grid EditState


type alias EditState =
    Maybe ( Name, String )


type alias Config msg =
    { toMsg : Grid -> msg
    , commitMsg : Grid -> msg
    , getCellValue : Name -> ValueOrError
    , getCellSource : Name -> String
    }


init : Grid
init =
    Grid Nothing


commit : (EditState -> a) -> Grid -> ( a, Grid )
commit commiter (Grid editState) =
    ( commiter editState, init )


view : Config msg -> Grid -> Html msg
view { toMsg, getCellSource, getCellValue, commitMsg } (Grid editState) =
    let
        editGrid name str =
            Grid (Just ( name, str ))

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
                ]

        columnHeaders =
            th [] []
                :: mapColumns
                    (\col ->
                        myTh [ css [ top (px 0) ] ] [ text <| toLetter col ]
                    )

        rowHeader row =
            myTh [ css [ left (px 0), position sticky ] ] [ text <| S.fromInt row ]

        cell row col =
            let
                cellName =
                    [ col |> toLetter, row |> S.fromInt ]
                        |> S.concat

                defaultCellView =
                    td
                        [ cellCss
                        , onClick <| commitMsg <| editGrid cellName (getCellSource cellName)
                        ]
                        [ getCellValue cellName
                            |> valueToString
                            |> text
                        ]

                cellCss =
                    css
                        [ border3 (px 1) solid (rgb 230 230 230)
                        , minWidth (Css.em 10)
                        , height (px 20)
                        ]
            in
            case editState of
                Just ( editedCellName, str ) ->
                    if cellName == editedCellName then
                        td [ cellCss ]
                            [ input
                                [ value str
                                , onInput <| (toMsg << editGrid cellName)
                                ]
                                []
                            ]

                    else
                        defaultCellView

                Nothing ->
                    defaultCellView

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
            , height (pct 100)
            , display block
            , overflow scroll
            ]
        ]
        [ thead [ css [ position sticky, top (px 0) ] ] columnHeaders
        , tbody [] rows
        ]
