module Grid exposing (Cmd(..), Config, Grid, Msg, commit, init, update, view)

import Css exposing (..)
import Types exposing (Error(..), Name, Value(..), ValueOrError)
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
    { toMsg : Msg -> msg
    , getCellValue : Name -> ValueOrError
    , getCellSource : Name -> String
    }


init : Grid
init =
    Grid Nothing


type Cmd
    = NoCmd
    | CommitChangesCmd String String


type Msg
    = StartEditing String String
    | UpdateEdit String String


update : Msg -> Grid -> ( Grid, Cmd )
update msg (Grid editState) =
    case msg of
        UpdateEdit name content ->
            ( Grid (Just ( name, content )), NoCmd )

        StartEditing name content ->
            ( Grid (Just ( name, content ))
            , case editState of
                Nothing ->
                    NoCmd

                Just ( oldName, oldContent ) ->
                    CommitChangesCmd oldName oldContent
            )


commit : Grid -> ( Grid, Cmd )
commit (Grid editState) =
    ( init
    , case editState of
        Nothing ->
            NoCmd

        Just ( name, content ) ->
            CommitChangesCmd name content
    )


view : Config msg -> Grid -> Html msg
view { toMsg, getCellSource, getCellValue } (Grid editState) =
    let
        numberOfRow =
            40

        numberOfColumns =
            20

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
                        , onClick <| toMsg <| StartEditing cellName (getCellSource cellName)
                        ]
                        [ getCellValue cellName
                            |> Types.valueOrErrorToString
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
                                , onInput <| (toMsg << UpdateEdit cellName)
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
        [ css [ borderCollapse collapse ] ]
        [ thead [ css [ position sticky, top (px 0) ] ] columnHeaders
        , tbody [] rows
        ]
