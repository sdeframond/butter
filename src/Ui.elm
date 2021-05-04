module Ui exposing (column, row)

import Css exposing (..)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)


row : List (Html msg) -> Html msg
row content =
    H.div
        [ css
            [ displayFlex
            , flexDirection Css.row
            , height (pct 100)
            ]
        ]
        content


column : List (Html msg) -> Html msg
column content =
    H.div
        [ css
            [ flex3 (int 0) (int 0) (px 100)
            , border3 (px 1) solid (rgb 0 0 0)
            , height (pct 100)
            , display inlineBlock
            , displayFlex
            , flexDirection Css.column
            ]
        ]
        content
