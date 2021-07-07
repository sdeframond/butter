module Ui exposing (button, column, fullRow, row)

import Css exposing (..)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes exposing (css)


fullRow : List (H.Attribute msg) -> List (Html msg) -> Html msg
fullRow attrs content =
    row (css [ height (pct 100) ] :: attrs)
        content


row : List (H.Attribute msg) -> List (Html msg) -> Html msg
row attrs content =
    H.div
        (css
            [ displayFlex
            , flexDirection Css.row
            ]
            :: attrs
        )
        content


column : List (H.Attribute msg) -> List (Html msg) -> Html msg
column attrs content =
    H.div
        (css
            [ flex3 (int 0) (int 0) (px 100)
            , border3 (px 1) solid (rgb 0 0 0)
            , height (pct 100)
            , displayFlex
            , flexDirection Css.column
            , padding (px 10)
            ]
            :: attrs
        )
        content


button : List (H.Attribute msg) -> List (Html msg) -> Html msg
button attrs content =
    H.div
        (css
            [ border3 (px 1) solid (rgb 100 100 100)
            , padding2 (px 5) (px 5)
            ]
            :: attrs
        )
        content
