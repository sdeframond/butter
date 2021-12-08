module Ui exposing (button, column, editableListItem, fullRow, row)

import Css exposing (..)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode


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


type alias EditableListItemConfig msg =
    { onSelect : () -> msg
    , onEdit : () -> msg
    , onRemove : () -> msg
    , onUpdate : String -> msg
    }


type alias EditableListItem =
    { name : String
    , isCurrent : Bool
    , editStatus : Maybe String
    }


lazyOnClickStopPropagation : (() -> msg) -> H.Attribute msg
lazyOnClickStopPropagation f =
    Events.stopPropagationOn "click"
        (Decode.succeed () |> Decode.map f |> Decode.map (\msg -> ( msg, True )))


lazyOnClick : (() -> msg) -> H.Attribute msg
lazyOnClick f =
    Events.on "click" (Decode.succeed () |> Decode.map f)


lazyOnDoubleClick : (() -> msg) -> H.Attribute msg
lazyOnDoubleClick f =
    Events.on "dblclick" (Decode.succeed () |> Decode.map f)


editableListItem : EditableListItemConfig msg -> EditableListItem -> Html msg
editableListItem { onSelect, onEdit, onRemove, onUpdate } item =
    let
        defaultItem isCurrent =
            button
                [ css
                    [ if isCurrent then
                        fontWeight bold

                      else
                        fontWeight normal
                    , displayFlex
                    , justifyContent spaceBetween
                    , overflow hidden
                    ]
                , lazyOnClick onSelect
                , lazyOnDoubleClick onEdit
                ]
                [ H.span
                    [ css
                        [ overflow hidden
                        , textOverflow ellipsis
                        , marginRight (px 5)
                        ]
                    ]
                    [ H.text item.name ]
                , H.span [ lazyOnClickStopPropagation onRemove ]
                    [ H.text "[x]" ]
                ]
    in
    case ( item.isCurrent, item.editStatus ) of
        ( _, Just newName ) ->
            button []
                [ H.input
                    [ Attr.value newName
                    , Events.onInput onUpdate
                    ]
                    []
                ]

        ( isCurrent, _ ) ->
            defaultItem isCurrent
