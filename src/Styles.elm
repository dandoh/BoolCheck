module Styles exposing (..)

import Html exposing (Attribute, div)
import Html.Attributes exposing (style)


displayFlex : Attribute msg
displayFlex =
    style "display" "flex"


flexRow : Attribute msg
flexRow =
    style "flex-direction" "row"


flexCol : Attribute msg
flexCol =
    style "flex-direction" "column"


alignCenter : Attribute msg
alignCenter =
    style "align-items" "center"

alignEnd : Attribute msg
alignEnd =
    style "align-items" "flex-end"

justifyCenter : Attribute msg
justifyCenter =
    style "justify-content" "center"


justifyEnd : Attribute msg
justifyEnd =
    style "justify-content" "flex-end"


justifySpaceBtw : Attribute msg
justifySpaceBtw =
    style "justify-content" "space-between"


h1bs =
    [ style "font-size" "2em"
    , style "font-weight" "bold"
    , style "display" "block"
    ]


h2bs =
    [ style "font-size" "1.5em"
    , style "font-weight" "bold"
    , style "display" "block"
    ]


h3bs =
    [ style "font-size" "1.17em"
    , style "font-weight" "bold"
    , style "display" "block"
    ]


h4bs =
    [ style "font-size" "1em"
    , style "font-weight" "bold"
    , style "display" "block"
    ]


h5bs =
    [ style "font-size" "0.83em"
    , style "font-weight" "bold"
    , style "display" "block"
    ]

clrPrimary =
    "rgb(0, 123, 255)"



overflowEllip =
    style "text-overflow" "ellipsis"


type Display
    = Flex
    | Block
    | Inline


type FlexDirection
    = FlexRow
    | FlexColumn


type FlexAlign
    = AlignCenter
    | AlignStart
    | AlignEnd


type FlexJustify
    = JustifyCenter
    | JustifyStart
    | JustifyEnd
    | JustifySpaceBetween
    | JustifySpaceAround


stWidth =
    style "width"


stHeight =
    style "height"


display d =
    case d of
        Flex ->
            style "display" "flex"

        Block ->
            style "display" "block"

        Inline ->
            style "display" "inline"


flexDirection d =
    case d of
        FlexRow ->
            style "flex-direction" "row"

        FlexColumn ->
            style "flex-direction" "column"


alignItems a =
    case a of
        AlignCenter ->
            style "align-items" "center"

        AlignStart ->
            style "align-items" "flex-start"

        AlignEnd ->
            style "align-items" "flex-end"

flex =
    style "flex"

justifyContent a =
    case a of
        JustifyCenter ->
            style "justify-content" "center"

        JustifyStart ->
            style "justify-content" "flex-start"

        JustifyEnd ->
            style "justify-content" "flex-end"

        JustifySpaceBetween ->
            style "justify-content" "space-between"

        JustifySpaceAround ->
            style "justify-content" "space-around"


rowDiv attrs children =
    div ([ display Flex, flexDirection FlexRow ] ++ attrs) children


colDiv attrs children =
    div ([ display Flex, flexDirection FlexColumn ] ++ attrs) children


zIndex =
    style "z-index"


overflow =
    style "overflow"


overflowX =
    style "overflow-x"


overflowY =
    style "overflow-y"

padding =
    style "padding"

margin =
    style "margin"

paddingLeft =
    style "padding-left"

paddingBottom =
    style "padding-bottom"

paddingTop =
    style "padding-top"

paddingRight =
    style "padding-right"

marginLeft =
    style "margin-left"

marginBottom =
    style "margin-bottom"

marginTop =
    style "margin-top"

marginRight =
    style "margin-right"

backgroundColor =
    style "background-color"

color =
    style "color"

fullWidth =
    style "width" "100%"

fullHeight =
    style "height" "100%"

fontSize =
    style "font-size"


fontFamily =
    style "font-family"


fontWeight =
    style "font-weight"


maxHeight =
    style "max-height"


minHeight =
    style "min-height"

border =
    style "border"

borderRadius =
    style "border-radius"

whiteSpace =
    style "white-space"

textShadow =
    style "text-shadow"

cursor =
    style "cursor"

position =
    style "position"

top =
    style "top"

left =
    style "left"

right =
    style "right"
