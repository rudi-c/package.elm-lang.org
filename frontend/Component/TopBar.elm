module Component.TopBar where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Signal
import Signal (..)
import Text
import Window

topBarHeight = 50

innerWidth = 980

logoSize = 28

searchBarWidth = 100


viewWithItem : Int -> Element -> Element
viewWithItem outerWidth item =
    let leftPadding = (outerWidth - innerWidth) // 2
        rightPadding = outerWidth - innerWidth - leftPadding
    in
        flow right
        [ bar leftPadding empty
        , bar topBarHeight
            (link "/" (image logoSize logoSize "/assets/elm_logo.svg"))
        , bar searchBarWidth (link "/packages" (Text.plainText "Packages"))
        , bar (innerWidth - topBarHeight - searchBarWidth + rightPadding) item
        ]

view : Int -> Element
view outerWidth = viewWithItem outerWidth empty

viewWithSearchBar : Int -> Element -> Element
viewWithSearchBar outerWidth searchBar = viewWithItem outerWidth searchBar


searchBarLeft : Int -> Int
searchBarLeft outerWidth =
    let leftPadding = (outerWidth - innerWidth) // 2
    in leftPadding + topBarHeight + searchBarWidth


bar : Int -> Element -> Element
bar fillerWidth elem =
  flow down
  [ color C.lightGrey (container fillerWidth topBarHeight midLeft elem)
  , color C.mediumGrey (spacer fillerWidth 1)
  ]
