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
        , bar logoSize
            (link "/" (image logoSize logoSize "/assets/elm_logo.svg"))
        , link "/packages" (bar searchBarWidth (Text.leftAligned (Text.color (Color.rgb 5 80 129) (Text.fromString "Packages"))))
        , barLeft (innerWidth - logoSize - searchBarWidth + rightPadding) item
        ]

view : Int -> Element
view outerWidth = viewWithItem outerWidth empty

viewWithSearchBar : Int -> Element -> Element
viewWithSearchBar outerWidth searchBar = viewWithItem outerWidth searchBar


searchBarLeft : Int -> Int
searchBarLeft outerWidth =
    let leftPadding = (outerWidth - innerWidth) // 2
    in leftPadding + logoSize + searchBarWidth


bar : Int -> Element -> Element
bar fillerWidth elem =
  color C.blue (container fillerWidth topBarHeight middle elem)

barLeft : Int -> Element -> Element
barLeft fillerWidth elem =
  color C.blue (container fillerWidth topBarHeight midLeft elem)
