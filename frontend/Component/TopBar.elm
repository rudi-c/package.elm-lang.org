module Component.TopBar where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Signal (..)
import Text

import Component.SearchBar as SearchBar

type Action = SearchBar SearchBar.Action

type alias State = { searchBar : SearchBar.State }

step : Action -> State -> State
step action state =
    case action of
        SearchBar a -> { state | searchBar <- SearchBar.step a state.searchBar }

actions : Signal Action
actions = SearchBar <~ SearchBar.actions

type SearchScope
    = Package String
    | Global


type Status
    = Normal
    | Hover
    | Focus


type alias Model =
    { searchScope : SearchScope
    , query : String
    , status : Status
    }


type Update = NoOp


topBarHeight = 50

innerWidth = 980

logoSize = 28

searchBarWidth = 100


view : Int -> Channel Update -> Model -> Element
view outerWidth channel model =
  let leftPadding =
        (outerWidth - innerWidth) // 2

      rightPadding =
        outerWidth - innerWidth - leftPadding
  in
  flow right
  [ bar leftPadding empty
  , bar topBarHeight
      (link "/" (image logoSize logoSize "/assets/elm_logo.svg"))
  , bar searchBarWidth (link "/packages" (Text.plainText "Packages"))
  , bar (innerWidth - topBarHeight - searchBarWidth + rightPadding) empty
  ]


bar : Int -> Element -> Element
bar fillerWidth elem =
  flow down
  [ color C.lightGrey (container fillerWidth topBarHeight middle elem)
  , color C.mediumGrey (spacer fillerWidth 1)
  ]

