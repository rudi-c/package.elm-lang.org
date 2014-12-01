module Component.TopBar where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Signal
import Signal (..)
import Text
import Window

import Component.SearchBar as SearchBar

-- Model & Updates, based on Elm Architecture guide at
-- https://gist.github.com/evancz/2b2ba366cae1887fe621

-- Only one type of action right now, but this can be extended.
type Action = SearchBar SearchBar.Action

type alias State = { searchBar : SearchBar.State }

initialState : State
initialState = { searchBar = SearchBar.initialState }

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


view : Int -> State -> Element
view outerWidth state =
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
  , bar (innerWidth - topBarHeight - searchBarWidth + rightPadding)
        (SearchBar.view state.searchBar)
  ]


bar : Int -> Element -> Element
bar fillerWidth elem =
  flow down
  [ color C.lightGrey (container fillerWidth topBarHeight middle elem)
  , color C.mediumGrey (spacer fillerWidth 1)
  ]

topBar : Signal Element
topBar = Signal.map2 view (fst <~ Window.dimensions)
                          (foldp step initialState actions)
