module Page.PageBuilder where

import Graphics.Element (..)
import Html
import Signal
import Signal (..)
import Window

import ColorScheme as C
import Component.Search as Search
import Component.TopBar as TopBar

type alias PageBuilder = (Int, Int) -> Element

-- Architecture inspired from https://gist.github.com/evancz/2b2ba366cae1887fe621
type Action = Search Search.Action
            | ClickOutsideDropdown

type alias State = { search : Search.State }

step : Action -> State -> State
step action state =
    case action of
        Search a -> { state | search <- Search.step a state.search }

actions : Signal Action
actions = mergeMany [ Search <~ Search.actions
                    ]

initialState : State
initialState = { search = Search.initialState }


-- Helper functions

buildPageInternal : PageBuilder -> (State -> (Int, Int) -> Element)
buildPageInternal pageBuilder state (windowWidth, windowHeight) =
    let mainView =
        flow down
            [ TopBar.viewWithSearchBar windowWidth (Search.searchBar state.search)
            , flow right
              [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
              , pageBuilder (windowWidth, windowHeight)
              ]
            ]
    in
        Html.div [] [ color C.background mainView |> Html.fromElement
                    , Search.dropdown state.search (TopBar.searchBarLeft windowWidth, 42)
                    ]
        |> Html.toElement windowWidth windowHeight


-- Use these to make a page with a topbar and search functionality

buildPage : PageBuilder -> Signal Element
buildPage pageBuilder =
    Signal.map2 (buildPageInternal pageBuilder)
                (foldp step initialState actions)
                Window.dimensions

buildPages : Signal PageBuilder -> Signal Element
buildPages pageBuilder =
    Signal.map3 buildPageInternal
                pageBuilder
                (foldp step initialState actions)
                Window.dimensions
