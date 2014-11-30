module Page.Home where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Signal
import Signal (..)
import String
import Window

import Component.TopBar as TopBar
import Component.Packages as Packages

-- Model & Updates, based on Elm Architecture guide at
-- https://gist.github.com/evancz/2b2ba366cae1887fe621

-- Only one type of action right now, but this can be extended.
type Action = TopBar TopBar.Action

type alias State = { topBar : TopBar.State }

initialState : State
initialState = { topBar = TopBar.initialState }

step : Action -> State -> State
step action state =
    case action of
        TopBar a -> { state | topBar <- TopBar.step a state.topBar }

actions : Signal Action
actions = TopBar <~ TopBar.actions


-- View

main : Signal.Signal Element
main =
    Signal.map3 scene
                Window.dimensions
                (Signal.constant [Packages.Package "elm-lang/core" "core libraries" ["1.0.0"]])
                (foldp step initialState actions)


search : Signal.Channel TopBar.Update
search =
    Signal.channel TopBar.NoOp


scene : (Int,Int) -> List Packages.Package -> State -> Element
scene (windowWidth, windowHeight) packages state =
  let packageListing =
        Packages.view 980 packages
  in
  color C.background <|
  flow down
  [ TopBar.view windowWidth search (TopBar.Model TopBar.Global "map" TopBar.Normal)
  , container windowWidth (max windowHeight (heightOf packageListing)) midTop packageListing
  ]
