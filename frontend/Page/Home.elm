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

-- View

main : Signal.Signal Element
main =
    Signal.map3 scene
                Window.dimensions
                (Signal.constant [Packages.Package "elm-lang/core" "core libraries" ["1.0.0"]])
                TopBar.topBar

scene : (Int,Int) -> List Packages.Package -> Element -> Element
scene (windowWidth, windowHeight) packages topBar =
  let packageListing =
        Packages.view 980 packages
  in
  color C.background <|
  flow down
  [ topBar
  , container windowWidth (max windowHeight (heightOf packageListing)) midTop packageListing
  ]
