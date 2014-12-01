module Page.Packages where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Http
import Json.Decode as Json
import Signal
import String
import Window

import Component.Search as Search
import Component.TopBar as TopBar
import Component.Packages as Packages


port title : String
port title =
    "Elm Packages"

main : Signal Element
main =
    Signal.map3 view Window.dimensions packages Search.searchState


view : (Int,Int) -> List Packages.Package -> Search.State -> Element
view (windowWidth, windowHeight) packages searchState =
  color C.background <|
  flow down
  [ TopBar.viewWithSearchBar windowWidth (Search.searchBar searchState)
  , flow right
    [ spacer ((windowWidth - 980) // 2) (windowHeight - TopBar.topBarHeight)
    , Packages.view 980 packages
    ]
  ]


allPackagesUrl : String
allPackagesUrl =
    "/all-packages"


packages : Signal (List Packages.Package)
packages =
    Http.sendGet (Signal.constant allPackagesUrl)
      |> Signal.map handleResult


handleResult : Http.Response String -> List Packages.Package
handleResult response =
  case response of
    Http.Success msg ->
      case Json.decodeString (Json.list Packages.package) msg of
        Ok packages -> packages
        Err _ -> []

    _ -> []
