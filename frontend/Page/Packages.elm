module Page.Packages where

import Color
import ColorScheme as C
import Graphics.Element (..)
import Html
import Http
import Json.Decode as Json
import Signal
import Signal (..)
import String
import Window

import Component.Packages as Packages
import Page.PageBuilder (PageBuilder, buildPages)


port title : String
port title =
    "Elm Packages"

main : Signal Element
main = buildPages (pageBuilder <~ packages)

pageBuilder : List Packages.Package -> PageBuilder
pageBuilder packages (windowWidth, windowHeight) =
    Packages.view 980 packages


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
