module Page.PackageDocs where

import Basics (..)
import Color
import ColorScheme as C
import Dict
import Json.Decode (..)
import Json.Decode as Json
import Graphics.Element (..)
import Http
import List
import LocalChannel as LC
import Signal
import String
import Window

import Component.PackageDocs as Docs
import Page.PageBuilder (PageBuilder, buildPages)


port context : { user : String, name : String, version : String, versionList : List String }

port title : String
port title =
    context.user ++ "/" ++ context.name ++ " " ++ context.version


packageUrl : String -> String
packageUrl version =
  "/packages/" ++ context.user ++ "/" ++ context.name ++ "/" ++ version


descriptionUrl : String
descriptionUrl =
  packageUrl context.version ++ "/elm-package.json"


description : Signal Docs.PackageInfo
description =
    Http.sendGet (Signal.constant descriptionUrl)
      |> Signal.map handleResult


packageInfo : List String -> Docs.PackageInfo
packageInfo modules =
  Docs.PackageInfo context.user context.name context.version context.versionList modules


handleResult : Http.Response String -> Docs.PackageInfo
handleResult response =
  case response of
    Http.Success msg ->
      case Json.decodeString ("exposed-modules" := list string) msg of
        Err _ -> packageInfo []
        Ok modules ->
            packageInfo modules

    _ -> packageInfo []


readmeUrl : String
readmeUrl =
  packageUrl context.version ++ "/README.md"


readme : Signal (Maybe String)
readme =
    Http.sendGet (Signal.constant readmeUrl)
      |> Signal.map extractReadme


extractReadme : Http.Response String -> Maybe String
extractReadme response =
  case response of
    Http.Success str -> Just str
    _ -> Nothing


main : Signal Element
main =
    buildPages (Signal.map2 pageBuilder description readme)

pageBuilder : Docs.PackageInfo -> Maybe String -> PageBuilder
pageBuilder packages readme (windowWidth, windowHeight) =
    Docs.view (LC.create identity versionChan) 980 packages readme

versionChan : Signal.Channel String
versionChan =
    Signal.channel ""


port redirect : Signal String
port redirect =
  Signal.keepIf ((/=) "") "" (Signal.subscribe versionChan)
    |> Signal.map packageUrl

