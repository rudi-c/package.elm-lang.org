module Component.Search where

import Debug
import Graphics.Element (..)
import Graphics.Input as Input
import Html
import Html (Html, Attribute, toElement)
import Html.Attributes (..)
import Html.Events
import Http
import Http (Request)
import Json.Decode as Json
import Json.Decode ((:=))
import List
import List (..)
import Signal
import Signal (..)
import String
import Text
import Keyboard

type SearchScope
    = Package String
    | Global

type Action = SearchBoxNone
            | SearchBoxTyped String
            | SearchResultArrived (Http.Response String)
            | Dimiss

type alias State = { query: String, results: Maybe (List ModuleMatch) }

-- Encodes a search result coming down from the server.
type alias ItemMatch = { item : String, boldRanges : List (Int, Int) }
type alias ModuleMatch = { moduleName : String, boldRanges : List (Int, Int),
                           matches : List ItemMatch }

initialState : State
initialState = { query = "", results = Nothing }

-- Signals/Actions

-- ESC dismisses the search results if any.
dimiss : Signal Action
dimiss =
    let escCode = 27
    in
        Keyboard.isDown escCode
        |> Signal.keepIf identity True -- Only want key down events
        |> Signal.map (\ _ -> Dimiss)

searchInput : Channel String
searchInput = channel ""

searchSignal : Signal String
searchSignal = subscribe searchInput

actions : Signal Action
actions =
    let responses = Http.send (searchRequest <~ searchSignal)
    in
        mergeMany [ constant SearchBoxNone
                  , SearchBoxTyped <~ searchSignal
                  , SearchResultArrived <~ responses
                  , dimiss
                  ]

-- Update

step : Action -> State -> State
step event state =
    case event of
        SearchBoxNone -> state
        SearchBoxTyped text -> { state | query <- text }
        SearchResultArrived results -> { state | results <- parseResults results }
        Dimiss -> { state | results <- Nothing }


-- View

searchBar : State -> Element
searchBar state = Html.div [] [ stringInput state.query ] |> toElement 400 30

dropdown : State -> (Int, Int) -> Html
dropdown state (x, y) =
    case state.results of
        Just result -> Html.div [ dropdownStyle x y ]
                                [ (searchResults state.query result) ]
        Nothing -> Html.div [] []


-- Build the HTML for the dropdown menu of results

boldedTextHelper : String -> List (Int, Int) -> Int -> List Html
boldedTextHelper string boldRanges currentIndex =
    case boldRanges of
        [] ->
            if currentIndex == (String.length string) then
                []
            else
                [String.dropLeft currentIndex string |> Html.text]
        (start, end) :: tail ->
            if currentIndex < start then
                let nonbold = (String.slice currentIndex start string |> Html.text)
                    rest = boldedTextHelper string boldRanges start
                in  nonbold :: rest
            else
                -- The ranges in boldRanges are inclusive ranges, whereas String.slice
                -- takes an exclusive range, so use (end + 1).
                let slice = String.slice start (end + 1) string
                    bold = Html.strong [] [ Html.text slice ]
                    rest = boldedTextHelper string tail (end + 1)
                in bold :: rest

boldedText : String -> List (Int, Int) -> Html
boldedText string boldRanges =
    -- Span to "concatenate" the pieces of the string
    Html.span [] (boldedTextHelper string boldRanges 0)

boldedItems : ModuleMatch -> List Html
boldedItems match =
    let boldedItem item =
        let relativeLink = match.moduleName ++ "#" ++ item.item
        -- block style needed to make entire blocks clickable
        in
            Html.a [ href relativeLink, displayBlockStyle ]
                   [ boldedText item.item item.boldRanges ]
    in
        -- ui-menu-item is to tag something as clickable
        (List.map (\ match -> Html.li [ resultItemStyle ] [boldedItem match])
             match.matches)

searchResult : String -> ModuleMatch -> List Html
searchResult query match =
    let mainText =
            Html.a [ href match.moduleName, displayBlockStyle ]
                        [ boldedText match.moduleName match.boldRanges ]
        main = Html.li [ moduleItemStyle ] [ mainText ]
        rest = boldedItems match
    in
        main :: rest

searchResults : String -> List ModuleMatch -> Html
searchResults query matches =
    Html.ul [ noPadding ] (matches |> List.map (searchResult query) |> concat)

stringInput : String -> Html
stringInput string =
    Html.input
        [ placeholder "Search..."
        , value string
        , Html.Events.on "input" Html.Events.targetValue (send searchInput)
        , searchBarStyle
        ]
        []


-- Styles

makePx : Int -> String
makePx int = toString int ++ "px"

displayBlockStyle : Attribute
displayBlockStyle = style [ ("display", "block") ]

noPadding : Attribute
noPadding = style [ ("padding", "0px"), ("margin", "0px") ]

searchBarStyle : Attribute
searchBarStyle =
    style [
      ("width", "100%")
    , ("height", "20px")
    , ("padding", "4px 5px")
    , ("font-size", "1.2em")
    ]

dropdownStyle : Int -> Int -> Attribute
dropdownStyle x y =
    style [
        ("position", "absolute"),
        ("background", "#ffffff"),
        ("z-index", "1001"),
        ("tabindex", "0"),
        ("left", makePx x),
        ("top", makePx y),
        ("width", "414px")
    ]

moduleItemStyle  : Attribute
moduleItemStyle  =
    style [
        ("display", "block"),
        ("padding", "4px 7px"),
        ("margin", "0"),
        ("border", "1px solid #181818"),
        ("border-top", "none"),
        ("color", "#222222"),
        ("font-family", "Helvetica"),
        ("letter-spacing", "0"),
        ("line-height", "100%"),
        ("cursor", "pointer")
    ]

resultItemStyle : Attribute
resultItemStyle =
    style [
        ("display", "block"),
        ("padding", "4px 25px"),
        ("margin", "0"),
        ("border", "1px solid #181818"),
        ("border-top", "none"),
        ("color", "#222222"),
        ("font-family", "monospace"),
        ("letter-spacing", "0"),
        ("line-height", "100%"),
        ("cursor", "pointer")
    ]


-- Querying the server for search results

searchRequest : String -> Request String
searchRequest query =
    if String.isEmpty query then
        -- Request won't get sent with nil HTTP.
        Http.get ""
    else
        Http.get <| "http://rudichen.me:9000/search/elm-lang-Elm/" ++ query ++ "?version=0.13"

responseDecoder : Json.Decoder (List ModuleMatch)
responseDecoder =
    let rangeDecoder = Json.map (\ range -> (head range, head (tail range)))
                                (Json.list Json.int)
        rangesDecoder = Json.list rangeDecoder
        matchDecoder =
            Json.object2
                (\ name ranges -> { item = name, boldRanges = ranges })
                ("name" := Json.string)
                ("boldRanges" := rangesDecoder)
        matchesDecoder = Json.list matchDecoder
        modulesDecoder =
            Json.object3
                (\ name matches boldRanges ->
                    { moduleName = name, matches = matches, boldRanges = boldRanges })
                ("module_name" := Json.string)
                ("matches" := matchesDecoder)
                ("boldRanges" := rangesDecoder)
    in
        Json.list modulesDecoder

parseResults : Http.Response String -> Maybe (List ModuleMatch)
parseResults response =
    case response of
        Http.Success result ->
            case Json.decodeString responseDecoder result of
                Ok value -> Just value
                Err error -> Debug.log error Nothing
        Http.Waiting -> Just [{ moduleName = "Waiting", boldRanges = [], matches = [] }]
        Http.Failure code msg -> Just [{ moduleName = "Failure: " ++ msg,
                                         boldRanges = [], matches = [] }
                                      ]

-- This is a hardcoded search result which can be useful for testing,
-- so I'm leaving it here.
--search : String -> List ModuleMatch
--search = [ { moduleName = "Array", boldRanges = [],
--             matches = [{ item = "filter", boldRanges = [(0,0), (2,3), (5,5)] }]
--           },
--           { moduleName = "List", boldRanges = [],
--             matches = [{ item = "filter", boldRanges = [(0,0), (2,3), (5,5)] },
--                        { item = "filterMap", boldRanges = [(0,0), (2,3), (5,5)] }
--                       ]
--           }
--         ]
