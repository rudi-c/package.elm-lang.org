module Searchbox where

import Graphics.Input as Input
import Html (Html, CssProperty, style, prop, text, on, getValue, toElement)
import Html.Tags
import Html.Tags (div, input, span, strong, ul, li)
import Html.Attributes (..)
import Http
import Json.Decode as Json
import Json.Decode ((:=))
import String

data Event = SearchBoxTyped String | SearchResultArrived (Http.Response String)

type ItemMatch = { item : String, boldRanges : [(Int, Int)] }
type ModuleMatch = { moduleName : String, boldRanges : [(Int, Int)], matches : [ItemMatch] }

searchUrl : String -> String
searchUrl query = "http://rudichen.me:9000/search/elm-lang-Elm/" ++ query ++ "?version=0.13"

-- VIEW

scene : SearchState -> Element
scene state =
    let dropdown =
        case state.results of
            Just result -> [ searchResults state.query result ]
            Nothing -> []
    in
        div [] ([ text state.query, stringInput state.query ] ++ dropdown)
        |> toElement 400 200

-- Hardcoded for now
search : String -> [ModuleMatch]
search query = [ { moduleName = "Array", boldRanges = [],
                   matches = [{ item = "filter", boldRanges = [(0,0), (2,3), (5,5)] }]
                 },
                 { moduleName = "List", boldRanges = [],
                   matches = [{ item = "filter", boldRanges = [(0,0), (2,3), (5,5)] },
                              { item = "filterMap", boldRanges = [(0,0), (2,3), (5,5)] }
                             ]
                 }
               ]

boldedTextHelper : String -> [(Int, Int)] -> Int -> [Html]
boldedTextHelper string boldRanges currentIndex =
    case boldRanges of
        [] ->
            if currentIndex == (String.length string) then
                []
            else
                [String.dropLeft currentIndex string |> text]
        (start, end) :: tail ->
            if currentIndex < start then
                let nonbold = (String.slice currentIndex start string |> text)
                    rest = boldedTextHelper string boldRanges start
                in  nonbold :: rest
            else
                -- The ranges in boldRanges are inclusive ranges, whereas String.slice
                -- takes an exclusive range, so use (end + 1).
                let bold = strong [] [String.slice start (end + 1) string |> text]
                    rest = boldedTextHelper string tail (end + 1)
                in bold :: rest

boldedText : String -> [(Int, Int)] -> Html
boldedText string boldRanges =
    -- Span to "concatenate" the pieces of the string
    span [] (boldedTextHelper string boldRanges 0)

boldedItems : ModuleMatch -> [Html]
boldedItems match =
    let boldedItem item =
        let relativeLink = match.moduleName ++ "#" ++ item.item
        -- block style needed to make entire blocks clickable
        in
            Html.Tags.a [ href relativeLink, style displayBlockStyle ]
                        [ boldedText item.item item.boldRanges ]
    in
        -- ui-menu-item is to tag something as clickable
        (map (\ match -> li [ style listItem2Style ] [boldedItem match])
             match.matches)

searchResult : String -> ModuleMatch -> [Html]
searchResult query match =
    let mainText =
            Html.Tags.a [ href match.moduleName, style displayBlockStyle ]
                        [ boldedText match.moduleName match.boldRanges ]
        main = li [ style listItemStyle] [ mainText ]
        rest = boldedItems match
    in
        main :: rest

searchResults : String -> [ModuleMatch] -> Html
searchResults query matches =
    ul [ style listStyle ]
       (matches |> map (searchResult query) |> concat)


stringInput : String -> Html
stringInput string =
    input
        [ placeholder "Text to reverse"
        , value string
        , on "input" getValue searchbox.handle identity
        , style myStyle
        ]
        []

displayBlockStyle : [CssProperty]
displayBlockStyle = [ prop "display" "block" ]

listStyle : [CssProperty]
listStyle =
    [ prop "position" "absolute",
      prop "background" "#ffffff",
      prop "z-index" "99999",
      prop "tabindex" "0",
      prop "top" "100px",
      prop "left" "20px",
      prop "width" "400px"
    ]

listItemStyle : [CssProperty]
listItemStyle =
    [ prop "display" "block",
      prop "padding" "4px 7px",
      prop "margin" "0",
      prop "border" "1px solid #181818",
      prop "border-top" "none",
      prop "color" "#222222",
      prop "font-family" "Helvetica",
      prop "letter-spacing" "0",
      prop "line-height" "100%",
      prop "cursor" "pointer"
      --prop "onclick" "location.href = 'google.com'"
    ]

listItem2Style : [CssProperty]
listItem2Style =
    [ prop "display" "block",
      prop "padding" "4px 25px",
      prop "margin" "0",
      prop "border" "1px solid #181818",
      prop "border-top" "none",
      prop "color" "#222222",
      prop "font-family" "monospace",
      prop "letter-spacing" "0",
      prop "line-height" "100%",
      prop "cursor" "pointer"
    ]

myStyle : [CssProperty]
myStyle =
    [ prop "width" "100%"
    , prop "height" "40px"
    , prop "padding" "10px 0"
    , prop "font-size" "2em"
    , prop "text-align" "center"
    ]

responseDecoder : Json.Decoder ModuleMatch
responseDecoder =
    let rangeDecoder = Json.map (\ range -> (head range, head (tail range)))
                                (Json.list Json.int)
        rangesDecoder = Json.list rangeDecoder
        matchDecoder = Json.object2
                            (\ name ranges -> { item = name, boldRanges = ranges })
                            ("name" := Json.string)
                            ("boldRanges" := rangesDecoder)
        matchesDecoder = Json.list matchDecoder
    in
        Json.object3
            (\ name matches boldRanges ->
                { moduleName = name, matches = matches, boldRanges = boldRanges })
            ("module_name" := Json.string)
            ("matches" := matchesDecoder)
            ("bold_ranges" := rangesDecoder)


-- State

type SearchState = { query: String, results: Maybe [ModuleMatch] }

initialState = { query = "", results = Nothing }

-- Update

parseResults : Http.Response String -> Maybe [ModuleMatch]
parseResults response =
    case response of
        Http.Success result -> Just (search "")
        Http.Waiting -> Just [{ moduleName = "Waiting", boldRanges = [], matches = [] }]
        Http.Failure _ _ -> Just [{ moduleName = "Failure", boldRanges = [], matches = [] }]

update : Event -> SearchState -> SearchState
update event state =
    case event of
        SearchBoxTyped text -> { state | query <- text }
        SearchResultArrived results -> { state | results <- parseResults results }

-- SIGNALS

main : Signal Element
main =
    let responses = Http.sendGet (searchUrl <~ searchbox.signal)
        events = merge (SearchBoxTyped <~ searchbox.signal)
                       (SearchResultArrived <~ responses)
    in
        scene <~ foldp update initialState events

searchbox : Input.Input String
searchbox = Input.input ""
