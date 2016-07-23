module Puzzlebomb exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Css exposing (stylesheet)
import Markdown

css = stylesheet """
    html {
        font-size: 20px;
    }
    body {
        width: 600px;
        margin: 0 auto;
        font-family: "Apple LiGothic", "Century Gothic", sans-serif;
    }
    .puzzlebomb-logo {
        max-width: 100%;
    }
    header.puzzlebomb-header {
        position: relative;
        border-bottom: 1px solid;
        margin-top: 1em;
        margin-bottom: 1em;
        padding-bottom: 0.5em;
    }
    header.puzzlebomb-header a {
        color: black;
        text-decoration: none;
    }
    header.puzzlebomb-header .month, header .issue {
        font-size: 1rem;
        position: absolute;
    }
    header.puzzlebomb-header .issue {
        left: 0;
    }
    header.puzzlebomb-header .month {
        right: 0;
    }
    .explanation {
        font-weight: bold;
        font-size: 0.75em;
    }
    footer {
        margin-top: 1em;
        border-top: 1px solid;
        font-weight: bold;
        font-size: 0.75em;
        text-align: right;
    }
    """
    

header : String -> Int -> Html msg
header issue month = 
    Html.header [class "puzzlebomb-header"] [ a [href "http://puzzlebomb.co.uk"] [
        h2 [class "issue"] [text issue],
        h2 [class "month"] [text <| "issue "++(toString month)],
        img [class "puzzlebomb-logo", src "../puzzlebomb-logo.png"] []
    ]]

footer : Html msg
footer = Html.footer [] [
        Markdown.toHtml [] """
[PUZZLEBOMB.co.uk](http://puzzlebomb.co.uk): assembled by [@stecks](http://twitter.com/stecks).

Interactive version by [clp](http://somethingorotherwhatever.com).
        """
    ]

puzzlebomb : String -> Int -> Html msg -> Html msg
puzzlebomb issue month puzzle = 
    div [class "puzzlebomb"] [
        css,
        header issue month,
        puzzle,
        footer
    ]
