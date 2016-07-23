import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Css exposing (stylesheet)
import Svg
import Svg.Attributes as Svga exposing (viewBox)

import Puzzlebomb
import Grid exposing (Grid,grid)
import Array exposing (Array)
import String exposing (toInt)
import Set
import Dict
import Markdown
import Json.Decode as Json exposing ((:=))

type alias Model = 
    {
    }

type Msg
    = Msg

main = Html.beginnerProgram
    {
        model = initModel,
        update = update,
        view = view
    }

initModel = {
    }

update msg model = case msg of
    Msg -> model

css = stylesheet """

    """

explanation = div [class "explanation"] [Markdown.toHtml [] """

"""]

view model = Puzzlebomb.puzzlebomb "june 2016" 54 <|
    div [] [
        css,
        explanation
        ]
