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

type Rung = Rung String String  -- Rung clue answer

type Rule = NoRule | Vowels | Consonants

type alias Ladder = Array Rung

type alias Model = 
    {
        ladders: Array Ladder
    }

type Msg
    = UpdateLadder Int LadderMsg

type LadderMsg
    = SetAnswer Int String

main = Html.beginnerProgram
    {
        model = initModel,
        update = update,
        view = view
    }

rung clue = Rung clue ""
ladder clues = Array.fromList <| List.map rung clues
ladder1 = ladder [
    "played first",
    "bully",
    "around",
    "ethnic",
    "ado",
    "circuits",
    "vessels"
    ]
ladder2 = ladder [
    "nightclothes",
    "prize",
    "buzzing",
    "pave",
    "be situated beyond",
    "Canadian police",
    "facilities"
    ]
ladder3 = ladder [
    "confirm",
    "picturesque",
    "deduction of facts",
    "dropper",
    "hunger",
    "sprite (old spelling)",
    "hairstyle"
    ]

initModel = {
    ladders = Array.fromList [ladder1,ladder2,ladder3]
    }

update msg model = case msg of
    UpdateLadder n lmsg -> case Array.get n model.ladders of
        Nothing -> model
        Just ladder -> {model|ladders=Array.set n (updateLadder lmsg ladder) model.ladders}

updateLadder msg ladder = case msg of
    SetAnswer n answer -> case Array.get n ladder of
        Nothing -> ladder
        Just (Rung clue _) -> Array.set n (Rung clue (String.toLower answer)) ladder

css = stylesheet """
.concordant-avowal h1 {
    text-transform: uppercase;
    margin: 0;
    font-size: 4rem;
    line-height: 0.9em;
}
.concordant-avowal header {
    position: relative;
}

.concordant-avowal header h2 {
    position: absolute;
    bottom: 0;
    right: 0;
    font-size: 0.7rem;
    width: 10em;
    text-align: right;
    margin: 0;
    line-height: 1em;
}

.attribution p {
    margin: 0;
}
.ladder {
    list-style: none;
    padding: 0;
    margin: 0 0.5em;
}

.ladders {
    display: flex;
    padding: 0;
}

.rung input {
    width: 6em;
    margin-left: 1em;
    height: 1em;
    margin-bottom: 1em;
    border: none;
    border-bottom: 1px solid black;
}

.rung {
    text-align: right;
    font-size: 0.7rem;
    display: flex;
    justify-content: flex-end;
    padding: 0.5em 0;
}
.rung.incorrect, .rung.incorrect input {
    background: hsl(0,100%,50%);
    color: white;
}
.rung.incorrect input {
    border-bottom-color: white;
}
.rung.correct, .rung.correct input {
    background: hsl(120,70%,70%);
}

.rung .clue {
    font-weight: bold;
}
    """

explanation = div [class "explanation"] [Markdown.toHtml [] """
The single-word answers to each set of clues form a 'ladder', in which each rung shares the same vowels or consonants, alternately, as the next word.
"""]

attribution = div [class "attribution"] [Markdown.toHtml [] """
CONCORDANT AVOWAL:  
by [@stecks](http://twitter.com/stecks) and [@apaultaylor](http://twitter.com/apaultaylor)
"""]

view model = Puzzlebomb.puzzlebomb "march 2016" 51 <|
    div [class "concordant-avowal"] [
        css,
        header [] [
            h1 [] [text "Concordant Avowal"],
            h2 [] [attribution]
        ],
        explanation,
        ul [class "ladders"] <| Array.toList <| Array.indexedMap (viewLadder model) model.ladders
        ]

viewLadder : Model -> Int -> Ladder -> Html Msg
viewLadder model n ladder = 
    let
        results = ladderAlternate ladder
    in
        ul [class "ladder"] (List.indexedMap (\k -> \rung -> Html.map (UpdateLadder n) (viewRung k rung)) results)

vowels = String.toList "aeiou"
consonants = String.toList "bcdfghjklmnpqrstvwxyz"
lettersOf letters word = Set.fromList <| String.toList <| String.filter (\c -> List.member c letters) word
sameLetters letters word1 word2 = (lettersOf letters word1) == (lettersOf letters word2)

ladderAlternate ladder = 
    let
        handle (Rung clue answer as rung) ((Rung _ last,_,rule),seq) = 
            let
                this = case rule of
                    NoRule -> 
                        let
                            sameVowels = sameLetters vowels last answer
                            sameConsonants = sameLetters consonants last answer
                            result = if last=="" || sameVowels || sameConsonants then Ok else Err
                            (rule',nrule) = if last=="" then (NoRule,NoRule) else if sameVowels && sameConsonants then (NoRule,NoRule) else if sameVowels then (Vowels,Consonants) else if sameConsonants then (Consonants,Vowels) else (NoRule,NoRule)
                        in
                            (rung,result rule',nrule)
                    Vowels -> if answer=="" then (rung,Err Vowels,NoRule) else (rung,if sameLetters vowels last answer then Ok Vowels else Err Vowels, Consonants)
                    Consonants -> if answer=="" then (rung,Err Consonants,NoRule) else (rung,if sameLetters consonants last answer then Ok Consonants else Err Consonants, Vowels)
            in
                (this,this::seq)
        (_,results) = Array.foldl handle ((Rung "" "",Ok NoRule,NoRule),[]) ladder
    in
        List.reverse results

viewRung n (Rung clue answer, result, nrule) = 
    let
        (rule,incorrect) = case result of
            Err r -> (r,True)
            Ok r -> (r,False)
        classes = [
            ("rung",True),
            ("incorrect",incorrect),
            ("correct",answer /= "" && not incorrect),
            ("vowels",rule==Vowels),
            ("consonants",rule==Consonants)
        ]
    in
        li [classList classes] [
            span [class "clue"] [text clue],
            input [onInput (SetAnswer n)] []
        ]
