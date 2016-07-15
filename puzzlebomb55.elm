import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Css exposing (stylesheet)

import Puzzlebomb
import Grid exposing (Grid,grid)
import Array exposing (Array)
import String exposing (toInt)
import Set
import Dict

type Cell = Cell (Maybe Int) Bool

type Box = Box Int (List (Int,Int))

type alias Model = 
    {
        grid : Grid.Grid Cell
    }

type Msg
    = SetValue Int Int String

main = Html.beginnerProgram
    {
        model = initModel,
        update = update,
        view = view
    }

empty = Cell Nothing False
i n = Cell (Just n) True
initialGrid = Grid.Grid 7 5 <| Array.fromList [
    empty, empty, empty, empty, empty, empty, i 15,
    empty, i 24, empty, i 28, empty, i 32, empty,
    i 9, empty, i 1, i 12, empty, empty, empty,
    empty, empty, i 30, empty, empty, i 20, empty,
    i 17, empty, empty, empty, i 33, empty, empty
    ]

boxes = 
    [
        Box 24 [(0,0),(0,1)],
        Box 62 [(1,0),(1,1),(1,2)],
        Box 16 [(2,0),(3,0),(4,0)],
        Box 87 [(5,0),(6,0),(5,1),(6,1)],
        Box 70 [(2,1),(3,1),(4,1)],
        Box 19 [(0,2),(0,3)],
        Box 42 [(2,2),(1,3),(2,3)],
        Box 63 [(3,2),(3,3),(3,4)],
        Box 86 [(4,2),(4,3),(4,4)],
        Box 52 [(5,2),(6,2)],
        Box 26 [(5,3),(5,4)],
        Box 50 [(6,3),(6,4)],
        Box 33 [(0,4),(1,4),(2,4)]
    ]

boxOfDict = 
    let
        pairs (Box _ coords as box) = List.map (\coord -> (coord,box)) coords
    in
        Dict.fromList <| List.concatMap pairs boxes

boxOf x y = Maybe.withDefault (Box 0 []) (Dict.get (x,y) boxOfDict)

initModel = {
    grid = initialGrid
    }

update msg model = case msg of
    SetValue x y str -> case Grid.get x y model.grid of
        Nothing -> model
        Just (Cell _ True) -> model
        Just (Cell _ False) -> 
            let
                v = Result.withDefault Nothing (toInt str `Result.andThen` (Just >> Ok))
                ok = case v of
                    Nothing -> True
                    Just n -> n>=1 && n<=35
            in
                if ok then {model | grid = Grid.set x y (Cell v False) model.grid} else model

css = stylesheet """
    .modoku {
        margin-top: 1em;
        position: relative;
        padding-right: 3em;
    }
    .modoku h1 {
        text-transform: uppercase;
        transform: rotate(90deg);
        position: absolute;
        right: -2.1em;
        top: 1.7em;
        margin: 0;
        font-size: 4.6em;
    }
    .grid {
        font-family: sans-serif;
    }
    .grid .cell {
        border: 1px solid #ddd;
        padding: 0.5em;
        width: 3em;
        height: 3em;
        text-align: center;
        position: relative;
    }
    .grid .cell .target {
        position: absolute;
        left: 0.2em;
        top: 0.2em;
        font-size: 0.7em;
        pointer-events: none;
        font-weight: bold;
    }
    .grid .cell.fixed .value {
        font-weight: bold;
        font-family: monospace;
    }
    .grid .cell.box-complete, .grid .cell.box-complete input {
        background: hsl(120,50%,95%);
    }
    .grid .cell.row-invalid,.grid .cell.row-invalid input,
    .grid .cell.col-invalid,.grid .cell.col-invalid input,
    .grid .cell.duplicate, .grid .cell.duplicate input,
    .grid .cell.box-incorrect, .grid .cell.box-incorrect input
    {
        background: red;
        color: white;
    }
    .grid .cell.box-incorrect .target {
        font-size: 0.8em;
        color: yellow;
        animation: wobble 0.3s infinite;
    }

    @keyframes wobble {
        0% {
            transform: rotate(0deg);
        }
        25% {
            transform: rotate(30deg);
        }
        75% {
            transform: rotate(-30deg);
        }
    }

    .grid .cell input {
        width: 2rem;
        height: 2rem;
        font-size: 1rem;
        border: none;
        text-align: center;
    }
    .grid .cell .value, .grid .cell input {
        font-size: 1.5em;
    }
    .grid .cell input:focus {
        background: #eee;
    }

    .grid .cell.box-left {
        border-left: 2px dashed black;
    }

    .grid .cell.box-right {
        border-right: 2px dashed black;
    }

    .grid .cell.box-top {
        border-top: 2px dashed black;
    }

    .grid .cell.box-bottom {
        border-bottom: 2px dashed black;
    }

    """

view model = Puzzlebomb.puzzlebomb "july 2016" 55 <|
    div [class "modoku"] [
        css,
        h1 [] [text "Modoku"],
        viewGrid model,
        div [class "explanation"] [
            p [] [text "Fill the grid with the numbers 1-35, using the clues given, so that each row contains 7 numbers each with a different remainder when you divide by 7, and each column contains 5 numbers each with a different remainder when you divide by 5."],
            p [] [text "The numbers in each of the dotted regions add up to the indicated total."]
            ]
        ]

cartesianProduct a b = List.concatMap (\x -> List.map (,) b) a

viewGrid model = table [class "grid"] <| List.map (\y -> tr [] (List.map (\x -> viewCell x y model) [0..6])) [0..4]

viewCell : Int -> Int -> Model -> Html Msg
viewCell x y model = Maybe.withDefault (Cell Nothing False) (Grid.get x y model.grid) |> (\(Cell val fixed as cell) ->
    let
        myBox = boxOf x y
        myBoxSum = boxSum myBox model
        valueString = Maybe.withDefault "" (Maybe.map toString val)
        isBoxFirst = Just (x,y) == List.head (boxCoords myBox)
        isUnique = case val of
            Nothing -> True
            Just n -> (Array.length <| Array.filter (\(Cell v _) -> v == Just n) (Grid.cells model.grid)) == 1
        content = 
            if fixed then span [class "value"] [text valueString] else
                input [maxlength 2, onInput <| SetValue x y] []
        classes = [
            ("cell",True),
            ("fixed",fixed),
            ("row-invalid",not <| rowValid y model),
            ("col-invalid",not <| columnValid x model),
            ("box-left",not <| (boxOf (x-1) y)==myBox),
            ("box-right",not <| (boxOf (x+1) y)==myBox),
            ("box-top",not <| (boxOf x (y-1))==myBox),
            ("box-bottom",not <| (boxOf x (y+1))==myBox),
            ("box-complete",myBoxSum == boxTarget myBox),
            ("box-incorrect",(myBoxSum > boxTarget myBox) || (myBoxSum < boxTarget myBox && boxComplete myBox model)),
            ("duplicate",not isUnique)
        ]
        targetSpan = span [class "target"] [text <| toString <| boxTarget myBox]
    in
        td [classList classes] <| [content]++(if isBoxFirst then [targetSpan] else [])
    )

rowValid row model =
    let
        cells = Array.filter (\(Cell value _) -> value /= Nothing) <| Grid.getRow row model.grid
        values =  Array.map ((\(Cell v _) -> Maybe.withDefault -1 v) >> (\v -> v%7)) cells
        uniques = (Array.toList >> Set.fromList) values
    in
        Set.size uniques == Array.length values

columnValid row model =
    let
        cells = Array.filter (\(Cell value _) -> value /= Nothing) <| Grid.getColumn row model.grid
        values =  Array.map ((\(Cell v _) -> Maybe.withDefault -1 v) >> (\v -> v%5)) cells
        uniques = (Array.toList >> Set.fromList) values
    in
        Set.size uniques == Array.length values

justList maybes = case maybes of
    [] -> []
    Nothing::rest -> justList rest
    (Just n)::rest -> n::(justList rest)

boxSum (Box target coords) model =
    let
        cells = justList <| List.map (\(x,y) -> Grid.get x y model.grid `Maybe.andThen` (\(Cell v _) -> v)) coords
    in
        List.sum cells

boxTarget (Box target _) = target
boxCoords (Box _ coords) = coords
boxComplete : Box -> Model -> Bool
boxComplete (Box _ coords) model = List.all ((/=) Nothing) <| List.map (\(x,y) -> (Grid.get x y model.grid) `Maybe.andThen` (\(Cell val _) -> val)) coords
