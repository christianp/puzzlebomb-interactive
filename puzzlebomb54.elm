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

type alias Box = Int

type alias Region = Set.Set (Int,Int)

type SudokuCell = SudokuFixed Int | SudokuFree (Maybe Int)
type ShikakuCell = ShikakuCell Bool Bool Bool Bool Bool -- shaded, joined n s e w
type LaserCell = Empty | NESW | NWSE
type Cell = Cell SudokuCell ShikakuCell LaserCell
type Laser = Laser Char Direction Int Direction Int Int -- label startside startposition endside endposition bounces
type Direction = N | S | E | W
type LaserTrailBit = NoTrail | Miss | Hit
type LaserTrail = LaserTrail LaserTrailBit LaserTrailBit LaserTrailBit LaserTrailBit -- n s e w
type alias LaserPath = {laser: Laser, path: List (Direction,Direction,(Int,Int)), end: (Direction,Int), bounces: Int}

type alias Model = 
    {
        grid: Grid.Grid Cell,
        currentTool : Tool,
        shikakuLast : Maybe (Int,Int)
    }

type Tool = SudokuTool | ShikakuTool | LaserTool

type Msg
    = CurrentTool Tool
    | SetValue Int Int String
    | ShikakuClick Int Int Direction
    | ChangeMirror Int Int

main = Html.beginnerProgram
    {
        model = initModel,
        update = update,
        view = view
    }

e = SudokuFree Nothing
f n = SudokuFixed n

initialSudoku = [
    f 1 , e, e, f 3, e, e, e, f 8, f 9,
    e, f 3, e, e, f 8, e, e, e, f 2,
    e, f 9, e, f 2, e, e, e, e, e,
    e, e, f 7, f 6, e, e, f 5, e, e,
    e, e, e, f 7, e, f 1, e, e, e,
    e, e, f 8, e, e, f 5, f 3, e, e,
    e, e, e, e, e, f 3, e, f 2, e,
    f 6, e, e, e, f 1, e, e, f 5, e,
    f 4, f 8, e, e, e, f 6, e, e, f 3
    ]

s = ShikakuCell True False False False False
n = ShikakuCell False False False False False
initialShikaku = [
    n, s, n, n, n, s, n, n, n,
    s, n, n, n, n, n, n, s, n,
    n, n, s, n, s, n, n, n, n,
    n, n, n, n, n, s, n, n, n,
    n, n, n, n, n, n, n, s, n,
    n, n, n, n, n, n, n, n, s,
    n, s, n, n, n, n, n, n, n,
    n, n, s, s, n, n, s, n, n,
    n, n, n, n, n, n, s, n, n
    ]

initialLasers = List.repeat 81 Empty

initialGrid = Grid.Grid 9 9 <| Array.fromList <| List.map3 Cell initialSudoku initialShikaku initialLasers

lasers = [
    Laser 'A' N 1 W 2 5,
    Laser 'B' N 3 W 7 3,
    Laser 'C' N 7 E 6 3,
    Laser 'D' E 3 S 7 1,
    Laser 'E' E 5 S 5 1,
    Laser 'F' S 1 S 0 2,
    Laser 'G' W 8 W 0 4,
    Laser 'H' S 3 W 1 1
    ]

initModel = {
    grid = initialGrid,
    currentTool = SudokuTool,
    shikakuLast = Nothing
    }

update msg model = case msg of
    CurrentTool tool -> {model|currentTool = tool}
    SetValue x y str -> case Grid.get x y model.grid of
        Nothing -> model
        Just (Cell (SudokuFixed _) _ _) -> model
        Just (Cell (SudokuFree _) sh la) -> 
            let
                dbg = Debug.log "set" (x,y,str)
                v = Result.withDefault Nothing (toInt str `Result.andThen` (Just >> Ok))
                ok = case v of
                    Nothing -> True
                    Just n -> n>=1 && n<=9
            in
                if ok then {model | grid = Grid.set x y (Cell (SudokuFree v) sh la) model.grid} else model
    ShikakuClick x y dir -> case (x,y,dir) of
        (0,_,W) -> model
        (8,_,E) -> model
        (_,0,N) -> model
        (_,8,S) -> model
        _ -> toggleWall (x,y) dir model |> toggleWall (adjacentCell x y dir) (opposite dir)
    ChangeMirror x y -> case Grid.get x y model.grid of
        Nothing -> model
        Just cell -> {model|grid = Grid.set x y (cycleMirror cell) model.grid}

toggleWall : (Int, Int) -> Direction -> Model -> Model
toggleWall (x,y) direction model = case Grid.get x y model.grid of
    Nothing -> model
    Just (Cell su (ShikakuCell shaded n s e w) l) ->
        let
            nshikaku = case direction of
                N -> ShikakuCell shaded (not n) s e w
                S -> ShikakuCell shaded n (not s) e w
                E -> ShikakuCell shaded n s (not e) w
                W -> ShikakuCell shaded n s e (not w)
            ncell = Cell su nshikaku l
        in
            {model|grid = Grid.set x y ncell model.grid}

cycleMirror (Cell su sh mirror) =
    let
        nmirror = case mirror of
            Empty -> NESW
            NESW -> NWSE
            NWSE -> Empty
    in
        Cell su sh nmirror

directionOf : Int -> Int -> Direction
directionOf dx dy = case (dx,dy) of 
    (1,0) -> E
    (-1,0) -> W
    (0,1) -> S
    (0,-1) -> N
    _ -> N

opposite: Direction -> Direction
opposite dir = case dir of
    N -> S
    S -> N
    E -> W
    W -> E

adjacentCell : Int -> Int -> Direction -> (Int,Int)
adjacentCell x y dir = case dir of
    N -> (x,y-1)
    S -> (x,y+1)
    E -> (x+1,y)
    W -> (x-1,y)

laserPath : Grid.Grid Cell -> Laser -> LaserPath
laserPath grid (Laser _ sdir spos edir epos bounces as laser) =
    let
        pos = case sdir of
            N -> (spos,0)
            S -> (spos,8)
            W -> (0,spos)
            E -> (8,spos)
        advance dir pos path = case pos of
            (-1,_) -> {path|end = (dir,snd pos)}
            (9,_) -> {path|end = (dir,snd pos)}
            (_,-1) -> {path|end = (dir,fst pos)}
            (_,9) -> {path|end = (dir,fst pos)}
            (x,y) -> case Grid.get x y grid of
                Nothing -> path
                Just (Cell _ _ mirror) -> 
                    let
                        ndir = case (mirror,dir) of
                            (Empty,d) -> d
                            (NESW,N) -> E
                            (NESW,S) -> W
                            (NESW,W) -> S
                            (NESW,E) -> N
                            (NWSE,N) -> W
                            (NWSE,S) -> E
                            (NWSE,W) -> N
                            (NWSE,E) -> S
                        npos = adjacentCell x y ndir
                        nbounces = if mirror==Empty then path.bounces else path.bounces+1
                    in
                        advance ndir npos {path|bounces=nbounces,path=(dir,ndir,pos)::path.path}
    in
        advance (opposite sdir) pos {laser=laser, bounces=0, end = (S,0), path= []}

laserPathGrid : Model -> List LaserPath -> Grid.Grid LaserTrail
laserPathGrid model paths = 
    let
        doPath path grid = 
            let
                ok = laserOk model.grid path.laser
                trail = if ok then Hit else Miss
            in
                List.foldl (doBit trail) grid path.path
        visitDir trail dir (LaserTrail n s e w) = case dir of
            N -> LaserTrail trail s e w
            S -> LaserTrail n trail e w
            E -> LaserTrail n s trail w
            W -> LaserTrail n s e trail
        doBit trail (indir,out,(x,y)) grid = case Grid.get x y grid of
            Nothing -> grid
            Just t -> Grid.set x y ((visitDir trail (opposite indir) >> visitDir trail out) t) grid
    in
        List.foldl doPath (Grid.grid 9 9 (LaserTrail NoTrail NoTrail NoTrail NoTrail)) paths

css = stylesheet """
    .sushikasers header h1, .sushikasers header h2 {
        display: inline-block;
    }
    .sushikasers h1 {
        text-transform: uppercase;
        font-size: 4rem;
        margin: 0;
        line-height: 1em;
    }
    .sushikasers .attribution {
        transform: rotate(90deg);
        width: 4.1rem;
        font-size: 0.5rem;
        text-align: right;
        vertical-align: top;
    }
    .controls {
        text-align: center;
    }

    .controls button {
        background: #f4f4f4;
        border: 2px solid #888;
        padding: 0.3em;
        font-size: 1.2rem;
        margin: 0.4rem 1rem;
    }

    .controls button.current-tool {
        background: #888;
        color: white;
    }

    .controls button:hover {
        background: hsl(120,50%,60%);
        color: white;
    }

    .grid {
        font-family: sans-serif;
        margin-left: auto;
        margin-right: auto;
        border-collapse: collapse;
    }
    .grid .cell {
        border: 2px solid #ddd;
        text-align: center;
        position: relative;
    }
    .grid .cell.fixed .value {
        font-weight: bold;
        font-family: monospace;
    }

    .grid .cell.shaded, .grid .cell.shaded input {
        background: #eee;
    }
    .grid .cell.shaded input:focus {
        background: white;
    }

    .sushikasers.sudoku .grid .cell.row-invalid, .sushikasers.sudoku .grid .cell.row-invalid input,
    .sushikasers.sudoku .grid .cell.col-invalid, .sushikasers.sudoku .grid .cell.col-invalid input,
    .sushikasers.sudoku .grid .cell.block-invalid, .sushikasers.sudoku .grid .cell.block-invalid input,
    .sushikasers.shikaku .grid .cell.region-not-rectangle,
    .sushikasers.shikaku .grid .cell.region-invalid,
    .sushikasers.lasers .grid .cell.region-too-many-mirrors
    {
        background: hsl(0,100%,70%);
        color: white;
    }
    .sushikasers.shikaku .grid .cell.region-correct,
    .sushikasers.lasers .grid .cell.region-has-mirror
    {
        background: hsl(120,50%,95%);
    }
    .grid td, .grid th {
        width: 2.1rem;
        height: 2.1rem;
    }
    .grid .cell input {
        width: 2rem;
        height: 1.9rem;
        font-size: 1rem;
        border: none;
        text-align: center;
        color: black;
    }
    .grid .cell .value, .grid .cell input {
        font-size: 1.5em;
    }
    .grid .cell input:focus {
        background: #eee;
    }

    .grid tr:nth-child(2) .cell {
        border-top: 4px solid black !important;
    }

    .grid tr:nth-child(10) .cell {
        border-bottom: 4px solid black !important;
    }

    .grid .cell:nth-of-type(1) {
        border-left: 4px solid black !important;
    }

    .grid .cell:nth-of-type(9) {
        border-right: 4px solid black !important;
    }

    .sushikasers.sudoku td:nth-of-type(3n+3):not(:nth-child(10)) {
        border-right: 2px solid black;
    }
    .sushikasers.sudoku tr:nth-child(3n+4):not(:nth-child(10)) td {
        border-bottom: 2px solid black;
    }

    .sushikasers.shikaku .grid .cell.box-left, .sushikasers.lasers .grid .cell.box-left {
        border-left: 2px solid black;
    }

    .sushikasers.shikaku .grid .cell.box-right, .sushikasers.lasers .grid .cell.box-right {
        border-right: 2px solid black;
    }

    .sushikasers.shikaku .grid .cell.box-top, .sushikasers.lasers .grid .cell.box-top {
        border-top: 2px solid black;
    }

    .sushikasers.shikaku .grid .cell.box-bottom, .sushikasers.lasers .grid .cell.box-bottom {
        border-bottom: 2px solid black;
    }

    .laser {
        padding: 0.3em;
        text-align: center;
    }

    .laser-end {
        background: #eee;
        padding: 0.2em 0.4em;
    }

    .laser-start {
        background: #eee;
        padding: 0.2em 0.5em;
        border-radius: 50%;
    }

    .laser-ok .laser-start, .laser-ok .laser-end {
        background: hsl(120,50%,75%);
    }

    .laser-trail, .mirror {
        width: 100%;
        height: 100%;
        position: absolute;
        left: 0;
        top: 0;
    }

    .laser-trail .laser-hit {
        fill: hsla(120,100%,50%,0.7);
    }
    .laser-trail .laser-miss {
        fill: hsla(0,100%,50%,0.7);
    }

    .mirror-nesw, .mirror-nwse {
        fill: black;
    }
    .mirror-nesw {
        transform: translate(0.5px,0.5px) rotate(45deg);
    }
    .mirror-nwse {
        transform: translate(0.5px,0.5px) rotate(-45deg);
    }

    """

explanation = div [class "explanation"] [Markdown.toHtml [] """
This puzzle is actually three puzzles.

First you must solve a Sudoku puzzle using the numbers - filling the grid so each row, column and 3×3 box contains the numbers 1-9 once each.

Then use the numbers in shaded cells to solve a Shikaku puzzle - partition the grid into square or rectangular boxes, each containing one number which is the area of that box.

Finally you must use the outlines of boxes from the Shikaku puzzle and the outside labels to place a 45° diagonal mirror in one square of each box, so that the lasers emitted by the lettered rectangles into the grid bounce off the number of mirrors given before reaching their targets. Both sides of the mirrors are reflective.
"""]

view model = Puzzlebomb.puzzlebomb "june 2016" 54 <|
    div [classList [
        ("sushikasers",True),
        ("sudoku",model.currentTool == SudokuTool),
        ("shikaku",model.currentTool == ShikakuTool),
        ("lasers",model.currentTool == LaserTool)
    ]] [
        css,
        viewGrid model (findRegions model.grid) (laserPathGrid model (List.map (laserPath model.grid) lasers)),
        div [class "controls"] [
            button [onClick (CurrentTool SudokuTool), classList [("current-tool",model.currentTool==SudokuTool)]] [text "Sudoku"],
            button [onClick (CurrentTool ShikakuTool), classList [("current-tool",model.currentTool==ShikakuTool)]] [text "Shikaku"],
            button [onClick (CurrentTool LaserTool), classList [("current-tool",model.currentTool==LaserTool)]] [text "Lasers"]
            ],
        header [] [
            h1 [] [text "Sushikasers"],
            h2 [class "attribution"] [attribution]
        ],
        explanation
        ]

attribution = Markdown.toHtml [] """
SUSHIKASERS:  
by [@stecks](http://twitter.com/stecks)  
& [@gheizhwinder](http://twitter.com/gheizhwinder)
"""

viewRegion : Region -> String
viewRegion positions = String.join "," <| List.map toString (Set.toList positions)

viewGrid model regions laserGrid = 
    let
        topLaserRow = (th [] [])::(List.map (viewLaserEnd model.grid N) [0..8])++[th [] []]
        rowCells y = List.map (\x -> viewMaybeCell x y model regions laserGrid) [0..8]
        gridRows = List.map (\y -> tr [] ((viewLaserEnd model.grid W y)::((rowCells y)++[viewLaserEnd model.grid E y]))) [0..8]
        bottomLaserRow = (th [] [])::(List.map (viewLaserEnd model.grid S) [0..8])++[th [] []]
    in
        table [class "grid"] ((tr [] topLaserRow)::(gridRows++[tr [] bottomLaserRow]))

viewMaybeCell : Int -> Int -> Model -> List Region -> Grid.Grid LaserTrail -> Html Msg
viewMaybeCell x y model regions laserGrid= 
    let
        mcell = Grid.get x y model.grid
    in
        case mcell of
            Nothing -> text ""
            Just cell -> viewCell x y model regions laserGrid cell 

viewCell : Int -> Int -> Model -> List Region -> Grid.Grid LaserTrail -> Cell -> Html Msg
viewCell x y model regions laserGrid (Cell sudoku (ShikakuCell shaded n s e w) lasercell as cell) =
    let
        val = case sudokuValue cell of
            Nothing -> ""
            Just n -> toString n
        showSudokuValue = case model.currentTool of 
            SudokuTool -> True
            ShikakuTool -> shaded
            LaserTool -> False
        fixed = case sudoku of
            SudokuFixed _ -> True
            _ -> False
        sudokuView = if showSudokuValue then (if fixed then span [class "value"] [text val] else input [onInput <| SetValue x y, value val] []) else text ""
        trail = Maybe.withDefault (LaserTrail NoTrail NoTrail NoTrail NoTrail) (Grid.get x y laserGrid)
        content = case model.currentTool of
            SudokuTool -> [sudokuView]
            ShikakuTool -> if shaded then [text val] else []
            LaserTool -> [viewLaserTrail trail,viewMirror lasercell]
        region = Maybe.withDefault (Set.singleton (x,y)) (regionOf (x,y) regions)
        numMirrors = regionMirrors region model.grid
        classes = [
            ("cell",True),
            ("fixed",fixed),
            ("shaded",shaded),
            ("box-top", not n),
            ("box-bottom",not s),
            ("box-right",not e),
            ("box-left",not w),
            ("row-invalid",not <| rowValid y model),
            ("col-invalid",not <| colValid x model),
            ("block-invalid",not <| blockValid (blockOf x y) model),
            ("region-not-rectangle",not <| isRectangle region),
            ("region-invalid",regionInvalid region model.grid),
            ("region-correct",regionValid region model.grid),
            ("region-has-mirror",numMirrors==1),
            ("region-too-many-mirrors",numMirrors>1)
        ]
        block = blockOf x y
        extraAttrs = case model.currentTool of
            SudokuTool -> []
            ShikakuTool -> [on "click" (clickQuadrant <| ShikakuClick x y)]
            LaserTool -> [onClick (ChangeMirror x y)]
    in
        td ([classList classes]++extraAttrs) content

blockOf x y = (y//3)*3+(x//3)

indexToPos i =
    let
        x = i%9
        y = (i-x)//9
    in
        (x,y)

getBlock block (Grid.Grid _ _ items) = List.map snd (List.filter (\(i,c) -> indexToPos i |> \(x,y) -> blockOf x y==block) (Array.toIndexedList items))

sudokuValue : Cell -> Maybe Int
sudokuValue (Cell sudoku _ _) = case sudoku of
    SudokuFixed n -> Just n
    SudokuFree v -> v

justList : List (Maybe a) -> List a
justList maybes = case maybes of
    [] -> []
    Nothing::rest -> justList rest
    (Just n)::rest -> n::(justList rest)

rowValid row model =
    let
        cells = (justList << Array.toList << Array.map sudokuValue << Grid.getRow row) model.grid
    in
        Set.size (Set.fromList cells) == List.length cells
colValid col model =
    let
        cells = (justList << Array.toList << Array.map sudokuValue << Grid.getColumn col) model.grid
    in
        Set.size (Set.fromList cells) == List.length cells

blockValid block model =
    let
        cells = (justList << List.map sudokuValue << getBlock block) model.grid
    in
        Set.size (Set.fromList cells) == List.length cells

clickQuadrant msg = Json.map msg (Json.object4 findQuadrant ("offsetX" := Json.float) ("offsetY" := Json.float) ("target" := ("clientWidth" := Json.float)) ("target" := ("clientHeight" := Json.float)))

findQuadrant x y w h =
  let
    d1 = w*y > x*h
    d2 = w*(h-y) > x*h
  in
    case (d1,d2) of
      (True,True) -> W
      (False,False) -> E
      (True,False) -> S
      (False,True) -> N

shikakuOpen (Cell _ (ShikakuCell shaded n s e w) l) dir = case dir of
    N -> n
    S -> s
    E -> e
    W -> w

shikakuShaded (Cell _ (ShikakuCell shaded _ _ _ _) l) = shaded

regionInvalid : Region -> Grid.Grid Cell -> Bool
regionInvalid region grid =
    let
        shadedPositions = Set.filter (\(x,y) -> Maybe.withDefault False <| Maybe.map (\c -> (shikakuShaded c) && ((sudokuValue c)/=Nothing)) (Grid.get x y grid)) region
        value = List.head (Set.toList shadedPositions) `Maybe.andThen` \(x,y) -> Grid.get x y grid `Maybe.andThen` sudokuValue
        numShaded = Set.size shadedPositions
    in
        numShaded>1 || (numShaded==1 && ((Just (Set.size region))/=value))

regionValid : Region -> Grid.Grid Cell -> Bool
regionValid region grid =
    let
        shadedPositions = Set.filter (\(x,y) -> Maybe.withDefault False <| Maybe.map (\c -> (shikakuShaded c) && ((sudokuValue c)/=Nothing)) (Grid.get x y grid)) region
        value = List.head (Set.toList shadedPositions) `Maybe.andThen` \(x,y) -> Grid.get x y grid `Maybe.andThen` sudokuValue
    in
        (isRectangle region) && (Set.size shadedPositions)==1 && ((Just (Set.size region))==value)

isRectangle : Region -> Bool
isRectangle region =
    let
        list = Set.toList region
        xs = List.map fst list
        ys = List.map snd list
        mleft = List.minimum xs
        mright = List.maximum xs
        mtop = List.minimum ys
        mbottom = List.maximum ys
    in
        case (mleft,mright,mtop,mbottom) of
            (Just left, Just right, Just top, Just bottom) -> (Set.size region) == (1+right-left)*(1+bottom-top)
            _ -> True

regionOf pos regions = List.head <| List.filter (Set.member pos) regions

regionMirrors region grid = 
    let
        hasMirror (Cell _ _ lasercell) = lasercell /= Empty
    in
        Set.size <| Set.filter (\(x,y) -> Maybe.withDefault False <| Maybe.map hasMirror (Grid.get x y grid)) region

findRegions : Grid.Grid Cell -> List Region
findRegions (Grid.Grid w h items as grid) =
    let
        initRegions = List.map Set.singleton (Grid.positions grid)
        mergeRegions regions a b = 
            let
                toMerge = List.filter (\r -> (Set.member a r) || (Set.member b r)) regions
                merged = List.foldl (Set.union) Set.empty toMerge
            in
                merged::(List.filter (\r -> not (List.member r toMerge)) regions)
        considerCellDir x y dir regions = 
            let
                (nx,ny) = adjacentCell x y dir
            in
                case (Grid.get x y grid, Grid.get nx ny grid) of
                    (Nothing,_) -> regions
                    (_,Nothing) -> regions
                    (Just cell, Just _) -> 
                        if shikakuOpen cell dir then 
                            mergeRegions regions (x,y) (nx,ny)
                        else
                            regions
        considerCell (x,y) regions = 
            let
                consider = considerCellDir x y
            in
                (consider N >> consider S >> consider E >> consider W) regions
    in
        List.foldl considerCell initRegions (Grid.positions grid)

laserOk grid (Laser _ _ _ edir epos bounces as laser) =
    let
        path = laserPath grid laser
    in
        path.bounces == bounces && (edir,epos)==path.end

viewLaserEnd : Grid.Grid Cell -> Direction -> Int -> Html Msg
viewLaserEnd grid dir pos =
    let
        start = List.head <| List.filter (\(Laser _ sdir spos _ _ _) -> dir==sdir && pos==spos) lasers
        end = List.head <| List.filter (\(Laser _ _ _ edir epos _) -> dir==edir && pos==epos) lasers
        maybeLaserOk maybeLaser = Maybe.withDefault False (Maybe.map (laserOk grid) maybeLaser)
        startOk = maybeLaserOk start
        endOk = maybeLaserOk end
    in
        th [classList [("laser",True),("laser-ok",startOk || endOk)]] [case (start,end) of
            (Nothing,Nothing) -> text ""
            (Just (Laser char _ _ _ _ bounces),_) -> span [class "laser-end"] [text <| String.cons char (toString bounces)]
            (_,Just (Laser char _ _ _ _ bounces)) -> span [class "laser-start"] [text <| String.fromChar char]
        ]

viewLaserTrail (LaserTrail n s e w) = 
    let
        tclass t = case t of
            NoTrail -> ""
            Hit -> "laser-hit"
            Miss -> "laser-miss"
        nr = Svg.rect [Svga.class (tclass n), Svga.x "0.4", Svga.y "0", Svga.width "0.2", Svga.height "0.5"] []
        sr = Svg.rect [Svga.class (tclass s), Svga.x "0.4", Svga.y "0.5", Svga.width "0.2", Svga.height "0.5"] []
        er = Svg.rect [Svga.class (tclass e), Svga.x "0.5", Svga.y "0.4", Svga.width "0.5", Svga.height "0.2"] []
        wr = Svg.rect [Svga.class (tclass w), Svga.x "0", Svga.y "0.4", Svga.width "0.5", Svga.height "0.2"] []
        rects = List.map snd <| List.filter (\(t,_) -> t/=NoTrail) [(n,nr),(s,sr),(e,er),(w,wr)]
    in
        span [class "laser-trail"] [Svg.svg [viewBox "0 0 1 1"] rects]

viewMirror mirror = 
    let
        attrs = [Svga.x "-0.05", Svga.y "-0.25", Svga.width "0.1", Svga.height "0.5"]
        rect = case mirror of
            Empty -> []
            NESW -> [Svg.rect (attrs++[Svga.class "mirror-nesw"]) []]
            NWSE -> [Svg.rect (attrs++[Svga.class "mirror-nwse"]) []]
    in
        span [class "mirror"] [Svg.svg [viewBox "0 0 1 1"] rect]
