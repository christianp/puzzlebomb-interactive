module Grid exposing (..)
import Array exposing (Array)

type Grid t = Grid Int Int (Array t)

grid : Int -> Int -> a -> Grid a
grid width height initial = Grid width height (Array.repeat (width*height) initial)

gridpos x y (Grid width height _) = y*width+x

get : Int -> Int -> Grid a -> Maybe a
get x y (Grid width height items as grid) = Array.get (gridpos x y grid) items

set : Int -> Int -> a -> Grid a -> Grid a
set x y a (Grid width height items as grid) = Grid width height (Array.set (gridpos x y grid) a items)

map : (a -> b) -> Grid a -> Grid b
map fn (Grid width height items) = Grid width height (Array.map fn items)

getRow : Int -> Grid a -> Array a
getRow row (Grid width height items) = Array.slice (width*row) (width*(row+1)) items

getColumn : Int -> Grid a -> Array a
getColumn col (Grid width height items) = Array.fromList <| List.map snd <| List.filter (\(i,v) -> (i%width==col)) (Array.toIndexedList items)

cells : Grid a -> Array a
cells (Grid _ _ items) = items

positions : Grid a -> List (Int,Int)
positions (Grid w h _) = List.concatMap (\x -> List.map (\y -> (x,y)) [0..(h-1)]) [0..(w-1)]
