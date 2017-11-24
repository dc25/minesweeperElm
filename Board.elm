module Board exposing(Cell, Board, w, h, exposeCells, gameOver)

import List exposing (map2, length, range, concatMap)
import List.Extra exposing (andThen)
import Dict exposing (Dict, fromList, insert, get, isEmpty)
import Maybe exposing (withDefault)
import Random.Pcg as R exposing (Generator, float, list)

import Pos exposing (..)
import Msg exposing (..)

type alias Cell = 
    { mined : Bool 
    , exposed : Bool
    , flagged : Bool
    , mineCount : Int
    } 

type alias Board = Dict Pos Cell

w : Int
w = 32

h : Int
h = 160

generateCell : Generator Cell
generateCell = R.map (\t -> Cell (t < 0.201) False False 0) (float 0.0 1.0)

generateBoard : Generator Board
generateBoard = 
    let indices = andThen (\y -> andThen (\x -> [(x, y)]) 
                              (range 0 (w-1)) ) (range 0 (h-1)) 
    in  R.map (\cs -> fromList (map2 (,) indices cs))
            (list (length indices) generateCell)

adjacents : Pos -> List Pos
adjacents (x,y) = 
    let patch = range (x-1) (x+1) |> concatMap (\xx -> 
                  range (y-1) (y+1) |> List.map (\yy -> (xx,yy)))

    in List.filter (\(xx,yy) ->    (xx,yy) /= (x,y) 
                                && xx >= 0 && yy >= 0 
                                && xx < w && yy < h ) patch
              
exposeCells : Pos -> Board -> Board
exposeCells pos board =
    let getCell board pos = withDefault (Cell False False False 0) (get pos board)
        c = getCell board pos 
        indices = adjacents pos
        count = length (List.filter (\{mined} -> mined) (List.map (getCell board) indices))
        {mined,exposed,flagged} = c
        checklist = if mined || exposed || flagged || count /= 0 then [] else indices
        exposedSelection = (insert pos ({c|exposed = True, mineCount = count}) board)
        exposedNeighbors = List.foldl exposeCells exposedSelection checklist
        exposeMinedCell _ c = if (c.mined) then {c|exposed=True} else c
        exposedMines = if mined 
                       then Dict.map exposeMinedCell exposedNeighbors
                       else exposedNeighbors
    in exposedMines

gameOver : Board -> Bool
gameOver board = not (isEmpty (Dict.filter (\_ {exposed, mined} -> exposed && mined ) board))
