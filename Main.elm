import List exposing (map2, length, range, filter, map, concatMap)
import List.Extra exposing (andThen)
import Dict exposing (Dict, fromList, values, insert, get, filter, isEmpty)
import Maybe exposing (withDefault)
import Html exposing (Html, beginnerProgram)
import Svg exposing (Svg, rect, svg, g, text_, line, polygon, circle)
import Svg.Attributes exposing (transform, version, r, x, y, width, height, style, textAnchor, fill, fontSize)
import Svg.Events exposing (onClick)
import Random.Pcg as R exposing (Generator, Seed, step, float, map, list,initialSeed)

import RightClick exposing (onRightClick)
import Pos exposing (..)
import Msg exposing (..)
import Flag exposing (showFlag)
import Mine exposing (showMine)

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
h = 16

cellSize : Int
cellSize = 25

generateCell : Generator Cell
generateCell = R.map (\t -> Cell (t < 0.201) False False 0) (float 0.0 1.0)

generateBoard : Generator Board
generateBoard = 
    let indices = andThen (\y -> andThen (\x -> [(x, y)]) 
                              (range 0 (w-1)) ) (range 0 (h-1)) 
    in  R.map (\cs -> fromList (map2 (,) indices cs))
            (list (length indices) generateCell)

init : (Board, Cmd Msg)
init = let (board, _) = (step generateBoard (initialSeed 0))
       in (board, Cmd.none)

getColor : Cell -> String
getColor {exposed} = if exposed then "#909090" else "#AAAAAA"

showSquare : Pos -> Cell -> Svg Msg
showSquare (xCoord,yCoord) cell = 
          rect [ x "0.05"
               , y "0.05"
               , width "0.9" 
               , height "0.9" 
               , style ("fill:" ++ getColor cell)
               , onClick (LeftPick (xCoord, yCoord))
               , onRightClick (RightPick (xCoord, yCoord))
               ] 
               []

showText : Pos -> Int -> List (Svg Msg)
showText pos count = 
    [ text_ [ x "0.5"
            , y "0.87" 
            , fontSize "1.0"
            , fill "blue"
            , textAnchor "middle"
            , onClick (LeftPick pos)
            , onRightClick (RightPick pos)
            ] 
            [ Svg.text (toString count)
            ]
    ]

showCellDetail : Pos -> Cell -> List (Svg Msg)
showCellDetail pos {mined, exposed, flagged, mineCount} = 
    case (  mined, exposed, flagged, 0 == mineCount) of
         (      _,       _,    True,     _) -> showFlag pos 
         (   True,    True,       _,     _) -> showMine pos 
         (      _,    True,       _, False) -> showText pos mineCount
         (      _,       _,       _,     _) -> []

showCell : Pos -> Cell -> Svg Msg
showCell pos cell = 
    let (x,y) = pos 
    in g [ transform ("scale (" ++ toString cellSize ++ ", " ++ toString cellSize ++ ") " ++ "translate (" ++ toString x ++ ", " ++ toString y ++ ") " )
         ]
         ([ showSquare pos cell ] ++ showCellDetail pos cell)

gameOver : Board -> Bool
gameOver board = not (isEmpty (Dict.filter (\_ {exposed, mined} -> exposed && mined ) board))

view : (Board, Cmd Msg) -> Html Msg
view (board,_) = 
    svg 
        [ version "1.1"
        , width (toString (w * cellSize))
        , height (toString (h * cellSize))
        ] 
        (values (Dict.map (\p c -> showCell p c) (board)))

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

update : Msg -> (Board, Cmd Msg) -> (Board, Cmd Msg)
update msg (board,_) = 
    if gameOver board 
    then (board, Cmd.none)
    else case msg of
             LeftPick pos -> 
                 (exposeCells pos board, Cmd.none)

             RightPick pos ->
                 let c = withDefault (Cell False False False 0) (get pos board)
                 in if (c.exposed)
                    then (board, Cmd.none) -- can't flag an exposed cell.
                    else (insert pos ({c|flagged = not (c.flagged)}) board, Cmd.none)

main =
  beginnerProgram { 
      model = init, 
      view = view, 
      update = update 
  }
