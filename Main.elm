import List exposing (map2, length, range, filter, map, concatMap)
import List.Extra exposing (andThen)
import Dict exposing (Dict, fromList, values, insert, get, filter, isEmpty)
import Maybe exposing (withDefault)
import Html exposing (Html, div, text, button, program)
import Svg exposing (Svg, rect, svg, g, text_, line, polygon, circle)
import Svg.Attributes exposing (transform, version, r, x, y, width, height, style, textAnchor, fill, fontSize)
import Svg.Events exposing (onClick)
import Random.Pcg as R exposing (Generator, Seed, step, float, map, list,initialSeed)

import RightClick exposing (onRightClick)
import Pos exposing (..)
import Game exposing (..)
import Msg exposing (..)
import Mine exposing (showMine)
import Flag exposing (showFlag)
import Smiley exposing (showFace)
import Time exposing (now)
import Task exposing (perform)
import Update exposing (exposeCells, gameOver)

cellSize : Int
cellSize = 20

generateCell : Generator Cell
generateCell = R.map (\t -> Cell (t < 0.201) False False 0) (float 0.0 1.0)

generateBoard : Generator Board
generateBoard = 
    let indices = andThen (\y -> andThen (\x -> [(x, y)]) 
                              (range 0 (w-1)) ) (range 0 (h-1)) 
    in  R.map (\cs -> fromList (map2 (,) indices cs))
            (list (length indices) generateCell)

getColor : Cell -> String
getColor {exposed} = if exposed then "#AAAAAA" else "#CCCCCC"

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
    let textColor = case count of
                        1 -> "cyan"
                        2 -> "green"
                        3 -> "red"
                        4 -> "brown"
                        _ -> "purple"

    in [ text_ [ x "0.5"
               , y "0.87" 
               , fontSize "1.0"
               , fill textColor
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

centerStyle = style "width: 75%; margin: 0 auto;text-align:center;"

view : Game -> Html Msg
view ({board}) = 
    div []
    (   [div [centerStyle] (showFace (gameOver board) )
        ,div [centerStyle] [ svg [ version "1.1"
                      , width (toString (w * cellSize))
                      , height (toString (h * cellSize))
                      ]
                      (values (Dict.map (\p c -> showCell p c) (board)))
                ]
        ,div [centerStyle] [ button [onClick Reset] [text "reset"] ]
        ]
    )

update : Msg -> Game -> (Game, Cmd Msg)
update msg game = 
    case msg of
        InitBoard s -> 
            let (b,ns) = (step generateBoard (initialSeed s))
            in ({board=b,seed=ns}, Cmd.none)

        LeftPick pos -> 
            if gameOver game.board
            then (game, Cmd.none)
            else ({board=exposeCells pos game.board, seed=game.seed}, Cmd.none)

        RightPick pos ->
            if gameOver game.board
            then (game, Cmd.none)
            else let {board, seed} = game
                     c = withDefault (Cell False False False 0) (get pos board)
                 in if (c.exposed)
                    then (game, Cmd.none) -- can't flag an exposed cell.
                    else ({board=insert pos ({c|flagged = not (c.flagged)}) board, seed=seed}, Cmd.none)

        Reset -> let (b,s) = step generateBoard game.seed
                 in ({board=b,seed=s}, Cmd.none)

initBoard = perform (\t -> InitBoard (round t)) now

main =
    program { init = ({board=Dict.empty,seed=initialSeed 0}, initBoard)
            , view = view
            , update = update
            , subscriptions = \m -> Sub.none 
            }
