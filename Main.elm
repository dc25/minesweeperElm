import List exposing (map2, length, range)
import List.Extra exposing (andThen)
import Dict exposing (Dict, fromList, values, insert, get)
import Maybe exposing (withDefault)
import Html exposing (Html, beginnerProgram)
import Svg exposing (Svg, rect, svg, g, text_)
import Svg.Attributes exposing (transform, version, x, y, width, height, style, textAnchor, fill, fontSize)
import Svg.Events exposing (onClick, on)
import Json.Decode as Json
import Html.Events exposing (onWithOptions)
import Random.Pcg exposing (Generator, Seed, step, float, map, list,initialSeed)

type alias Cell = 
    { mined : Bool 
    , exposed : Bool
    , flagged : Bool
    , mineCount : Int
    } 

type alias Pos = (Int, Int)

type alias Board = Dict Pos Cell

type Msg = LeftPick Pos | RightPick Pos

w : Int
w = 40

h : Int
h = 80

cellSize : Int
cellSize = 25


onRightClick message =
  onWithOptions
    "contextmenu"
    { stopPropagation = True
    , preventDefault = True
    }
    (Json.succeed message)

generateCell : Generator Cell
generateCell = map (\t -> Cell (t < 0.201) False False 0) (float 0.0 1.0)

generateBoard : Generator Board
generateBoard = 
    let indices = andThen (\r -> andThen (\c -> [(r, c)]) 
                              (range 0 (w-1)) ) (range 0 (h-1)) 
    in  map (\cs -> fromList (map2 (,) indices cs))
            (list (length indices) generateCell)

init : (Board, Cmd Msg)
init = let (board, _) = (step generateBoard (initialSeed 0))
       in (board, Cmd.none)

getColor : Cell -> String
getColor {exposed} = if exposed then "#909090" else "#AAAAAA"

showSquare : Pos -> Cell -> Svg Msg
showSquare (row,col) cell = 
          rect [ x (toString 0.05)
               , y (toString 0.05)
               , width (toString 0.9)
               , height (toString 0.9)
               , style ("fill:" ++ getColor cell)
               , onClick (LeftPick (row, col))
               , onRightClick (RightPick (row, col))
               ] 
               []

showText : Pos -> Int -> Svg Msg
showText pos count = 
    text_ [ x "0.5"
          , y "0.87" 
          , fontSize "1.0"
          -- , dominantBaseline "middle"
          , fill "blue"
          , textAnchor "middle"
          , onClick (LeftPick pos)
          , onRightClick (RightPick pos)
          ] 
          [ Svg.text (toString count)
          ]

showCellDetail : Pos -> Cell -> Svg Msg
showCellDetail pos {mined, exposed, flagged, mineCount} = 
    case (  mined, exposed, flagged, 0 == mineCount) of
         (      _,       _,    True,     _) -> showText pos mineCount
         (   True,    True,       _,     _) -> showText pos mineCount
         (      _,    True,       _, False) -> showText pos mineCount
         (      _,       _,       _,     _) -> showText pos mineCount

showCell : Pos -> Cell -> Svg Msg
showCell pos cell = 
    let (row,col) = pos 
    in g [ transform ("scale (" ++ toString cellSize ++ ", " ++ toString cellSize ++ ") " ++ "translate (" ++ toString col ++ ", " ++ toString row ++ ") " )
         ]
         [ showSquare pos cell
         , showCellDetail pos cell
         ]

view : (Board, Cmd Msg) -> Html Msg
view (model,_) = 
    svg 
        [ version "1.1"
        , width (toString (w * cellSize))
        , height (toString (h * cellSize))
        ] 
        (values (Dict.map (\p c -> showCell p c) (model)))

update : Msg -> (Board, Cmd Msg) -> (Board, Cmd Msg)
update msg (model,_) = 
    case msg of
        (LeftPick pos) ->
            let c = withDefault (Cell False False False 0) (get pos model)
            in (insert pos ({c|exposed = True}) model, Cmd.none)
        (RightPick pos) ->
            let c = withDefault (Cell False False False 0) (get pos model)
            in (insert pos ({c|flagged = not (c.flagged)}) model, Cmd.none)

main =
  beginnerProgram { 
      model = init, 
      view = view, 
      update = update 
  }
