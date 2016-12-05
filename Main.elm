import List exposing (range, concatMap, foldl, head,member,filter,length,minimum,concat,map,map2,tail)
import List.Extra exposing (minimumBy, andThen)
import String exposing (join)
import Dict exposing (Dict, fromList, map, toList, keys, values)
import Html as H
import Html.Attributes as HA
import Time exposing (Time,every, second)
import Svg exposing (rect, line, svg, g)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (transform, version, viewBox, x, y, x1, y1, x2, y2, fill, style, width, height)

import Html.Events exposing (onWithOptions)
import Json.Decode as Json 

type alias Cell = 
    { mined : Bool 
    , exposed : Bool
    , flagged : Bool
    } 

type alias Pos = (Int, Int)

type alias Model = 
    { 
    board : Dict Pos Cell
    }

type Msg = NoOp | Tick Time | SetStart Pos

w = 40
h = 80
cellSize = 20

init : (Model, Cmd Msg)
init = 
    let boardAsList 
          = andThen (\r -> andThen (\c -> [((r, c), Cell False False False)]) 
            (List.range 0 (w-1)) ) (List.range 0 (h-1)) 
        board = fromList boardAsList
    in (Model board, Cmd.none)

view : (Model, Cmd Msg) -> H.Html Msg
view (model,_) = 
    let size = 0.9
        placement = 0.5 - (size/2.0)

        showChecker row col = 
            g [ transform ("scale (" ++ toString cellSize ++ ", " ++ toString cellSize ++ ") " ++ "translate (" ++ toString col ++ ", " ++ toString row ++ ") " )
              ]
              [ rect [ x (toString 0.05)
                     , y (toString 0.05)
                     , width (toString 0.9)
                     , height (toString 0.9)
                     ] 
                     []
              ]

        render model = Dict.map (\(r,c) _ -> showChecker r c) model.board
        center = HA.style [ ( "text-align", "center") ] 

    in 
        H.div 
          []
          [ H.div 
              [center] 
              [ svg 
                  [ version "1.1"
                  , width (toString (w * cellSize))
                  , height (toString (h * cellSize))
                  ] 
                  [ g [] <| values (render model)]
              ]
          ]

update : Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
update msg (model,_) = (model, Cmd.none)

main =
  H.beginnerProgram { 
      model = init, 
      view = view, 
      update = update 
  }
