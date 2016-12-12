import List.Extra exposing (andThen)
import Dict exposing (Dict, fromList, values)
import Html as H
import Svg exposing (rect, svg, g)
import Svg.Attributes exposing (transform, version, x, y, width, height)

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

type Msg = LeftPick Pos | RightPick Pos

w = 15
h = 10
cellSize = 25

init : (Model, Cmd Msg)
init = 
    let boardAsList 
          = andThen (\r -> andThen (\c -> [((r, c), Cell False False False)]) 
            (List.range 0 (w-1)) ) (List.range 0 (h-1)) 
    in (Model (fromList boardAsList), Cmd.none)

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

view : (Model, Cmd Msg) -> H.Html Msg
view (model,_) = 
              svg 
                  [ version "1.1"
                  , width (toString (w * cellSize))
                  , height (toString (h * cellSize))
                  ] 
                  (values (Dict.map (\(r,c) _ -> showChecker r c) (model.board)))

update : Msg -> (Model, Cmd Msg) -> (Model, Cmd Msg)
update msg (model,_) = (model, Cmd.none)

main =
  H.beginnerProgram { 
      model = init, 
      view = view, 
      update = update 
  }
