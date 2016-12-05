import List.Extra exposing (andThen)
import Dict exposing (Dict, fromList, values)
import Html as H
import Svg exposing (rect, svg, g)
import Svg.Attributes exposing (transform, version, x, y, width, height)

type alias Pos = (Int, Int)

w = 40
h = 80
cellSize = 20

init : (Dict Pos (), Cmd ())
init = 
    let boardAsList 
          = andThen (\r -> andThen (\c -> [((r, c), ())]) 
            (List.range 0 (w-1)) ) (List.range 0 (h-1)) 
    in (fromList boardAsList, Cmd.none)

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

view : (Dict Pos (), Cmd ()) -> H.Html ()
view (model,_) = 
              svg 
                  [ version "1.1"
                  , width (toString (w * cellSize))
                  , height (toString (h * cellSize))
                  ] 
                  (values (Dict.map (\(r,c) _ -> showChecker r c) model))

update : () -> (Dict Pos (), Cmd ()) -> (Dict Pos (), Cmd ())
update msg (model,_) = (model, Cmd.none)

main =
  H.beginnerProgram { 
      model = init, 
      view = view, 
      update = update 
  }
