module Flag exposing (showFlag)

import Svg exposing (Svg, line, polygon)
import Svg.Attributes exposing (fill, stroke, strokeWidth, x1, y1, x2, y2, points)
import Svg.Events exposing (onClick)

import Pos exposing (..)
import Msg exposing (..)
import RightClick exposing (..)

showFlag : Pos -> List (Svg Msg)
showFlag pos = 
    [ polygon [ points "0.20,0.40 0.70,0.55 0.70,0.25"
              , fill   "red"
              , onClick (LeftPick pos)
              , onRightClick (RightPick pos)
              ]
              [
              ]

    , line    [ x1 "0.70" 
              , y1 "0.25" 
              , x2 "0.70"
              , y2 "0.85"
              , strokeWidth ".07"
              , stroke "black"
              , onClick (LeftPick pos)
              , onRightClick (RightPick pos)
              ] 
              [
              ]
    ]
