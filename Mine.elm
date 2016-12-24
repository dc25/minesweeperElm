module Mine exposing (showMine)

import Svg exposing (Svg, polygon, circle)
import Svg.Attributes exposing (fill, cx, cy, r, points)
import Svg.Events exposing (onClick)

import Pos exposing (..)
import Msg exposing (..)
import RightClick exposing (..)

showMine : Pos -> List (Svg Msg)
showMine pos = 
    [ polygon [ points "0.65,0.15 0.85,0.35 0.65,0.55 0.45,0.35 " 
              , fill   "brown"
              ]
              [
              ]

    , circle [ cx "0.45" 
             , cy "0.55"
             , r  "0.3"
             , fill "brown"
             , onClick (LeftPick pos)
             , onRightClick (RightPick pos)
             ] 
             [
             ]
    ]
