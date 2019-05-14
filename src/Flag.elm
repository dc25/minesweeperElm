module Flag exposing (showFlag)

import Msg exposing (..)
import Pos exposing (..)
import RightClick exposing (..)
import Svg exposing (Svg, line, polygon)
import Svg.Attributes exposing (fill, points, stroke, strokeWidth, x1, x2, y1, y2)
import Svg.Events exposing (onClick)


showFlag : Pos -> List (Svg Msg)
showFlag pos =
    [ polygon
        [ points "0.20,0.40 0.70,0.55 0.70,0.25"
        , fill "red"
        , onClick (LeftPick pos)
        , onRightClick (RightPick pos)
        ]
        []
    , line
        [ x1 "0.70"
        , y1 "0.25"
        , x2 "0.70"
        , y2 "0.85"
        , strokeWidth ".07"
        , stroke "black"
        , onClick (LeftPick pos)
        , onRightClick (RightPick pos)
        ]
        []
    ]
