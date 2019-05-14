module Mine exposing (showMine)

import Msg exposing (..)
import Pos exposing (..)
import RightClick exposing (..)
import Svg exposing (Svg, circle, polygon)
import Svg.Attributes exposing (cx, cy, fill, points, r)
import Svg.Events exposing (onClick)


showMine : Pos -> List (Svg Msg)
showMine pos =
    [ polygon
        [ points "0.65,0.15 0.85,0.35 0.65,0.55 0.45,0.35 "
        , fill "brown"
        ]
        []
    , circle
        [ cx "0.45"
        , cy "0.55"
        , r "0.3"
        , fill "brown"
        , onClick (LeftPick pos)
        , onRightClick (RightPick pos)
        ]
        []
    ]
