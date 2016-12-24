module RightClick exposing (onRightClick) 

import Json.Decode as Json
import Html.Events exposing (onWithOptions)

onRightClick message =
  onWithOptions
    "contextmenu"
    { stopPropagation = True
    , preventDefault = True
    }
    (Json.succeed message)
