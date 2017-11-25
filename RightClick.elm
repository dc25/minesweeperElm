module RightClick exposing (onRightClick)

import Html.Events exposing (onWithOptions)
import Json.Decode as Json


onRightClick message =
    onWithOptions
        "contextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.succeed message)
