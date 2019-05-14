module RightClick exposing (onRightClick)

import Html as HTML
import Html.Events as HEV exposing (custom)
import Json.Decode as Json


onRightClick : msg -> HTML.Attribute msg
onRightClick msg =
    HEV.custom "contextmenu"
        (Json.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )

