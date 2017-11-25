module Smiley exposing (showFace)

import Html exposing (Html)
import List exposing (concatMap, map)
import Msg exposing (..)
import Pos exposing (..)
import RightClick exposing (..)
import Svg exposing (Svg, circle, g, path, polygon, svg)
import Svg.Attributes exposing (cx, cy, d, fill, height, points, r, stroke, strokeWidth, style, transform, version, width)
import Svg.Events exposing (onClick)


showFace : Bool -> List (Html Msg)
showFace lost =
    let
        sz =
            100
    in
    [ svg
        [ version "1.1"
        , width (toString sz)
        , height (toString sz)
        ]
        [ g
            [ transform
                ("    scale ("
                    ++ toString sz
                    ++ ", "
                    ++ toString sz
                    ++ ") "
                    ++ "translate (0.5, 0.5)"
                )
            ]
            ([ -- face outline
               circle
                [ cx "0.0"
                , cy "0.0"
                , r "0.4"
                , style "fill:yellow"
                , stroke "black"
                , strokeWidth "0.02"
                ]
                []
             ]
                ++ -- eyes
                   map
                    (\xc ->
                        circle
                            [ cx (toString xc)
                            , cy "-0.1"
                            , r "0.08"
                            , style "fill:yellow"
                            , stroke "black"
                            , strokeWidth "0.02"
                            ]
                            []
                    )
                    [ 0.15, -0.15 ]
                ++ [ -- smile/frown
                     path
                        [ d
                            ("M-0.15,0.15 a0.2,0.2 0 0 "
                                ++ (if lost then
                                        "1"
                                    else
                                        "0"
                                   )
                                ++ " 0.30,0.0"
                            )
                        , stroke "black"
                        , strokeWidth "0.02"
                        , fill "none"
                        ]
                        []
                   ]
                ++ (if lost then
                        -- eye crosses
                        let
                            param =
                                [ -0.15, 0.15 ]
                                    |> concatMap
                                        (\ex ->
                                            [ -0.1, 0.1 ]
                                                |> concatMap
                                                    (\dx ->
                                                        [ -0.1, 0.1 ] |> List.map (\dy -> ( ex, dx, dy ))
                                                    )
                                        )
                        in
                        map
                            (\( ex, dx, dy ) ->
                                path
                                    [ d ("M " ++ toString ex ++ " -0.1 l " ++ toString dx ++ " " ++ toString dy)
                                    , stroke "black"
                                    , strokeWidth "0.02"
                                    , fill "none"
                                    ]
                                    []
                            )
                            param
                    else
                        -- eyeballs
                        map
                            (\xc ->
                                circle
                                    [ cx (toString xc)
                                    , cy "-0.1"
                                    , r "0.04"
                                    , style "fill:black"
                                    ]
                                    []
                            )
                            [ 0.15, -0.15 ]
                   )
            )
        ]
    ]
