module Main exposing (..)

import Browser exposing (element)
import Board exposing (Board, Cell, exposeCells, gameOver, h, w)
import Dict exposing (Dict, filter, fromList, get, insert, isEmpty, values)
import Flag exposing (showFlag)
import Html exposing (Html, button, div, text)
import List exposing (concatMap, filter, length, map, map2, range)
import List.Extra exposing (andThen)
import Maybe exposing (withDefault)
import Mine exposing (showMine)
import Msg exposing (..)
import Pos exposing (..)
import Random as R exposing (Generator, Seed, float, initialSeed, list, map, step)
import RightClick exposing (onRightClick)
import Smiley exposing (showFace)
import Svg exposing (Svg, circle, g, line, polygon, rect, svg, text_)
import Svg.Attributes exposing (fill, fontSize, height, r, style, textAnchor, transform, version, width, x, y)
import Svg.Events exposing (onClick)
import Task exposing (perform)
import Time exposing (now, posixToMillis)
import String exposing (fromInt)


type alias Game =
    { board : Board, seed : Seed }


cellSize : Int
cellSize =
    20


generateCell : Generator Cell
generateCell =
    R.map (\t -> Cell (t < 0.201) False False 0) (float 0.0 1.0)


generateBoard : Generator Board
generateBoard =
    let
        indices =
            andThen
                (\y ->
                    andThen (\x -> [ ( x, y ) ])
                        (range 0 (w - 1))
                )
                (range 0 (h - 1))
    in
    R.map (\cs -> fromList (map2 (\a b -> (a,b)) indices cs))
        (list (length indices) generateCell)


getColor : Cell -> String
getColor { exposed } =
    if exposed then
        "#909090"
    else
        "#CCCCCC"


showSquare : Pos -> Cell -> Svg Msg
showSquare ( xCoord, yCoord ) cell =
    rect
        [ x "0.05"
        , y "0.05"
        , width "0.9"
        , height "0.9"
        , style ("fill:" ++ getColor cell)
        , onClick (LeftPick ( xCoord, yCoord ))
        , onRightClick (RightPick ( xCoord, yCoord ))
        ]
        []


showText : Pos -> Int -> List (Svg Msg)
showText pos count =
    let
        textColor =
            case count of
                1 ->
                    "blue"

                2 ->
                    "green"

                3 ->
                    "red"

                4 ->
                    "brown"

                _ ->
                    "purple"
    in
    [ text_
        [ x "0.5"
        , y "0.87"
        , fontSize "1.0"
        , fill textColor
        , textAnchor "middle"
        , onClick (LeftPick pos)
        , onRightClick (RightPick pos)
        ]
        [ Svg.text (fromInt count)
        ]
    ]


showCellDetail : Pos -> Cell -> List (Svg Msg)
showCellDetail pos cell =
    if cell.flagged then
        showFlag pos
    else
        if cell.mined && cell.exposed then
            showMine pos
        else
            if cell.exposed && cell.mineCount /= 0 then
                showText pos cell.mineCount
            else
                []

showCell : Pos -> Cell -> Svg Msg
showCell pos cell =
    let
        ( x, y ) =
            pos

        scale =
            fromInt cellSize
    in
    g
        [ transform
            ("scale ("
                ++ scale
                ++ ", "
                ++ scale
                ++ ") "
                ++ "translate ("
                ++ fromInt x
                ++ ", "
                ++ fromInt y
                ++ ") "
            )
        ]
        ([ showSquare pos cell ] ++ showCellDetail pos cell)


centerStyle =
    style "width: 75%; margin: 0 auto;text-align:center;"


view : Game -> Html Msg
view { board } =
    div []
        [ div [ centerStyle ] (showFace (gameOver board))
        , div [ centerStyle ] [text "Implemented in Elm"]
        , div [ centerStyle ]
            [ svg
                [ version "1.1"
                , width (fromInt (w * cellSize))
                , height (fromInt (h * cellSize))
                ]
                (values (Dict.map (\p c -> showCell p c) board))
            ]
        , div [ centerStyle ] [ button [ onClick Reset ] [ text "reset" ] ]
        ]


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        InitBoard s ->
            let
                ( b, ns ) =
                    step generateBoard (initialSeed s)
            in
            ( { board = b, seed = ns }, Cmd.none )

        Reset ->
            let
                ( b, ns ) =
                    step generateBoard game.seed
            in
            ( { board = b, seed = ns }, Cmd.none )

        LeftPick pos ->
            if gameOver game.board then
                ( game, Cmd.none )
            else
                ( { board = exposeCells pos game.board, seed = game.seed }, Cmd.none )

        RightPick pos ->
            if gameOver game.board then
                ( game, Cmd.none )
            else
                let
                    { board, seed } =
                        game

                    c =
                        withDefault (Cell False False False 0) (get pos board)
                in
                if c.exposed then
                    ( game, Cmd.none )
                    -- can't flag an exposed cell.
                else
                    ( { board = insert pos { c | flagged = not c.flagged } board, seed = seed }, Cmd.none )

initBoard =
    perform (\t -> InitBoard (posixToMillis t)) Time.now


main : Program () Game Msg
main =
    element
        { init = \_ -> ( Game Dict.empty (initialSeed 0) , initBoard )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
