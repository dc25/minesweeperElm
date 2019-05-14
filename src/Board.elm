module Board exposing (Board, Cell, exposeCells, gameOver, h, w)

import Dict exposing (Dict, fromList, get, insert, isEmpty)
import List exposing (concatMap, length, map2, range)
import List.Extra exposing (andThen)
import Maybe exposing (withDefault)
import Msg exposing (..)
import Pos exposing (..)
import Random as R exposing (Generator, float, list)


type alias Cell =
    { mined : Bool
    , exposed : Bool
    , flagged : Bool
    , mineCount : Int
    }


type alias Board =
    Dict Pos Cell


w : Int
w =
    40


h : Int
h =
    30


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
    R.map (\cs -> fromList (map2 (\a b->(a,b)) indices cs))
        (list (length indices) generateCell)


adjacents : Pos -> List Pos
adjacents ( x, y ) =
    let
        patch =
            range (x - 1) (x + 1)
                |> concatMap
                    (\xx ->
                        range (y - 1) (y + 1) |> List.map (\yy -> ( xx, yy ))
                    )
    in
    List.filter
        (\( xx, yy ) ->
            ( xx, yy )
                /= ( x, y )
                && xx
                >= 0
                && yy
                >= 0
                && xx
                < w
                && yy
                < h
        )
        patch


exposeCells : Pos -> Board -> Board
exposeCells pos board =
    let
        getCell bd ps =
            withDefault (Cell False False False 0) (get ps bd)

        c =
            getCell board pos

        indices =
            adjacents pos

        count =
            length (List.filter (\{ mined } -> mined) (List.map (getCell board) indices))

        checklist =
            if c.mined || c.exposed || c.flagged || count /= 0 then
                []
            else
                indices

        exposedSelection =
            insert pos { c | exposed = True, mineCount = count } board

        exposedNeighbors =
            List.foldl exposeCells exposedSelection checklist

        exposeMinedCell _ cl =
            if cl.mined then
                { cl | exposed = True }
            else
                cl

        exposedMines =
            if c.mined then
                Dict.map exposeMinedCell exposedNeighbors
            else
                exposedNeighbors
    in
    exposedMines


gameOver : Board -> Bool
gameOver board =
    not (isEmpty (Dict.filter (\_ { exposed, mined } -> exposed && mined) board))
