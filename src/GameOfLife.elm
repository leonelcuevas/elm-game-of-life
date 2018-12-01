module GameOfLife exposing (Board, Cell(..), createBoard, nextState, setCell, toList)

import Dict
import Grid exposing (Coord, Grid)
import List.Extra


type Cell
    = Alive
    | Dead


type alias Board =
    Grid Cell


map : (Coord -> Cell -> Cell) -> Board -> Board
map =
    Grid.map


toList : (( Coord, Cell ) -> a) -> Board -> List a
toList =
    Grid.toList


createBoard : Int -> Int -> Board
createBoard width height =
    Grid.emptyGrid width height Dead


nextCellState : Board -> Coord -> Cell -> Cell
nextCellState board coord cell =
    let
        neighbors =
            Grid.getNeighborValues board coord

        aliveNeighbors =
            case neighbors of
                Just neighborList ->
                    List.Extra.count (\el -> el == Alive) neighborList

                Nothing ->
                    0
    in
    case cell of
        Alive ->
            if aliveNeighbors == 2 || aliveNeighbors == 3 then
                Alive

            else
                Dead

        Dead ->
            if aliveNeighbors == 3 then
                Alive

            else
                Dead


nextState : Board -> Board
nextState board =
    { board | data = Dict.map (nextCellState board) board.data }


setCell : Board -> Coord -> Board
setCell board coord =
    Grid.setValue board coord Alive
