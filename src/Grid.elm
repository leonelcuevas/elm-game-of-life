module Grid exposing (Coord, Grid, emptyGrid, getNeighborValues, map, setValue, toList)

import Dict exposing (Dict)
import Maybe.Extra


type alias Coord =
    ( Int, Int )


type alias Grid a =
    { data : Dict Coord a
    , width : Int
    , height : Int
    }


emptyGrid : Int -> Int -> a -> Grid a
emptyGrid width height default =
    let
        coordinates =
            coordTuples width height

        asocList =
            List.map (\c -> ( c, default )) coordinates
    in
    { data = Dict.fromList asocList
    , width = width
    , height = height
    }


rangeNonInc : Int -> Int -> List Int
rangeNonInc start end =
    List.range start (end - 1)


coordTuples : Int -> Int -> List Coord
coordTuples width height =
    List.concatMap (\y -> List.map (\x -> ( x, y )) (rangeNonInc 0 width)) (rangeNonInc 0 height)


isInsideGrid : Grid a -> Coord -> Bool
isInsideGrid grid ( x, y ) =
    let
        w =
            grid.width

        h =
            grid.height
    in
    x >= 0 && x < w && y >= 0 && y < h


setValue : Grid a -> Coord -> a -> Grid a
setValue grid coord val =
    if isInsideGrid grid coord then
        let
            newData =
                Dict.insert coord val grid.data
        in
        { grid | data = newData }

    else
        grid


getValue : Grid a -> Coord -> Maybe a
getValue { data } coord =
    Dict.get coord data


getNeighborValues : Grid a -> Coord -> Maybe (List a)
getNeighborValues grid coord =
    if isInsideGrid grid coord then
        let
            neighborCoords =
                getNeighborCoords coord

            maybeNeighbors =
                List.map (\c -> getValue grid c) neighborCoords
        in
        Just (Maybe.Extra.values maybeNeighbors)

    else
        Nothing


getNeighbors : Grid a -> Coord -> Maybe (List ( Coord, a ))
getNeighbors grid coord =
    if isInsideGrid grid coord then
        let
            neighborCoords =
                getNeighborCoords coord

            maybeNeighbors =
                grid.data
                    |> Dict.filter (\c _ -> List.member c neighborCoords)
        in
        Just (Dict.toList maybeNeighbors)

    else
        Nothing


getNeighborCoords : Coord -> List Coord
getNeighborCoords ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]


map : (Coord -> a -> b) -> Grid a -> Grid b
map f grid =
    { data = Dict.map f grid.data
    , width = grid.width
    , height = grid.height
    }


toList : (( Coord, a ) -> b) -> Grid a -> List b
toList f grid =
    List.map f (Dict.toList grid.data)
