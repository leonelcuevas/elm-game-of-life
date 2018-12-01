module Main exposing (main)

import Browser
import Canvas exposing (Commands)
import CanvasColor
import GameOfLife exposing (Board, Cell(..))
import Html exposing (Attribute, Html, br, button, div, h1, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Time


main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { board : Board
    , tileSize : Float
    , mouseMoving : Bool
    , iterationTime : Float
    , autoIterating : Bool
    , canvasWidth : Int
    , canvasHeight : Int
    }


type alias MouseCoords =
    ( Float, Float )



-- Model


initialModel : () -> ( Model, Cmd Msg )
initialModel flags =
    let
        model =
            { board = GameOfLife.createBoard 100 50
            , tileSize = 10
            , mouseMoving = False
            , iterationTime = 100 -- miliseconds
            , autoIterating = False
            , canvasWidth = 1000
            , canvasHeight = 500
            }
    in
    ( model, Cmd.none )



-- Messages


type Msg
    = MouseDownAt MouseCoords
    | MouseMoveAt MouseCoords
    | MouseUpAt MouseCoords
    | NextIteration
    | ToggleAuto



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDownAt coords ->
            ( { model
                | board = setCellFromMouseCoords model coords
                , mouseMoving = True
              }
            , Cmd.none
            )

        MouseMoveAt coords ->
            let
                newModel =
                    if model.mouseMoving then
                        { model
                            | board = setCellFromMouseCoords model coords
                        }

                    else
                        model
            in
            ( newModel, Cmd.none )

        MouseUpAt coords ->
            ( { model
                | board = setCellFromMouseCoords model coords
                , mouseMoving = False
              }
            , Cmd.none
            )

        NextIteration ->
            ( { model | board = GameOfLife.nextState model.board }
            , Cmd.none
            )

        ToggleAuto ->
            ( { model | autoIterating = not model.autoIterating }
            , Cmd.none
            )


setCellFromMouseCoords : Model -> MouseCoords -> Board
setCellFromMouseCoords { board, tileSize } ( p_x, p_y ) =
    let
        x =
            floor (p_x / tileSize)

        y =
            floor (p_y / tileSize)
    in
    GameOfLife.setCell board ( x, y )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.autoIterating then
        Time.every model.iterationTime (\time -> NextIteration)

    else
        Sub.none



-- View


view : Model -> Html Msg
view model =
    div [ style "textAlign" "center", style "fontFamily" "helvetica" ]
        [ br [] []
        , div []
            [ h1 [] [ text "Game Of Life" ]
            , br [] []
            , renderBoard model
            , br [] []
            , button [ onClick NextIteration ] [ text "Next Iteration" ]
            , button [ onClick ToggleAuto ] [ text "Play / Stop" ]
            ]
        ]


renderBoard : Model -> Html Msg
renderBoard model =
    let
        drawingCoords =
            getDrawingCoords model
    in
    Canvas.element
        model.canvasWidth
        model.canvasHeight
        [ style "border" "1px solid black"
        , Mouse.onDown (\event -> MouseDownAt event.offsetPos)
        , Mouse.onMove (\event -> MouseMoveAt event.offsetPos)
        , Mouse.onUp (\event -> MouseUpAt event.offsetPos)
        ]
        (renderBoardHelper
            model.tileSize
            drawingCoords
            Canvas.empty
        )


renderBoardHelper : Float -> List ( Float, Float, Cell ) -> Commands -> Commands
renderBoardHelper size drawingCoords canvas =
    List.foldl (renderSquare size) canvas drawingCoords


getDrawingCoords : Model -> List ( Float, Float, Cell )
getDrawingCoords { board, tileSize } =
    GameOfLife.toList
        (\( ( x, y ), cell ) ->
            let
                coord_x =
                    tileSize * toFloat x

                coord_y =
                    tileSize * toFloat y
            in
            ( coord_x, coord_y, cell )
        )
        board


renderSquare : Float -> ( Float, Float, Cell ) -> Commands -> Commands
renderSquare size ( x, y, cell ) cmds =
    let
        color =
            case cell of
                Alive ->
                    CanvasColor.rgb 0 200 0

                Dead ->
                    CanvasColor.rgb 128 128 128
    in
    cmds
        |> Canvas.fillStyle color
        |> Canvas.fillRect x y size size
