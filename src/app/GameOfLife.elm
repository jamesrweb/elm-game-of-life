module GameOfLife exposing (Board(..), Cell(..), CellState, Coordinate(..), Grid, Model(..), Msg(..), main)

import Array exposing (Array)
import Browser
import Browser.Events
import Grid
import Html exposing (Html)
import Html.Attributes
import List.Extra
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Grid =
    Array (Array Cell)


type Board
    = Board Grid


type Model
    = Loading
    | Loaded Board


type CellState
    = Alive
    | Empty


width : number
width =
    64


height : number
height =
    32


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GotInitialState <| Random.list (width * height) <| Random.weighted ( 80, Empty ) [ ( 20, Alive ) ]
    )


type Msg
    = AnimationFrameTick
    | GotInitialState (List CellState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrameTick ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Loaded board ->
                    ( Loaded <| evolve board, Cmd.none )

        GotInitialState chars ->
            let
                grid : Grid
                grid =
                    List.Extra.greedyGroupsOf width chars
                        |> List.indexedMap
                            (\rowIndex row ->
                                List.indexedMap
                                    (\columnIndex column ->
                                        Cell ( Coordinate ( columnIndex, rowIndex ), column )
                                    )
                                    row
                            )
                        |> List.map Array.fromList
                        |> Array.fromList
            in
            ( Loaded <| Board grid, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrame (always AnimationFrameTick)


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            Html.p []
                [ Html.text "Loading..."
                ]

        Loaded board ->
            viewBoard board


viewBoard : Board -> Html Msg
viewBoard (Board grid) =
    Html.article [ Html.Attributes.class "grid h-dvh place-items-center font-mono text-sm" ] <| (Array.map toRow grid |> Array.toList |> Html.div [ Html.Attributes.class "grid gap-2" ] |> List.singleton)


toRow : Array Cell -> Html Msg
toRow =
    Array.map toCell >> Array.toList >> Html.div [ Html.Attributes.class "flex gap-2" ]


toCell : Cell -> Html Msg
toCell (Cell ( _, state )) =
    Html.text
        (String.fromChar <|
            case state of
                Alive ->
                    '+'

                Empty ->
                    '_'
        )
        |> List.singleton
        |> Html.span [ Html.Attributes.classList [ ( "font-bold", state == Alive ) ] ]


type Coordinate
    = Coordinate ( Int, Int )


type Cell
    = Cell ( Coordinate, CellState )


countNeighbours : Grid -> Cell -> Int
countNeighbours grid (Cell ( Coordinate ( x, y ), _ )) =
    [ Coordinate ( x - 1, y )
    , Coordinate ( x + 1, y )
    , Coordinate ( x, y + 1 )
    , Coordinate ( x, y - 1 )
    , Coordinate ( x - 1, y - 1 )
    , Coordinate ( x - 1, y + 1 )
    , Coordinate ( x + 1, y - 1 )
    , Coordinate ( x + 1, y + 1 )
    ]
        |> Array.fromList
        |> Array.filter (isValidNeighbour grid)
        |> Array.length


evolve : Board -> Board
evolve (Board board) =
    Board <|
        Grid.fold2d
            { cols = width, rows = height }
            evolveCell
            board


evolveCell : ( Int, Int ) -> Grid -> Grid
evolveCell ( x, y ) grid =
    Array.get y grid
        |> Maybe.andThen (Array.get x)
        |> Maybe.map (\cell -> evolveCellHelper { cell = cell, grid = grid })
        |> Maybe.withDefault grid


evolveCellHelper : { cell : Cell, grid : Grid } -> Grid
evolveCellHelper { cell, grid } =
    let
        (Cell ( (Coordinate ( x, y )) as coords, _ )) =
            cell

        nextState : CellState
        nextState =
            resolveNextCellState grid cell
    in
    Array.indexedMap
        (\rowIndex row ->
            Array.indexedMap
                (\columnIndex column ->
                    if columnIndex == x && rowIndex == y then
                        Cell ( coords, nextState )

                    else
                        column
                )
                row
        )
        grid


resolveNextCellState : Grid -> Cell -> CellState
resolveNextCellState grid ((Cell ( _, state )) as cell) =
    if state == Alive then
        resolveAliveCellNextState grid cell

    else
        resolveEmptyCellNextState grid cell


resolveEmptyCellNextState : Grid -> Cell -> CellState
resolveEmptyCellNextState grid ((Cell ( _, state )) as cell) =
    let
        neighbours : Int
        neighbours =
            countNeighbours grid cell
    in
    if neighbours == 3 then
        Alive

    else if List.range 0 8 |> List.member neighbours then
        Empty

    else
        state


resolveAliveCellNextState : Grid -> Cell -> CellState
resolveAliveCellNextState grid ((Cell ( _, state )) as cell) =
    let
        neighbours : Int
        neighbours =
            countNeighbours grid cell
    in
    if List.range 0 1 |> List.member neighbours then
        Empty

    else if List.range 4 8 |> List.member neighbours then
        Empty

    else if List.range 2 3 |> List.member neighbours then
        Alive

    else
        state


isValidNeighbour : Grid -> Coordinate -> Bool
isValidNeighbour grid (Coordinate ( x, y )) =
    Array.get y grid
        |> Maybe.andThen (Array.get x)
        |> Maybe.map (\(Cell ( _, state )) -> state == Alive)
        |> Maybe.withDefault False
