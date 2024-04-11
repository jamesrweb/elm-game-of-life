module GameOfLife exposing (Board(..), Cell(..), Coordinate(..), Grid, Model(..), Msg(..), main)

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


alive : Char
alive =
    '+'


empty : Char
empty =
    '.'


width : number
width =
    64


height : number
height =
    64


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GotInitialState <| Random.list (width * height) <| Random.uniform empty [ alive ]
    )


type Msg
    = AnimationFrameTick
    | GotInitialState (List Char)


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
toCell (Cell ( _, symbol )) =
    Html.text (String.fromChar symbol)
        |> List.singleton
        |> Html.span [ Html.Attributes.classList [ ( "font-bold", symbol == alive ) ] ]


type Coordinate
    = Coordinate ( Int, Int )


type Cell
    = Cell ( Coordinate, Char )


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
    if x >= 0 && y >= 0 && x < width && y < height then
        Array.get y grid
            |> Maybe.andThen (Array.get x)
            |> Maybe.map (\cell -> evolveCellHelper { cell = cell, grid = grid })
            |> Maybe.withDefault grid

    else
        grid


evolveCellHelper : { cell : Cell, grid : Grid } -> Grid
evolveCellHelper { cell, grid } =
    let
        (Cell ( (Coordinate ( x, y )) as coords, symbol )) =
            cell

        neighbours : Int
        neighbours =
            countNeighbours grid cell

        nextSymbol : Char
        nextSymbol =
            if symbol == alive then
                if List.range 0 1 |> List.member neighbours then
                    empty

                else if List.range 4 8 |> List.member neighbours then
                    empty

                else if List.range 2 3 |> List.member neighbours then
                    alive

                else
                    symbol

            else if symbol == empty then
                if neighbours == 3 then
                    alive

                else if List.range 0 8 |> List.member neighbours then
                    empty

                else
                    symbol

            else
                symbol
    in
    Array.indexedMap
        (\rowIndex row ->
            Array.indexedMap
                (\columnIndex column ->
                    if columnIndex == x && rowIndex == y then
                        Cell ( coords, nextSymbol )

                    else
                        column
                )
                row
        )
        grid


isValidNeighbour : Grid -> Coordinate -> Bool
isValidNeighbour grid (Coordinate ( x, y )) =
    Array.get y grid
        |> Maybe.andThen (Array.get x)
        |> Maybe.map (\(Cell ( _, symbol )) -> symbol == alive)
        |> Maybe.withDefault False
