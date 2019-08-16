module ConwaysGameOfLife exposing (init, subscriptions, update, view)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events
import Time exposing (Posix)



-- Model


type alias Model =
    { board : Board
    , gameState : GameState
    , cellSize : String
    , timeIntervalInSec : Float
    }


type GameState
    = Started
    | Stopped
    | GameOver


type alias Board =
    Array Row


type alias Row =
    Array Cell


type alias Cell =
    { isAlive : Bool }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( numberOfRow, numberOfCol ) =
            ( 80, 80 )
    in
    ( { board =
            Array.repeat numberOfRow <|
                Array.repeat numberOfCol { isAlive = False }
      , gameState = Stopped
      , cellSize = "14px"
      , timeIntervalInSec = 0.4
      }
    , Cmd.none
    )



-- Update


type Msg
    = StartButtonClicked
    | StopButtonClicked
    | ResetButtonClicked
    | CellClicked Int Int
    | UpdateGame Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartButtonClicked ->
            ( -- only start game when living cell exists
              if hasLivingCellInBoard model.board then
                { model | gameState = Started }

              else
                model
            , Cmd.none
            )

        StopButtonClicked ->
            ( { model | gameState = Stopped }
            , Cmd.none
            )

        ResetButtonClicked ->
            init ()

        CellClicked colNumber rowNumber ->
            ( changeCell rowNumber colNumber model
            , Cmd.none
            )

        UpdateGame intervel ->
            ( getNextModel model
            , Cmd.none
            )


changeCell : Int -> Int -> Model -> Model
changeCell rowNumber colNumber model =
    case Array.get rowNumber model.board of
        Nothing ->
            model

        Just row ->
            case Array.get colNumber row of
                Nothing ->
                    model

                Just cell ->
                    let
                        newRow =
                            Array.set
                                colNumber
                                { cell | isAlive = not cell.isAlive }
                                row

                        newBoard =
                            Array.set rowNumber newRow model.board
                    in
                    { model
                        | board = newBoard
                        , gameState =
                            if model.gameState == GameOver then
                                Stopped

                            else
                                model.gameState
                    }


getNextModel : Model -> Model
getNextModel model =
    let
        newBoard =
            -- Array.indexedMap (getNextRow model) model.board
            getNextBoard model

        newGameState =
            if hasLivingCellInBoard newBoard then
                model.gameState

            else
                GameOver
    in
    { model
        | board = newBoard
        , gameState = newGameState
    }


getNextBoard : Model -> Board
getNextBoard model =
    Array.indexedMap (getNextRow model) model.board


getNextRow : Model -> Int -> Row -> Row
getNextRow model rowNumber row =
    Array.indexedMap (getNextCell model rowNumber) row


getNextCell : Model -> Int -> Int -> Cell -> Cell
getNextCell model rowNumber colNumber cell =
    let
        livingNeighberNumber =
            getLivingNeighberNumber rowNumber colNumber model.board
    in
    if cell.isAlive then
        if livingNeighberNumber < 2 || livingNeighberNumber > 3 then
            { cell | isAlive = False }

        else
            cell

    else if livingNeighberNumber == 3 then
        { cell | isAlive = True }

    else
        cell


getLivingNeighberNumber : Int -> Int -> Board -> Int
getLivingNeighberNumber rowNumber colNumber board =
    let
        neighberLocations =
            [ ( rowNumber - 1, colNumber - 1 )
            , ( rowNumber - 1, colNumber )
            , ( rowNumber - 1, colNumber + 1 )
            , ( rowNumber, colNumber - 1 )
            , ( rowNumber, colNumber + 1 )
            , ( rowNumber + 1, colNumber - 1 )
            , ( rowNumber + 1, colNumber )
            , ( rowNumber + 1, colNumber + 1 )
            ]
    in
    List.map (isLivingCell board) neighberLocations
        |> List.sum


isLivingCell : Board -> ( Int, Int ) -> Int
isLivingCell board ( rowNumber, colNumber ) =
    case Array.get rowNumber board of
        Just row ->
            case Array.get colNumber row of
                Just cell ->
                    if cell.isAlive then
                        1

                    else
                        0

                Nothing ->
                    0

        Nothing ->
            0


hasLivingCellInBoard : Board -> Bool
hasLivingCellInBoard board =
    Array.map hasLivingCellInRow board
        |> Array.filter identity
        |> Array.isEmpty
        |> not


hasLivingCellInRow : Row -> Bool
hasLivingCellInRow row =
    Array.map (\cell -> cell.isAlive) row
        |> Array.filter identity
        |> Array.isEmpty
        |> not



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameState == Started then
        Time.every (model.timeIntervalInSec * 1000) UpdateGame

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        bottonText : String
        bottonText =
            if model.gameState == Started then
                "Stop"

            else
                "Start"

        bottonMsg : Msg
        bottonMsg =
            if model.gameState == Started then
                StopButtonClicked

            else
                StartButtonClicked
    in
    div []
        [ viewBotton bottonMsg bottonText
        , viewBotton ResetButtonClicked "Reset"
        , div
            [ style "display" "inline-block"
            , style "margin-left" "20px"
            ]
            [ text
                (if model.gameState == GameOver then
                    "Game Over :'("

                 else
                    ""
                )
            ]
        , viewBoard model
        ]


viewBotton : Msg -> String -> Html Msg
viewBotton msg text =
    button
        [ style "width" "80px"
        , style "height" "30px"
        , style "margin-top" "20px"
        , style "margin-left" "20px"
        , Html.Events.onClick msg
        ]
        [ Html.text text ]


viewBoard : Model -> Html Msg
viewBoard model =
    div [ style "margin" "20px 20px" ]
        [ Array.indexedMap (viewRow model.cellSize) model.board
            |> Array.toList
            |> div []
        ]


viewRow : String -> Int -> Row -> Html Msg
viewRow cellSize rowNumber row =
    Array.indexedMap (viewCell cellSize rowNumber) row
        |> Array.toList
        |> div [ style "height" cellSize ]


viewCell : String -> Int -> Int -> Cell -> Html Msg
viewCell cellSize rowNumber colNumber cell =
    let
        getBackgroundColor : Bool -> String
        getBackgroundColor isAlive =
            if isAlive then
                "#f80"

            else
                "#000"
    in
    div
        [ style "display" "inline-block"
        , style "width" cellSize
        , style "height" "100%"
        , style "border" "1px solid gray"
        , style "background-color" (getBackgroundColor cell.isAlive)
        , Html.Events.onClick <| CellClicked colNumber rowNumber
        ]
        []
