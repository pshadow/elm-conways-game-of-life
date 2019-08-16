module ConwaysGameOfLife exposing (init, subscriptions, update, view)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events
import Random exposing (Generator)
import Random.Array
import Time exposing (Posix)



-- Model


type alias Model =
    { board : Board
    , gameState : GameState
    , cellSize : String
    , timeIntervalInSec : Float
    , numberOfCol : Int
    , numberOfRow : Int
    }


type GameState
    = Started
    | Stopped
    | GameOver


type alias Board =
    Array Row


type alias Row =
    Array IsAlive


type alias IsAlive =
    Bool


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( initNumberOfRow, initNumberOfCol ) =
            ( 40, 40 )
    in
    ( { board =
            Array.repeat initNumberOfRow <| Array.repeat initNumberOfCol False
      , gameState = Stopped
      , cellSize = "14px"
      , timeIntervalInSec = 0.6
      , numberOfCol = initNumberOfCol
      , numberOfRow = initNumberOfRow
      }
    , Cmd.none
    )



-- Update


type Msg
    = StartButtonClicked
    | StopButtonClicked
    | RandomArrayReceived (Array Bool)
    | RandomButtonClicked
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

        RandomArrayReceived randomArr ->
            let
                newBoard =
                    Array.indexedMap
                        (\rowNumber row ->
                            Array.indexedMap
                                (\colNumber cell ->
                                    case Array.get (rowNumber * model.numberOfRow + colNumber) randomArr of
                                        Nothing ->
                                            True

                                        Just bool ->
                                            bool
                                )
                                row
                        )
                        model.board
            in
            ( { model | board = newBoard }
            , Cmd.none
            )

        RandomButtonClicked ->
            let
                randomBoolArray : Generator (Array Bool)
                randomBoolArray =
                    Random.Array.array
                        (model.numberOfCol * model.numberOfRow)
                        (Random.weighted ( 20, True ) [ ( 80, False ) ])
            in
            ( { model | gameState = Stopped }
            , Random.generate RandomArrayReceived randomBoolArray
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

                Just isAlive ->
                    let
                        newRow =
                            Array.set colNumber (not isAlive) row

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


getNextCell : Model -> Int -> Int -> IsAlive -> IsAlive
getNextCell model rowNumber colNumber isAlive =
    let
        livingNeighberNumber =
            getLivingNeighberNumber rowNumber colNumber model.board
    in
    if isAlive then
        if livingNeighberNumber < 2 || livingNeighberNumber > 3 then
            False

        else
            isAlive

    else if livingNeighberNumber == 3 then
        True

    else
        isAlive


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
                Just isAlive ->
                    if isAlive then
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
    Array.filter identity row
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
        , viewBotton RandomButtonClicked "Random"
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


viewCell : String -> Int -> Int -> IsAlive -> Html Msg
viewCell cellSize rowNumber colNumber isAlive =
    let
        backgroundColor =
            if isAlive then
                "#f80"

            else
                "#e9e9e9"
    in
    div
        [ style "display" "inline-block"
        , style "width" cellSize
        , style "height" "100%"
        , style "border" "1px solid gray"
        , style "background-color" backgroundColor
        , Html.Events.onClick <| CellClicked colNumber rowNumber
        ]
        []
