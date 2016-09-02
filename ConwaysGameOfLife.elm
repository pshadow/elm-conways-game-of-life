module ConwaysGameOfLife exposing (..)

import Html exposing (..)
import Html.App
import Html.Attributes
import Html.Events
import Array exposing (Array)
import Time exposing (Time)


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { rows : Array Row
    , gameStarted : Bool
    , cellSize : String
    , timeIntervalInSec : Float
    }


type alias Row =
    Array Cell


type alias Cell =
    { isAlive : Bool }


( numberOfRow, numberOfCol ) =
    ( 50, 80 )
init : ( Model, Cmd Msg )
init =
    ( { rows =
            Array.repeat numberOfRow <|
                Array.repeat numberOfCol { isAlive = False }
      , gameStarted = False
      , cellSize = "12px"
      , timeIntervalInSec = 0.5
      }
    , Cmd.none
    )



-- Update


type Msg
    = ChangeGameStatus
    | ResetGame
    | UpdateCell Int Int
    | EvolveCells Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameStatus ->
            ( { model | gameStarted = not model.gameStarted }
            , Cmd.none
            )

        ResetGame ->
            init

        UpdateCell colNumber rowNumber ->
            ( changeCell rowNumber colNumber model
            , Cmd.none
            )

        EvolveCells intervel ->
            ( evolve model
            , Cmd.none
            )


changeCell : Int -> Int -> Model -> Model
changeCell rowNumber colNumber model =
    case (Array.get rowNumber model.rows) of
        Nothing ->
            model

        Just row ->
            case (Array.get colNumber row) of
                Nothing ->
                    model

                Just cell ->
                    let
                        newRow =
                            Array.set
                                colNumber
                                { cell | isAlive = not cell.isAlive }
                                row

                        newRows =
                            Array.set rowNumber newRow model.rows
                    in
                        { model | rows = newRows }


evolve : Model -> Model
evolve model =
    if not model.gameStarted then
        model
    else
        { model
            | rows = Array.indexedMap (evolveRow model) model.rows
        }


evolveRow : Model -> Int -> Row -> Row
evolveRow model rowNumber row =
    Array.indexedMap (evolveCell model rowNumber) row


evolveCell : Model -> Int -> Int -> Cell -> Cell
evolveCell model rowNumber colNumber cell =
    let
        livingNeighberNumber =
            getLivingNeighberNumber rowNumber colNumber model.rows
    in
        if cell.isAlive then
            if (livingNeighberNumber < 2 || livingNeighberNumber > 3) then
                { cell | isAlive = False }
            else
                cell
        else if livingNeighberNumber == 3 then
            { cell | isAlive = True }
        else
            cell


getLivingNeighberNumber : Int -> Int -> Array Row -> Int
getLivingNeighberNumber rowNumber colNumber rows =
    isLivingCell (rowNumber - 1) (colNumber - 1) rows
        + isLivingCell (rowNumber - 1) (colNumber) rows
        + isLivingCell (rowNumber - 1) (colNumber + 1) rows
        + isLivingCell rowNumber (colNumber - 1) rows
        + isLivingCell rowNumber (colNumber + 1) rows
        + isLivingCell (rowNumber + 1) (colNumber - 1) rows
        + isLivingCell (rowNumber + 1) (colNumber) rows
        + isLivingCell (rowNumber + 1) (colNumber + 1) rows


isLivingCell : Int -> Int -> Array Row -> Int
isLivingCell rowNumber colNumber rows =
    case (Array.get rowNumber rows) of
        Just row ->
            case (Array.get colNumber row) of
                Just cell ->
                    if cell.isAlive then
                        1
                    else
                        0

                Nothing ->
                    0

        Nothing ->
            0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (model.timeIntervalInSec * Time.second) EvolveCells



-- VIEW


view : Model -> Html Msg
view model =
    let
        getBottonText : String
        getBottonText =
            if model.gameStarted then
                "Stop"
            else
                "Start"
    in
        div []
            [ viewBotton False ChangeGameStatus getBottonText
            , viewBotton model.gameStarted ResetGame "Reset"
            , viewBoard model
            ]


viewBotton : Bool -> Msg -> String -> Html Msg
viewBotton isDisabled msg text =
    button
        [ Html.Attributes.style
            [ ( "width", "80px" )
            , ( "height", "30px" )
            , ( "margin-top", "20px" )
            , ( "margin-left", "20px" )
            ]
        , Html.Attributes.disabled isDisabled
        , Html.Events.onClick msg
        ]
        [ Html.text text ]


viewBoard : Model -> Html Msg
viewBoard model =
    div [ Html.Attributes.style [ ( "margin", "20px 20px" ) ] ]
        [ Array.indexedMap (viewRow model.cellSize) model.rows
            |> Array.toList
            |> div []
        ]


viewRow : String -> Int -> Row -> Html Msg
viewRow cellSize rowNumber row =
    Array.indexedMap (viewCell cellSize rowNumber) row
        |> Array.toList
        |> div [ Html.Attributes.style [ ( "height", cellSize ) ] ]


viewCell : String -> Int -> Int -> Cell -> Html Msg
viewCell cellSize rowNumber colNumber cell =
    div
        [ Html.Attributes.style
            [ ( "display", "inline-block" )
            , ( "width", cellSize )
            , ( "height", "100%" )
            , ( "border", "1px solid gray" )
            , ( "background-color"
              , if cell.isAlive then
                    "#ff8800"
                else
                    "#ffffff"
              )
            ]
        , Html.Events.onClick <| UpdateCell colNumber rowNumber
        ]
        []
