module Main exposing (..)

import Html.App
import ConwaysGameOfLife


main : Program Never
main =
    Html.App.program
        { init = ConwaysGameOfLife.init
        , view = ConwaysGameOfLife.view
        , update = ConwaysGameOfLife.update
        , subscriptions = ConwaysGameOfLife.subscriptions
        }
