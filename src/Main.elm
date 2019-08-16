module Main exposing (main)

import Browser
import ConwaysGameOfLife


main =
    Browser.element
        { init = ConwaysGameOfLife.init
        , view = ConwaysGameOfLife.view
        , update = ConwaysGameOfLife.update
        , subscriptions = ConwaysGameOfLife.subscriptions
        }
