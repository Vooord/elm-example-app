module Main exposing (main)

import Browser
import Html exposing (text)


type alias Model =
    {}


main : Program () Model msg
main =
    Browser.sandbox
        { init = {}
        , update = update
        , view = view
        }


update msg model =
    model


view model =
    text "hello!"
