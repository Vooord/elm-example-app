module Main exposing (main)

import Browser
import Html exposing (button, div, form, h1, input, label, text)
import Html.Attributes exposing (checked, class, for, id, placeholder, required, type_, value)


type alias Model =
    { email : String
    , checkbox : Bool
    }


main : Program () Model msg
main =
    Browser.sandbox
        { init =
            { email = ""
            , checkbox = False
            }
        , update = update
        , view = view
        }


update msg model =
    model


view : Model -> Html.Html msg
view model =
    form [ class "form" ]
        [ h1 [] [ text "Do you wanna work in FAANG?" ]
        , input [ placeholder "email", value model.email ] []
        , div []
            [ input [ id "check", type_ "checkbox", required True, checked model.checkbox ] []
            , label [ for "check" ] [ text "I agree to sell my soul." ]
            ]
        , button [] [ text "Submit" ]
        ]
