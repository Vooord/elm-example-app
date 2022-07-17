module Main exposing (main)

import Browser
import Html exposing (button, div, form, h1, input, label, text)
import Html.Attributes exposing (checked, class, for, id, placeholder, required, type_, value)
import Html.Events exposing (onCheck, onInput)


type alias PersonalData =
    { email : String
    , checkbox : Bool
    }


type alias Model =
    PersonalData


main : Program () Model Msg
main =
    Browser.sandbox
        { init = PersonalData "" False
        , update = update
        , view = view
        }


type Msg
    = ChangeEmail String
    | ChangeCheckbox Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeEmail email ->
            { model | email = email }

        ChangeCheckbox check ->
            { model | checkbox = check }


view : Model -> Html.Html Msg
view model =
    form [ class "form" ]
        [ h1 [] [ text "Do you wanna work in FAANG?" ]
        , input [ placeholder "email", onInput ChangeEmail, value model.email ] []
        , div []
            [ input
                [ id "check"
                , type_ "checkbox"
                , required True
                , onCheck ChangeCheckbox
                , checked model.checkbox
                ]
                []
            , label [ for "check" ] [ text "I agree to sell my soul." ]
            ]
        , button [] [ text "Submit" ]
        ]
