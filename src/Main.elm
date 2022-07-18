module Main exposing (main)

import Browser
import Html exposing (button, div, form, h1, input, label, text)
import Html.Attributes exposing (checked, class, for, id, placeholder, required, type_, value)
import Html.Events exposing (onCheck, onInput)


type alias PersonalData =
    { email : String
    , checkbox : Bool
    }


type alias ChooseOfficeData =
    { selectedOffice : String }


type Model
    = PersonalDataStep PersonalData
    | ChooseOfficeStep ChooseOfficeData PersonalData


main : Program () Model Msg
main =
    Browser.sandbox
        { init = PersonalDataStep (PersonalData "" False)
        , update = update
        , view = view
        }


type Msg
    = ChangeEmail String
    | ChangeCheckbox Bool


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( ChangeEmail email, PersonalDataStep data ) ->
            PersonalDataStep { data | email = email }

        ( ChangeCheckbox check, PersonalDataStep data ) ->
            PersonalDataStep { data | checkbox = check }

        ( _, ChooseOfficeStep _ _ ) ->
            model


view : Model -> Html.Html Msg
view model =
    case model of
        -- so you can destruct a record and use it as-is in the same time
        PersonalDataStep ({ email, checkbox } as data) ->
            form [ class "form" ]
                [ h1 [] [ text "Do you wanna work in FAANG?" ]
                , input [ placeholder "email", onInput ChangeEmail, value email ] []
                , div []
                    [ input
                        [ id "check"
                        , type_ "checkbox"
                        , required True
                        , onCheck ChangeCheckbox
                        , checked checkbox
                        ]
                        []
                    , label [ for "check" ] [ text "I agree to sell my soul." ]
                    ]
                , button [] [ text "Submit" ]
                ]

        ChooseOfficeStep chooseData personalData ->
            Debug.todo "choose office step"
