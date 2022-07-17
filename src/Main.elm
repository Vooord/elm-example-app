module Main exposing (main)

import Browser
import Html exposing (button, div, form, h1, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, placeholder, required, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, preventDefaultOn)
import Json.Decode as JD


type alias PersonalData =
    { email : String
    , checkbox : Bool
    }


type alias ChooseOfficeData =
    { selectedOffice : Maybe String }


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
    | SelectOffice String
    | NextStep


offices =
    [ "Long Beach, LA"
    , "Long Island, NY"
    , "Greenwich, London"
    , "Saint Denis, Paris"
    ]


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( ChangeEmail email, PersonalDataStep data ) ->
            PersonalDataStep { data | email = email }

        ( ChangeCheckbox check, PersonalDataStep data ) ->
            PersonalDataStep { data | checkbox = check }

        ( NextStep, PersonalDataStep data ) ->
            ChooseOfficeStep { selectedOffice = Nothing } data

        ( _, PersonalDataStep _ ) ->
            model

        ( SelectOffice o, ChooseOfficeStep chooseData personalData ) ->
            ChooseOfficeStep { chooseData | selectedOffice = Just o } personalData

        ( NextStep, ChooseOfficeStep _ _ ) ->
            Debug.todo "send to server"

        ( _, ChooseOfficeStep _ _ ) ->
            model


view : Model -> Html.Html Msg
view model =
    case model of
        -- so you can destruct a record and use it as-is in the same time
        PersonalDataStep ({ email, checkbox } as data) ->
            form [ class "form", preventDefaultOn "submit" (JD.succeed ( NextStep, True )) ]
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

        ChooseOfficeStep chooseData _ ->
            ul []
                (List.map
                    (\o ->
                        li
                            [ onClick (SelectOffice o)
                            , classList
                                [ ( "selected"
                                  , case chooseData.selectedOffice of
                                        Just selectedOffice ->
                                            o == selectedOffice

                                        Nothing ->
                                            False
                                  )
                                ]
                            ]
                            [ text o ]
                    )
                    offices
                )
