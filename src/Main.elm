module Main exposing (main)

import Browser
import Html exposing (button, div, form, h1, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, placeholder, required, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, preventDefaultOn)
import Http
import Json.Decode as JD


type alias PersonalData =
    { email : String
    , checkbox : Bool
    }


type alias ChooseOfficeData =
    { officeInfo : Maybe OfficeInfo }


type OfficeInfo
    = OfficeInfo SelectedOffice OfficeList


type alias SelectedOffice =
    Maybe String


type alias OfficeList =
    List String


type Model
    = PersonalDataStep PersonalData
    | ChooseOfficeStep ChooseOfficeData PersonalData


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( PersonalDataStep (PersonalData "" False), Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = ChangeEmail String
    | ChangeCheckbox Bool
    | SelectOffice String
    | NextStep
    | GotOfficeList (Result Http.Error (List String))



{- Expected office list from API:
   [ "Long Beach, LA"
   , "Long Island, NY"
   , "Greenwich, London"
   , "Saint Denis, Paris"
   ]
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangeEmail email, PersonalDataStep data ) ->
            PersonalDataStep { data | email = email }
                |> withNoSideEffect

        ( ChangeCheckbox check, PersonalDataStep data ) ->
            PersonalDataStep { data | checkbox = check }
                |> withNoSideEffect

        ( NextStep, PersonalDataStep data ) ->
            ( ChooseOfficeStep { officeInfo = Nothing } data
            , Http.get
                { url = "https://mocki.io/v1/8f3038e5-0341-4089-ac88-085c43b03ad7"
                , expect = Http.expectJson GotOfficeList officeListDecoder
                }
            )

        ( _, PersonalDataStep _ ) ->
            model
                |> withNoSideEffect

        ( GotOfficeList result, ChooseOfficeStep chooseData pd ) ->
            case result of
                Ok officeList ->
                    ChooseOfficeStep { chooseData | officeInfo = Just (OfficeInfo Nothing officeList) } pd
                        |> withNoSideEffect

                -- TODO
                Err _ ->
                    model
                        |> withNoSideEffect

        ( SelectOffice newOffice, ChooseOfficeStep chD persD ) ->
            ChooseOfficeStep { chD | officeInfo = selectOffice newOffice chD.officeInfo } persD
                |> withNoSideEffect

        ( NextStep, ChooseOfficeStep _ _ ) ->
            Debug.todo "send to server"

        ( _, ChooseOfficeStep _ _ ) ->
            model
                |> withNoSideEffect


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
            chooseData.officeInfo
                |> Maybe.map
                    (\(OfficeInfo selectedOffice officeList) ->
                        officeList
                            |> List.map (renderOfficeLi selectedOffice)
                            |> ul []
                    )
                |> Maybe.withDefault (text "Loading office list...")


selectOffice : String -> Maybe OfficeInfo -> Maybe OfficeInfo
selectOffice newSelectedOffice maybeOfficeInfo =
    case maybeOfficeInfo of
        Just (OfficeInfo _ officeList) ->
            Just <| OfficeInfo (Just newSelectedOffice) officeList

        Nothing ->
            maybeOfficeInfo


officeListDecoder : JD.Decoder (List String)
officeListDecoder =
    JD.list JD.string


renderOfficeLi maybeSelectedOffice o =
    let
        isSelected =
            case maybeSelectedOffice of
                Just selectedOffice ->
                    o == selectedOffice

                Nothing ->
                    False
    in
    li
        [ onClick (SelectOffice o)
        , classList [ ( "selected", isSelected ) ]
        ]
        [ text o ]


withNoSideEffect : a -> ( a, Cmd msg )
withNoSideEffect model =
    ( model, Cmd.none )
