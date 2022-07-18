module Main exposing (main)

import Browser
import Html exposing (button, div, form, h1, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, placeholder, required, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, preventDefaultOn)
import Http
import Json.Decode as JD
import Time


type alias PersonalData =
    { email : String
    , checkbox : Bool
    }


type alias ChooseOfficeData =
    { officeInfo : Maybe OfficeInfo
    , showAsMap : Bool
    , error : Maybe String
    }


type OfficeInfo
    = OfficeInfo SelectedOffice OfficeList


type alias SelectedOffice =
    Maybe String


type alias OfficeList =
    List String


type Model
    = PersonalDataStep PersonalData
    | ChooseOfficeStep ChooseOfficeData PersonalData
    | ChooseInterviewSlot
        { email : String
        , checkbox : Bool
        , selectedOffice : String
        , slot : Maybe Time.Posix
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( PersonalDataStep <| PersonalData "" False, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = ChangeEmail String
    | ChangeCheckbox Bool
    | SelectOffice String
    | ShowMap Bool
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
            ( ChooseOfficeStep (ChooseOfficeData Nothing False Nothing) data
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
            ChooseOfficeStep { chD | officeInfo = selectOffice newOffice chD.officeInfo, error = Nothing } persD
                |> withNoSideEffect

        ( ShowMap show, ChooseOfficeStep chooseData personalData ) ->
            ChooseOfficeStep { chooseData | showAsMap = show } personalData
                |> withNoSideEffect

        ( NextStep, ChooseOfficeStep ({ officeInfo } as cd) ({ email, checkbox } as pd) ) ->
            case officeInfo of
                Just (OfficeInfo selectedOffice _) ->
                    case selectedOffice of
                        Just office ->
                            -- we forget about `showAsMap` and `error` as those props are not relevant for the next step
                            ChooseInterviewSlot
                                { email = email
                                , checkbox = checkbox
                                , selectedOffice = office
                                , slot = Nothing
                                }
                                |> withNoSideEffect

                        Nothing ->
                            ChooseOfficeStep { cd | error = Just "You must know why are you selling your soul." } pd
                                |> withNoSideEffect

                Nothing ->
                    ChooseOfficeStep { cd | error = Just "There's no valid office information yet." } pd
                        |> withNoSideEffect

        ( _, ChooseOfficeStep _ _ ) ->
            model
                |> withNoSideEffect

        ( NextStep, ChooseInterviewSlot data ) ->
            Debug.todo "send to server"

        ( _, ChooseInterviewSlot data ) ->
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
                        div []
                            [ div []
                                [ input [ id "show-as-map-check", type_ "checkbox", checked chooseData.showAsMap, onCheck ShowMap ] []
                                , label [ for "show-as-map-check" ] [ text "Show as map" ]
                                ]
                            , if chooseData.showAsMap then
                                div [] [ text "todo map" ]

                              else
                                officeList
                                    |> List.map (renderOfficeLi selectedOffice)
                                    |> ul []
                            , case chooseData.error of
                                Just error ->
                                    div [ class "error" ] [ text error ]

                                Nothing ->
                                    button [ onClick NextStep ] [ text "Save" ]
                            ]
                    )
                |> Maybe.withDefault (text "Loading office list...")

        ChooseInterviewSlot _ ->
            -- TODO
            div [] [ text "To be continued..." ]


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
