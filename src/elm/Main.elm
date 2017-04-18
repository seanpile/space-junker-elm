module Main exposing (..)

import Html exposing (Html, div, text, program)
import Task
import Time exposing (Time)
import AnimationFrame exposing (diffs)
import SolarSystem exposing (SolarSystem, seed, advance)


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { solarSystem : Maybe SolarSystem }


init : ( Model, Cmd Msg )
init =
    ( { solarSystem = Nothing }, Task.perform Initialize Time.now )



-- MESSAGES


type Msg
    = Initialize Time
    | Frame Time



-- VIEW


view : Model -> Html Msg
view model =
    case model.solarSystem of
        Just system ->
            let
                earth =
                    SolarSystem.find "earth" system
            in
                div []
                    [ text (toString earth) ]

        Nothing ->
            div []
                [ text "Not finished loading yet!" ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize t ->
            let
                system =
                    seed t
            in
                ( { solarSystem = Just system }, Cmd.none )

        Frame dx ->
            case model.solarSystem of
                Just system ->
                    ( { model | solarSystem = Just (advance dx system) }, Cmd.none )

                Nothing ->
                    Debug.crash ("Shouldn't happen since we only listen after initialization")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.solarSystem of
        -- Start the animation frame after the solarSystem has been initialized
        Just system ->
            diffs Frame

        Nothing ->
            Sub.none
