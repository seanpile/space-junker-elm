module Main exposing (..)

import Html exposing (Html, div, text, program)
import Window
import Task
import Time exposing (Time)
import AnimationFrame
import SolarSystem exposing (SolarSystem, seed, advance)
import View


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


maxIterations : Int
maxIterations =
    1000



-- MODEL


type alias Model =
    { initialized : Bool
    , numTimes : Int
    , timeStep : Float
    , window : Window.Size
    , solarSystem : SolarSystem
    }


initialModel : Model
initialModel =
    { initialized = False
    , numTimes = 0
    , timeStep = 1.0e6
    , window = { width = 0, height = 0 }
    , solarSystem = SolarSystem.seed 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch
        [ Task.perform Initialize Time.now
        , Task.perform OnWindowResize Window.size
        ]
    )



-- MESSAGES


type Msg
    = Initialize Time
    | OnWindowResize Window.Size
    | Advance Time



-- VIEW


view : Model -> Html Msg
view { window, solarSystem } =
    View.render window solarSystem



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize t ->
            ( { model | solarSystem = seed t, initialized = True }, Cmd.none )

        OnWindowResize windowSize ->
            ( { model | window = windowSize }, Cmd.none )

        Advance dt ->
            let
                solarSystem =
                    advance (dt * model.timeStep) model.solarSystem
            in
                ( { model
                    | numTimes = model.numTimes + 1
                    , solarSystem = solarSystem
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.initialized && model.numTimes < maxIterations then
        Sub.batch
            [ AnimationFrame.diffs Advance
            , Window.resizes OnWindowResize
            ]
    else
        Sub.batch
            [ Window.resizes OnWindowResize ]
