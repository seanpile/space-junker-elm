module Main exposing (..)

import Html exposing (Html, div, text, program)
import Math.Matrix4 as Matrix4
import Math.Vector3 as Vector3
import Window
import Task
import Time exposing (Time)
import AnimationFrame
import SolarSystem exposing (SolarSystem)
import View
import OrbitControls exposing (OrbitEvent)


-- MAIN


maxIterations : Int
maxIterations =
    10000


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
    { initialized : Bool
    , numTimes : Int
    , timeStep : Float
    , solarSystem : Maybe SolarSystem
    , context : Maybe View.RenderingContext
    , orbitControl : Maybe ( OrbitControls.Options, OrbitControls.State )
    }


initialModel : Model
initialModel =
    { initialized = False
    , numTimes = 0
    , timeStep = 1.0e6
    , solarSystem = Nothing
    , context = Nothing
    , orbitControl = Nothing
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Task.perform Initialize
        (Time.now
            |> Task.andThen (\time -> Task.map (\size -> ( time, size )) Window.size)
        )
    )



-- MESSAGES


type Msg
    = Initialize ( Time, Window.Size )
    | OnWindowResize Window.Size
    | OnOrbit OrbitEvent
    | Advance Time



-- VIEW


view : Model -> Html Msg
view { context, solarSystem, orbitControl } =
    case Maybe.map3 (,,) context solarSystem orbitControl of
        Nothing ->
            div []
                [ Html.text "Loading..."
                ]

        Just ( context, solarSystem, orbitControl ) ->
            div (OrbitControls.listeners (Tuple.second orbitControl) OnOrbit)
                [ View.render context solarSystem ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Initialize the scene given the current time and window size
        Initialize ( t, window ) ->
            let
                system =
                    SolarSystem.seed t

                context =
                    View.init window system

                orbitState =
                    OrbitControls.defaultState

                defaultOrbitOptions =
                    OrbitControls.defaultOptions

                orbitOptions =
                    { defaultOrbitOptions
                        | zoomSpeed = 1.0
                        , up = context.up
                        , target = context.target
                    }
            in
                ( { model
                    | solarSystem = Just system
                    , context = Just context
                    , orbitControl = Just ( orbitOptions, orbitState )
                    , initialized = True
                  }
                , Cmd.none
                )

        -- Update the scene when the window resizes
        OnWindowResize windowSize ->
            ( { model | context = Maybe.map2 View.updateWindow model.context (Just windowSize) }, Cmd.none )

        OnOrbit event ->
            case (Maybe.map2 (,) model.context model.orbitControl) of
                Nothing ->
                    ( model, Cmd.none )

                Just ( context, orbitControl ) ->
                    let
                        ( options, state ) =
                            orbitControl

                        ( updatedCamera, updatedState ) =
                            OrbitControls.applyWithOptions options event ( context.camera, state )

                        updatedContext =
                            View.updateCamera context updatedCamera
                    in
                        ( { model
                            | orbitControl = Just ( options, updatedState )
                            , context = Just updatedContext
                          }
                        , Cmd.none
                        )

        -- Update the scene every frame to provide smooth animation
        Advance dt ->
            case model.solarSystem of
                Nothing ->
                    ( model, Cmd.none )

                Just system ->
                    let
                        solarSystem =
                            SolarSystem.advance (dt * model.timeStep) system
                    in
                        ( { model
                            | numTimes = model.numTimes + 1
                            , solarSystem = Just solarSystem
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
