module Main exposing (main)


import Html.App
import Html exposing (Html)
import Html.Attributes exposing (style, class)
import Color exposing (Color)
import Time exposing (Time)
import Keyboard
import Key
import String
import Random
import Array exposing (Array)


main : Program Never
main =
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL

type Model
    = Waiting
    | Playing PlayModel
    | Ended Score


type alias Score = Int


type alias PlayModel =
    { remaining : Time
    , color : Color
    , score : Score
    }



init : ( Model, Cmd Msg )
init =
    ( Waiting, Cmd.none )


start : ( Model, Cmd Msg )
start =
    ( Playing
        { remaining = totalTime
        , color = defaultColor
        , score = 0
        }
    , generateColor
    )


timeInterval : Time
timeInterval =
    Time.second


totalTime : Time
totalTime =
    3 * Time.minute


defaultColor : Color
defaultColor =
    Color.white


colors : Array Color
colors =
    Array.fromList
        [ Color.blue
        , Color.red
        , Color.green
        , Color.lightPurple
        , Color.black
        , Color.yellow
        , Color.darkPurple
        , Color.orange
        ]



-- UPDATE

type Msg
    = Tick Time
    | KeyUp Keyboard.KeyCode
    | NewColor Color


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        KeyUp keyCode ->
            keyUp keyCode model

        Tick time ->
            ( tick time model, Cmd.none )

        NewColor color ->
            ( changeColor color model, Cmd.none )


keyUp : Keyboard.KeyCode -> Model -> ( Model, Cmd Msg)
keyUp keyCode model =
    case Key.fromCode keyCode of
        Key.Space ->
            case model of
                Waiting ->
                    start

                Playing submodel ->
                    ( Playing
                        { submodel
                        | score = submodel.score + 1
                        }
                    , generateColor
                    )

                Ended _ ->
                    ( Waiting, Cmd.none )

        _ ->
            ( model, Cmd.none )


colorGenerator : Random.Generator Color
colorGenerator =
    Random.map
        (\i -> Maybe.withDefault defaultColor (Array.get i colors))
        (Random.int 0 ((Array.length colors) - 1))


generateColor : Cmd Msg
generateColor =
    Random.generate NewColor colorGenerator



changeColor : Color -> Model -> Model
changeColor color model =
    case model of
        Playing submodel ->
            Playing { submodel | color = color }

        _ ->
            model


tick : Time -> Model -> Model
tick time model =
    case model of
        Playing submodel ->
            if submodel.remaining > 0 then
                Playing
                    { submodel
                    | remaining = submodel.remaining - timeInterval
                    }

            else
                Ended submodel.score

        Waiting ->
            model

        Ended _ ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ class "container" ]
        [ viewCurrentScreen model ]


viewCurrentScreen : Model -> Html msg
viewCurrentScreen model =
    case model of
        Waiting ->
            viewStartScreen

        Playing submodel ->
            viewProgressScreen submodel

        Ended score ->
            viewFinalScore score


viewStartScreen : Html msg
viewStartScreen =
    Html.div
        [ class "welcome-screen" ]
        [ Html.text "Press the button to start" ]


viewProgressScreen : PlayModel -> Html msg
viewProgressScreen model =
    Html.div
        [ class "progress-screen" ]
        [ viewTime model.remaining
        , viewColor model.color
        , viewScore model.score
        ]


viewTime : Time -> Html msg
viewTime time =
    Html.div
        [ class "timer"
        ]
        [ minutesAndSeconds time
            |> Html.text
        ]


minutesAndSeconds : Time -> String
minutesAndSeconds time =
    let
        raw = round (Time.inSeconds time)
        minutes = raw // 60 |> toString
        seconds = rem raw 60 |> toString
    in
        [ minutes, seconds ]
            |> List.map (String.padLeft 2 '0')
            |> String.join ":"


viewColor : Color -> Html msg
viewColor color =
    Html.div
        [ class "current-color"
        ,  style
            [ ("background-color", cssColor color)
            ]
        ]
        []


viewScore : Score -> Html msg
viewScore score =
    Html.div
        [ class "score" ]
        [ Html.text ("Score: " ++ toString score) ]



cssColor : Color -> String
cssColor color =
    let
        rgb = Color.toRgb color
        components = [rgb.red, rgb.green, rgb.blue]
    in
        "rgb(" ++ (String.join "," (List.map toString components)) ++ ")"


viewFinalScore : Score -> Html msg
viewFinalScore score =
    Html.div
        [ class "score-screen" ]
        [ Html.div
            []
            [ Html.text "Final score" ]
        , Html.div
            [ class "final-score" ]
            [ Html.text (toString score) ]
        , Html.div
            [ class "restart-hint" ]
            [ Html.text "Press the button to play again" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        -- TODO: throttle ?
        [ Keyboard.ups KeyUp
        , Time.every timeInterval Tick
        ]

