module BallColor
    exposing
        ( Color
        , name
        , cssColor
        , default
        , generator
        )

import Color
import String
import Random
import Array exposing (Array)


type Color
    = Blue
    | Red
    | Green
    | Pink
    | Black
    | Yellow
    | Purple
    | Orange
    | White


name : Color -> String
name color =
    toString color |> String.toLower


toElmColor : Color -> Color.Color
toElmColor color =
    case color of
        Blue ->
            Color.blue

        Red ->
            Color.red

        Green ->
            Color.green

        Pink ->
            Color.lightPurple

        Black ->
            Color.black

        Yellow ->
            Color.yellow

        Purple ->
            Color.darkPurple

        Orange ->
            Color.orange

        White ->
            Color.white


cssColor : Color -> String
cssColor color =
    let
        rgb =
            Color.toRgb (toElmColor color)

        components =
            [ rgb.red, rgb.green, rgb.blue ]
    in
        "rgb(" ++ (String.join "," (List.map toString components)) ++ ")"


generator : Random.Generator Color
generator =
    Random.map
        (\i -> Maybe.withDefault default (Array.get i colors))
        (Random.int 0 ((Array.length colors) - 1))


colors : Array Color
colors =
    Array.fromList
        [ Pink
        , Blue
        , Red
        , Green
        , Black
        , Yellow
        , Purple
        , Orange
        ]


default : Color
default =
    White



-- constructors


pink =
    Pink


blue =
    Blue


red =
    Red


green =
    Green


black =
    Black


yellow =
    Yellow


purple =
    Purple


orange =
    Orange
