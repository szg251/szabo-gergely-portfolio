module Screen.Color exposing (..)

import Css exposing (Color, rgb)


type ScreenColor
    = Black
    | DarkGray
    | Red
    | LightRed
    | Green
    | LightGreen
    | Brown
    | Yellow
    | Blue
    | LightBlue
    | Magenta
    | LightMagenta
    | Cyan
    | LightCyan
    | LightGray
    | White


toCssColor : ScreenColor -> Color
toCssColor screenColor =
    case screenColor of
        Black ->
            rgb 0 0 0

        DarkGray ->
            rgb 85 85 85

        Red ->
            rgb 170 0 0

        LightRed ->
            rgb 255 85 85

        Green ->
            rgb 0 170 0

        LightGreen ->
            rgb 85 255 85

        Brown ->
            rgb 170 85 0

        Yellow ->
            rgb 255 255 85

        Blue ->
            rgb 0 0 170

        LightBlue ->
            rgb 85 85 255

        Magenta ->
            rgb 170 0 170

        LightMagenta ->
            rgb 255 85 255

        Cyan ->
            rgb 0 170 170

        LightCyan ->
            rgb 85 255 255

        LightGray ->
            rgb 170 170 170

        White ->
            rgb 255 255 255


fromScreenColor : String -> Maybe ScreenColor
fromScreenColor colorCode =
    case colorCode of
        "0;30" ->
            Just Black

        "1;30" ->
            Just DarkGray

        "0;31" ->
            Just Red

        "1;31" ->
            Just LightRed

        "0;32" ->
            Just Green

        "1;32" ->
            Just LightGreen

        "0;33" ->
            Just Brown

        "1;33" ->
            Just Yellow

        "0;34" ->
            Just Blue

        "1;34" ->
            Just LightBlue

        "0;35" ->
            Just Magenta

        "1;35" ->
            Just LightMagenta

        "0;36" ->
            Just Cyan

        "1;36" ->
            Just LightCyan

        "0;37" ->
            Just LightGray

        "1;37" ->
            Just White

        _ ->
            Nothing
