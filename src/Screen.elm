module Screen exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


type alias Model =
    { outputBuffer : List String
    , visibleOutput : List (List Char)
    , counter : Int
    , cursorVisible : Bool
    , cursorPosition : CursorPosition
    }


type alias CursorPosition =
    ( Int, Int )


init : List String -> Model
init outputBuffer =
    { outputBuffer = outputBuffer
    , visibleOutput = []
    , counter = 0
    , cursorVisible = False
    , cursorPosition = ( 0, 0 )
    }


tick : Model -> Model
tick =
    count >> blinkCursor >> printNext


count : Model -> Model
count model =
    { model | counter = model.counter + 1 }


blinkCursor : Model -> Model
blinkCursor model =
    if modBy 30 model.counter /= 0 then
        model

    else
        { model | cursorVisible = not model.cursorVisible }


printNext : Model -> Model
printNext model =
    if modBy 8 model.counter /= 0 then
        model

    else
        case model.outputBuffer of
            [] ->
                model

            firstRow :: rest ->
                case String.uncons firstRow of
                    Just ( char, rowRest ) ->
                        { model
                            | visibleOutput = appendChar char model.visibleOutput
                            , outputBuffer = rowRest :: rest
                            , cursorPosition = (\( ln, col ) -> ( ln, col + 1 )) model.cursorPosition
                        }

                    Nothing ->
                        { model
                            | visibleOutput = [] :: model.visibleOutput
                            , outputBuffer = rest
                            , cursorPosition = (\( ln, _ ) -> ( ln + 1, 0 )) model.cursorPosition
                        }


appendChar : Char -> List (List Char) -> List (List Char)
appendChar char visibleOutput =
    let
        split lst =
            Maybe.map2 Tuple.pair (List.tail lst) (List.head lst)
    in
    case split visibleOutput of
        Nothing ->
            [ [ char ] ]

        Just ( firstRows, lastRow ) ->
            (char :: lastRow) :: firstRows


view : Model -> Html msg
view model =
    let
        rows =
            List.reverse model.visibleOutput
                |> List.map ((::) ' ')
                |> List.map List.reverse
    in
    layout []
        (column
            [ width fill
            , height fill
            , Background.color (rgb 0 0 0)
            , Font.color (rgb 0.7 0.7 0.7)
            , paddingXY 20 20
            , Font.family [ Font.monospace ]
            ]
            (List.indexedMap
                (viewRow model)
                rows
            )
        )


viewRow : Model -> Int -> List Char -> Element msg
viewRow model ln chars =
    paragraph []
        (List.indexedMap (viewChar model ln) chars)


viewChar : Model -> Int -> Int -> Char -> Element msg
viewChar { cursorPosition, cursorVisible } ln col char =
    el
        ([ width (px 15)
         , height (px 20)
         ]
            ++ (if cursorPosition == ( ln, col ) && cursorVisible then
                    [ Background.color (rgb 0 1 1) ]

                else
                    []
               )
        )
        (text (String.fromChar char))


viewCursor : Bool -> Element msg
viewCursor cursorVisible =
    el
        (if cursorVisible then
            [ Background.color (rgb 0 1 1)
            , width (px 15)
            , height (px 20)
            ]

         else
            []
        )
        Element.none
