module Screen exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


type alias Model =
    { outputBuffer : List String
    , visibleOutput : List Line
    , counter : Int
    , cursorVisible : Bool
    , cursorPosition : CursorPosition
    }


type Line
    = Line (List Char)


lineMap : (List Char -> List Char) -> Line -> Line
lineMap fn (Line line) =
    Line (fn line)


type alias CursorPosition =
    ( Int, Int )


init : List String -> Model
init outputBuffer =
    { outputBuffer = outputBuffer
    , visibleOutput = [ Line [] ]
    , counter = 0
    , cursorVisible = True
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

            firstLn :: rest ->
                let
                    lnCount =
                        List.length model.visibleOutput - 1

                    colCount =
                        case List.head model.visibleOutput of
                            Just (Line cols) ->
                                List.length cols

                            Nothing ->
                                0
                in
                case String.uncons firstLn of
                    Just ( char, lnRest ) ->
                        { model
                            | visibleOutput = appendChar char model.visibleOutput
                            , outputBuffer = lnRest :: rest
                            , cursorPosition = ( lnCount, colCount + 1 )
                        }

                    Nothing ->
                        { model
                            | visibleOutput = Line [] :: model.visibleOutput
                            , outputBuffer = rest
                            , cursorPosition = ( lnCount + 1, 0 )
                        }


appendChar : Char -> List Line -> List Line
appendChar char visibleOutput =
    let
        split lst =
            Maybe.map2 Tuple.pair (List.head lst) (List.tail lst)
    in
    case visibleOutput of
        (Line lastLn) :: firstLns ->
            Line (char :: lastLn)
                :: firstLns

        _ ->
            [ Line [ char ] ]


view : Model -> Html msg
view model =
    let
        lns =
            List.reverse model.visibleOutput
                |> List.map (lineMap ((::) ' ' >> List.reverse))
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
                (viewLn model)
                lns
            )
        )


viewLn : Model -> Int -> Line -> Element msg
viewLn model ln (Line cols) =
    paragraph []
        (List.indexedMap (viewCol model ln) cols)


viewCol : Model -> Int -> Int -> Char -> Element msg
viewCol { cursorPosition, cursorVisible } ln col char =
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
