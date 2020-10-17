module Screen exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra as ListE


type alias Model =
    { outputBuffer : List (Line String)
    , visibleOutput : List (Line (List Char))
    , counter : Int
    , cursorVisible : Bool
    , cursorPosition : CursorPosition
    }


type Line a
    = Line (List (Block a))


type Block a
    = NormalBlock a
    | Colored ( Color, a )
    | Link ( String, a )
    | EndOfLine


mapLine : (List (Block a) -> List (Block b)) -> Line a -> Line b
mapLine fn (Line line) =
    Line (fn line)


mapBlock : (a -> b) -> Block a -> Block b
mapBlock fn block =
    case block of
        NormalBlock y ->
            NormalBlock (fn y)

        Colored ( x, y ) ->
            Colored ( x, fn y )

        Link ( x, y ) ->
            Link ( x, fn y )

        EndOfLine ->
            EndOfLine


unconsBlock : Block String -> Maybe ( Block Char, Block String )
unconsBlock block =
    case mapBlock String.uncons block of
        NormalBlock (Just ( y, z )) ->
            Just ( NormalBlock y, NormalBlock z )

        Colored ( x, Just ( y, z ) ) ->
            Just ( Colored ( x, y ), Colored ( x, z ) )

        Link ( x, Just ( y, z ) ) ->
            Just ( Link ( x, y ), Link ( x, z ) )

        NormalBlock Nothing ->
            Nothing

        Colored ( x, Nothing ) ->
            Nothing

        Link ( x, Nothing ) ->
            Nothing

        EndOfLine ->
            Nothing


blockLength : Block (List Char) -> Int
blockLength block =
    case block of
        NormalBlock x ->
            List.length x

        Colored ( _, x ) ->
            List.length x

        Link ( _, x ) ->
            List.length x

        EndOfLine ->
            0


type alias CursorPosition =
    ( Int, Int )


init : List (Line String) -> Model
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

            (Line firstLn) :: lastLns ->
                let
                    ( lnCount, colCount ) =
                        getLastPosition model.visibleOutput
                in
                case firstLn of
                    [] ->
                        printNext
                            { model
                                | visibleOutput = Line [] :: model.visibleOutput
                                , outputBuffer = lastLns
                                , cursorPosition = ( lnCount + 1, 0 )
                            }

                    firstBlock :: lastBlocks ->
                        case unconsBlock firstBlock of
                            Just ( firstChar, lastChars ) ->
                                { model
                                    | visibleOutput = appendChar firstChar model.visibleOutput
                                    , outputBuffer = Line (lastChars :: lastBlocks) :: lastLns
                                    , cursorPosition = ( lnCount, colCount + 1 )
                                }

                            Nothing ->
                                printNext
                                    { model | outputBuffer = Line lastBlocks :: lastLns }


getLastPosition : List (Line (List Char)) -> CursorPosition
getLastPosition visibleOutput =
    ( List.length visibleOutput - 1
    , case List.head visibleOutput of
        Just (Line blocks) ->
            List.map blockLength blocks
                |> List.foldl (+) 0

        Nothing ->
            0
    )


appendChar : Block Char -> List (Line (List Char)) -> List (Line (List Char))
appendChar charBlock visibleOutput =
    case visibleOutput of
        (Line lastLn) :: restLns ->
            case lastLn of
                lastBlock :: firstBlocks ->
                    case ( lastBlock, charBlock ) of
                        ( NormalBlock w1, NormalBlock char ) ->
                            Line (NormalBlock (char :: w1) :: firstBlocks) :: restLns

                        ( Colored ( color1, chars ), Colored ( color2, char ) ) ->
                            if color1 == color2 then
                                Line (Colored ( color1, char :: chars ) :: firstBlocks) :: restLns

                            else
                                Line
                                    (Colored ( color2, chars )
                                        :: Colored ( color1, List.singleton char )
                                        :: firstBlocks
                                    )
                                    :: restLns

                        ( Link ( href1, chars ), Link ( href2, char ) ) ->
                            if href1 == href2 then
                                Line (Link ( href1, char :: chars ) :: firstBlocks) :: restLns

                            else
                                Line
                                    (Link ( href2, chars )
                                        :: Link ( href1, List.singleton char )
                                        :: firstBlocks
                                    )
                                    :: restLns

                        ( block, char ) ->
                            Line (mapBlock List.singleton char :: block :: firstBlocks) :: restLns

                [] ->
                    Line [ mapBlock List.singleton charBlock ] :: restLns

        _ ->
            [ Line [ mapBlock List.singleton charBlock ] ]


view : Model -> Html msg
view model =
    let
        lns =
            List.reverse model.visibleOutput
                |> List.map (mapLine ((::) EndOfLine >> List.map (mapBlock List.reverse) >> List.reverse))
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


viewLn : Model -> Int -> Line (List Char) -> Element msg
viewLn model ln (Line cols) =
    let
        ( _, elements ) =
            ListE.mapAccuml (viewBlock model ln) 0 cols
    in
    paragraph [] elements


viewBlock : Model -> Int -> Int -> Block (List Char) -> ( Int, Element msg )
viewBlock model ln prevCol block =
    case block of
        NormalBlock cols ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , row [] elements
            )

        Colored ( color, cols ) ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , row [ Font.color color ] elements
            )

        Link ( href, cols ) ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , newTabLink
                [ htmlAttribute (style "display" "inline-block")
                , mouseOver
                    [ Font.color (rgb 0 0 1)
                    , Background.color (rgb 0 0.7 0.7)
                    ]
                ]
                { label = row [] elements
                , url = href
                }
            )

        EndOfLine ->
            viewCol model ln prevCol ' '


viewCol : Model -> Int -> Int -> Char -> ( Int, Element msg )
viewCol { cursorPosition, cursorVisible } ln prevCol char =
    ( prevCol + 1
    , el
        ([ width (px 15)
         , height (px 20)
         ]
            ++ (if cursorPosition == ( ln, prevCol ) then
                    [ Background.color (rgb 0 1 1) ]

                else
                    []
               )
        )
        (text (String.fromChar char))
    )
