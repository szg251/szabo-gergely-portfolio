module Screen exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (newPassword)
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra as ListE


type alias Model =
    { visibleOutput : VisibleOutput
    , command : Command
    , counter : Int
    , cursorVisible : Bool
    , cursorPosition : CursorPosition
    }


type Command
    = NoCommand
    | Print (Block String)
    | MoveCursor CursorPosition
    | EndOfLine
    | Batch (List Command)


type alias VisibleOutput =
    List (Line (List Char))


type Line a
    = Line (List (Block a))


type Block a
    = NormalBlock a
    | Colored ( Color, a )
    | Link ( String, a )
    | EmptyBlock


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

        EmptyBlock ->
            EmptyBlock


splitBlockAt : Int -> Block (List Char) -> ( Block (List Char), Block (List Char) )
splitBlockAt index block =
    case block of
        NormalBlock xss ->
            let
                ( x, xs ) =
                    ListE.splitAt index xss
            in
            ( NormalBlock x, NormalBlock xs )

        Colored ( color, xss ) ->
            let
                ( x, xs ) =
                    ListE.splitAt index xss
            in
            ( Colored ( color, x ), Colored ( color, xs ) )

        Link ( href, xss ) ->
            let
                ( x, xs ) =
                    ListE.splitAt index xss
            in
            ( Link ( href, x ), Link ( href, xs ) )

        EmptyBlock ->
            ( EmptyBlock, EmptyBlock )


isEmptyBlock : Block (List Char) -> Bool
isEmptyBlock block =
    case block of
        NormalBlock xss ->
            List.isEmpty xss

        Colored ( color, xss ) ->
            List.isEmpty xss

        Link ( href, xss ) ->
            List.isEmpty xss

        EmptyBlock ->
            True


mergeBlocks : Block (List Char) -> Block (List Char) -> List (Block (List Char))
mergeBlocks blockA blockB =
    case ( blockA, blockB ) of
        ( NormalBlock a, NormalBlock b ) ->
            [ NormalBlock (a ++ b) ]

        ( Colored ( colorA, a ), Colored ( colorB, b ) ) ->
            if colorA == colorB then
                [ Colored ( colorA, a ++ b ) ]

            else
                [ blockA, blockB ]

        ( Link ( hrefA, a ), Link ( hrefB, b ) ) ->
            if hrefA == hrefB then
                [ Link ( hrefA, a ++ b ) ]

            else
                [ blockA, blockB ]

        ( a, b ) ->
            [ blockA, blockB ]


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

        EmptyBlock ->
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

        EmptyBlock ->
            0


type alias CursorPosition =
    ( Int, Int )


init : Command -> Model
init command =
    { command = command
    , visibleOutput = [ Line [] ]
    , counter = 0
    , cursorVisible = True
    , cursorPosition = ( 0, 0 )
    }


tick : Model -> Model
tick =
    count >> blinkCursor >> evalCommand


count : Model -> Model
count model =
    { model | counter = model.counter + 1 }


blinkCursor : Model -> Model
blinkCursor model =
    if modBy 30 model.counter /= 0 then
        model

    else
        { model | cursorVisible = not model.cursorVisible }


evalCommand : Model -> Model
evalCommand model =
    case model.command of
        NoCommand ->
            model

        EndOfLine ->
            evalCommand
                { model
                    | visibleOutput = Line [] :: model.visibleOutput
                    , command = NoCommand
                    , cursorPosition =
                        ( Tuple.first model.cursorPosition + 1, 0 )
                }

        Print block ->
            case unconsBlock block of
                Just ( firstChar, lastChars ) ->
                    { model
                        | visibleOutput =
                            insertChar model.cursorPosition
                                firstChar
                                model.visibleOutput
                        , command = Print lastChars
                        , cursorPosition =
                            ( Tuple.first model.cursorPosition
                            , Tuple.second model.cursorPosition + 1
                            )
                    }

                Nothing ->
                    { model | command = NoCommand }

        MoveCursor newPosition ->
            { model
                | command = NoCommand
                , cursorPosition = newPosition
            }

        Batch commands ->
            case commands of
                [] ->
                    { model | command = NoCommand }

                firstCommand :: restCommands ->
                    let
                        evaled =
                            evalCommand
                                { model | command = firstCommand }
                    in
                    { evaled
                        | command =
                            if evaled.command == NoCommand then
                                Batch restCommands

                            else
                                Batch (evaled.command :: restCommands)
                    }


getLastPosition : VisibleOutput -> CursorPosition
getLastPosition visibleOutput =
    ( List.length visibleOutput - 1
    , case List.head visibleOutput of
        Just (Line blocks) ->
            List.map blockLength blocks
                |> List.foldl (+) 0

        Nothing ->
            0
    )


insertChar : CursorPosition -> Block Char -> VisibleOutput -> VisibleOutput
insertChar cursorPosition charBlock visibleOutput =
    let
        reverseLnIndex =
            List.length visibleOutput - Tuple.first cursorPosition - 1

        ( bottomLns, upperLns ) =
            ListE.splitAt reverseLnIndex visibleOutput
                |> Tuple.mapSecond ((++) (List.repeat -reverseLnIndex (Line [])))
    in
    case upperLns of
        (Line lastLn) :: restLns ->
            let
                ( length, blocks ) =
                    ListE.mapAccuml
                        (\colMin nextBlock ->
                            let
                                colMax =
                                    colMin + blockLength nextBlock
                            in
                            ( colMax, ( colMin, colMax, nextBlock ) )
                        )
                        0
                        lastLn

                reverseColIndex =
                    length - Tuple.second cursorPosition

                ( rightBlocksWithCol, leftBlocksWithCol ) =
                    blocks
                        |> ListE.splitWhen
                            (\( colMin, colMax, _ ) ->
                                colMin <= reverseColIndex && reverseColIndex <= colMax
                            )
                        |> Maybe.withDefault
                            ( []
                            , ( length
                              , length + -reverseColIndex
                              , NormalBlock (List.repeat -reverseColIndex ' ')
                              )
                                :: blocks
                            )

                updatedLn =
                    let
                        rightBlocks =
                            List.map (\( _, _, block ) -> block) rightBlocksWithCol

                        updatedLeftBlocks =
                            case leftBlocksWithCol of
                                ( colMin, _, lastBlock ) :: firstBlocksTuples ->
                                    let
                                        ( charsRight, charsLeft ) =
                                            splitBlockAt (reverseColIndex - colMin) lastBlock
                                    in
                                    charsRight
                                        :: mapBlock List.singleton charBlock
                                        :: charsLeft
                                        :: List.map (\( _, _, block ) -> block) firstBlocksTuples

                                [] ->
                                    [ mapBlock List.singleton charBlock ]
                    in
                    (rightBlocks ++ updatedLeftBlocks)
                        |> List.filter (not << isEmptyBlock)
                        |> List.foldr
                            (\next prevBlocks ->
                                case prevBlocks of
                                    head :: tail ->
                                        mergeBlocks next head ++ tail

                                    [] ->
                                        next :: []
                            )
                            []
            in
            bottomLns ++ Line updatedLn :: restLns

        _ ->
            [ Line [ mapBlock List.singleton charBlock ] ]


view : Model -> Html msg
view model =
    let
        lns =
            List.reverse model.visibleOutput
                |> List.map (mapLine ((::) EmptyBlock >> List.map (mapBlock List.reverse) >> List.reverse))
    in
    layout []
        (column
            [ width fill
            , height fill
            , Background.color (rgb 0 0 0)
            , Font.color (rgb 0.7 0.7 0.7)
            , paddingXY 20 20
            , Font.family
                [ Font.typeface "unscii-16"
                , Font.monospace
                ]
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

        EmptyBlock ->
            viewCol model ln prevCol ' '


viewCol : Model -> Int -> Int -> Char -> ( Int, Element msg )
viewCol { cursorPosition, cursorVisible } ln prevCol char =
    ( prevCol + 1
    , el
        ([ width (px 10)
         , height (px 18)
         ]
            ++ (if cursorPosition == ( ln, prevCol ) && cursorVisible then
                    [ Background.color (rgb 0 1 1) ]

                else
                    []
               )
        )
        (text (String.fromChar char))
    )


print : String -> Command
print string =
    Print (NormalBlock string)


printColored : Color -> String -> Command
printColored color string =
    Print (Colored ( color, string ))


printLink : String -> String -> Command
printLink href string =
    Print (Link ( href, string ))


printLn : String -> Command
printLn string =
    Batch [ Print (NormalBlock string), EndOfLine ]


printColoredLn : Color -> String -> Command
printColoredLn color string =
    Batch [ Print (Colored ( color, string )), EndOfLine ]


printLinkLn : String -> String -> Command
printLinkLn href string =
    Batch [ Print (Link ( href, string )), EndOfLine ]


moveCursor : CursorPosition -> Command
moveCursor =
    MoveCursor


batch : List Command -> Command
batch =
    Batch


none : Command
none =
    NoCommand
