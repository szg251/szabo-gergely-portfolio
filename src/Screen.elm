module Screen exposing (..)

import Css exposing (..)
import Css.Global as Global exposing (body, global)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, style)
import List.Extra as ListE
import Svg.Attributes exposing (visibility)


type alias Model =
    { visibleOutput : VisibleOutput
    , command : ScreenCommand
    , counter : Int
    , cursorVisible : Bool
    , cursorPosition : CursorPosition
    }


type ScreenCommand
    = NoCommand
    | Print (Block String)
    | Delete
    | ClearScreen
    | MoveCursor (CursorPosition -> CursorPosition)
    | LineBreak
    | Batch (List ScreenCommand)


type alias VisibleOutput =
    List Line


type Line
    = Line (List (Block (List Char)))


type Block a
    = NormalBlock a
    | Colored ( Color, a )
    | Link ( String, a )
    | EmptyBlock


mapLine : (List (Block (List Char)) -> List (Block (List Char))) -> Line -> Line
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


init : ScreenCommand -> Model
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


appendCommand : ScreenCommand -> Model -> Model
appendCommand command model =
    case model.command of
        NoCommand ->
            { model | command = command }

        Batch commands ->
            { model | command = Batch (commands ++ [ command ]) }

        prevCommand ->
            { model | command = Batch (prevCommand :: [ command ]) }


evalCommand : Model -> Model
evalCommand model =
    case model.command of
        NoCommand ->
            model

        LineBreak ->
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

        Delete ->
            { model
                | visibleOutput = deleteChar model.cursorPosition model.visibleOutput
                , command = NoCommand
                , cursorPosition =
                    ( Tuple.first model.cursorPosition
                    , max 0 (Tuple.second model.cursorPosition - 1)
                    )
            }

        ClearScreen ->
            { model
                | visibleOutput = []
                , cursorPosition = ( 0, 0 )
                , command = NoCommand
            }

        MoveCursor updatedPosition ->
            { model
                | command = NoCommand
                , cursorPosition = updatedPosition model.cursorPosition
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
                    if evaled.command == NoCommand then
                        evalCommand { evaled | command = Batch restCommands }

                    else
                        { evaled | command = Batch (evaled.command :: restCommands) }


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


splitVisibleOutputAt :
    CursorPosition
    -> VisibleOutput
    ->
        { upperLns : List Line
        , currentLnRight : List (Block (List Char))
        , currentLnLeft : List (Block (List Char))
        , bottomLns : List Line
        }
splitVisibleOutputAt cursorPosition visibleOutput =
    let
        reverseLnIndex =
            List.length visibleOutput - Tuple.first cursorPosition - 1

        ( bottomLns, upperLns ) =
            ListE.splitAt reverseLnIndex visibleOutput
                |> Tuple.mapSecond ((++) (List.repeat -reverseLnIndex (Line [])))
    in
    case upperLns of
        (Line lastLn) :: restUpperLns ->
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

                rightBlocks =
                    List.map (\( _, _, block ) -> block) rightBlocksWithCol

                ( currentLnRight, currentLnLeft ) =
                    case leftBlocksWithCol of
                        ( colMin, _, lastBlock ) :: firstBlocksTuples ->
                            let
                                ( charsRight, charsLeft ) =
                                    splitBlockAt (reverseColIndex - colMin) lastBlock
                            in
                            ( charsRight :: rightBlocks
                            , charsLeft :: List.map (\( _, _, block ) -> block) firstBlocksTuples
                            )

                        [] ->
                            ( rightBlocks
                            , []
                            )
            in
            { upperLns = restUpperLns
            , currentLnLeft = currentLnLeft
            , currentLnRight = currentLnRight
            , bottomLns = bottomLns
            }

        _ ->
            { upperLns = []
            , currentLnLeft = []
            , currentLnRight = []
            , bottomLns = bottomLns
            }


mergeVisibleOutput :
    { upperLns : List Line
    , currentLnRight : List (Block (List Char))
    , currentLnLeft : List (Block (List Char))
    , bottomLns : List Line
    }
    -> VisibleOutput
mergeVisibleOutput { upperLns, currentLnLeft, currentLnRight, bottomLns } =
    let
        updatedLn =
            (currentLnRight ++ currentLnLeft)
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
    bottomLns ++ Line updatedLn :: upperLns


deleteChar : CursorPosition -> VisibleOutput -> VisibleOutput
deleteChar cursorPosition visibleOutput =
    splitVisibleOutputAt cursorPosition visibleOutput
        |> (\splitted ->
                case splitted.currentLnLeft of
                    [] ->
                        splitted

                    block :: restBlocks ->
                        let
                            ( _, remainder ) =
                                splitBlockAt 1 block
                        in
                        { splitted | currentLnLeft = remainder :: restBlocks }
           )
        |> mergeVisibleOutput


insertChar : CursorPosition -> Block Char -> VisibleOutput -> VisibleOutput
insertChar cursorPosition charBlock visibleOutput =
    splitVisibleOutputAt cursorPosition visibleOutput
        |> (\splitted ->
                { splitted | currentLnLeft = mapBlock List.singleton charBlock :: splitted.currentLnLeft }
           )
        |> mergeVisibleOutput


view : Model -> List (Html msg)
view model =
    let
        lns =
            List.reverse model.visibleOutput
                |> List.map (mapLine ((::) EmptyBlock >> List.map (mapBlock List.reverse) >> List.reverse))
    in
    global
        [ body
            [ backgroundColor (rgb 0 0 0)
            , color (rgb 178 178 178)
            , padding2 (px 20) (px 20)
            , fontFamilies [ "unscii-16", "monospace" ]
            ]
        ]
        :: List.indexedMap
            (viewLn model)
            lns


viewLn : Model -> Int -> Line -> Html msg
viewLn model ln (Line cols) =
    let
        ( _, elements ) =
            ListE.mapAccuml (viewBlock model ln) 0 cols
    in
    span [ css [ displayFlex ] ] elements


viewBlock : Model -> Int -> Int -> Block (List Char) -> ( Int, Html msg )
viewBlock model ln prevCol block =
    case block of
        NormalBlock cols ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , span [] elements
            )

        Colored ( blockColor, cols ) ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , span [ css [ color blockColor ] ] elements
            )

        Link ( blockUrl, cols ) ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , a
                [ css
                    [ display inlineBlock
                    , color inherit
                    , hover
                        [ color (rgb 0 0 255)
                        , backgroundColor (rgb 255 178 178)
                        ]
                    ]
                , href blockUrl
                ]
                elements
            )

        EmptyBlock ->
            viewCol model ln prevCol ' '


viewCol : Model -> Int -> Int -> Char -> ( Int, Html msg )
viewCol { cursorPosition, cursorVisible } ln prevCol char =
    ( prevCol + 1
    , span
        [ css
            ([ display inlineBlock
             , width (px 10)
             , height (px 18)
             , fontSize (px 20)
             ]
                ++ (if cursorPosition == ( ln, prevCol ) && cursorVisible then
                        [ backgroundColor (rgb 0 255 255) ]

                    else
                        []
                   )
            )
        ]
        [ text (String.fromChar char) ]
    )


print : String -> ScreenCommand
print string =
    Print (NormalBlock string)


printColored : Color -> String -> ScreenCommand
printColored color string =
    Print (Colored ( color, string ))


printLink : String -> String -> ScreenCommand
printLink href string =
    Print (Link ( href, string ))


printLn : String -> ScreenCommand
printLn string =
    Batch [ Print (NormalBlock string), LineBreak ]


printColoredLn : Color -> String -> ScreenCommand
printColoredLn color string =
    Batch [ Print (Colored ( color, string )), LineBreak ]


printLinkLn : String -> String -> ScreenCommand
printLinkLn href string =
    Batch [ Print (Link ( href, string )), LineBreak ]


lineBreak : ScreenCommand
lineBreak =
    LineBreak


clearScreen : ScreenCommand
clearScreen =
    ClearScreen


delete : ScreenCommand
delete =
    Delete


moveCursor : (CursorPosition -> CursorPosition) -> ScreenCommand
moveCursor =
    MoveCursor


batch : List ScreenCommand -> ScreenCommand
batch =
    Batch


noCommand : ScreenCommand
noCommand =
    NoCommand
