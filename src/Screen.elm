module Screen exposing (..)

import Browser.Dom
import Css exposing (..)
import Css.Global exposing (body, global)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href)
import List.Extra as ListE
import Process
import Screen.Color exposing (ScreenColor(..))
import Task


type Msg
    = NoOp
    | EvalNextCommand
    | BlinkCursor
    | Flush
    | AppendCommand ScreenCommand
    | ScreenSizeChanged { screenWidth : Int, screenHeight : Int }


type alias Model =
    { visibleOutput : VisibleOutput
    , command : ScreenCommand
    , cursorVisible : Bool
    , cursorPosition : CursorPosition
    , screenWidth : Int
    , screenHeight : Int
    }


init : { screenWidth : Int, screenHeight : Int, command : ScreenCommand } -> ( Model, Cmd Msg )
init { screenWidth, screenHeight, command } =
    ( { command = command
      , visibleOutput = [ Line [] ]
      , cursorVisible = True
      , cursorPosition = ( 0, 0 )
      , screenWidth = screenWidth
      , screenHeight = screenHeight
      }
    , Task.perform (always EvalNextCommand) (Process.sleep 0)
    )


type ScreenCommand
    = NoCommand
    | Print (Block String)
    | Delete
    | ClearScreen
    | ClearLn
    | MoveCursor (CursorPosition -> CursorPosition)
    | LineBreak
    | Batch (List ScreenCommand)


type alias VisibleOutput =
    List Line


type Line
    = Line (List (Block (List Char)))


type Block a
    = NormalBlock a
    | Colored ( ScreenColor, a )
    | Link ( String, a, String )
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

        Link ( x, y, z ) ->
            Link ( x, fn y, z )

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

        Link ( href, xss, target ) ->
            let
                ( x, xs ) =
                    ListE.splitAt index xss
            in
            ( Link ( href, x, target ), Link ( href, xs, target ) )

        EmptyBlock ->
            ( EmptyBlock, EmptyBlock )


isEmptyBlock : Block (List Char) -> Bool
isEmptyBlock block =
    case block of
        NormalBlock xss ->
            List.isEmpty xss

        Colored ( _, xss ) ->
            List.isEmpty xss

        Link ( _, xss, _ ) ->
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

        ( Link ( hrefA, a, targetA ), Link ( hrefB, b, targetB ) ) ->
            if hrefA == hrefB then
                [ Link ( hrefA, a ++ b, targetA ) ]

            else
                [ blockA, blockB ]

        ( _, _ ) ->
            [ blockA, blockB ]


unconsBlock : Block String -> Maybe ( Block Char, Block String )
unconsBlock block =
    case mapBlock String.uncons block of
        NormalBlock (Just ( y, ys )) ->
            Just ( NormalBlock y, NormalBlock ys )

        Colored ( x, Just ( y, ys ) ) ->
            Just ( Colored ( x, y ), Colored ( x, ys ) )

        Link ( x, Just ( y, ys ), z ) ->
            Just ( Link ( x, y, z ), Link ( x, ys, z ) )

        NormalBlock Nothing ->
            Nothing

        Colored ( _, Nothing ) ->
            Nothing

        Link ( _, Nothing, _ ) ->
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

        Link ( _, x, _ ) ->
            List.length x

        EmptyBlock ->
            0


type alias CursorPosition =
    ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        BlinkCursor ->
            ( blinkCursor model, Cmd.none )

        EvalNextCommand ->
            evalCommand model
                |> scheduleNextEval

        AppendCommand command ->
            appendCommand command model
                |> scheduleNextEval

        Flush ->
            flush ( model, Cmd.none )

        ScreenSizeChanged { screenWidth, screenHeight } ->
            ( { model | screenWidth = screenWidth, screenHeight = screenHeight }
            , Cmd.none
            )


flush : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
flush ( model, cmd ) =
    if model.command /= NoCommand then
        let
            ( nextModel, nextCmd ) =
                evalCommand model
        in
        flush
            ( nextModel
            , Cmd.batch
                [ cmd
                , nextCmd
                ]
            )

    else
        ( model, cmd )


scheduleNextEval : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
scheduleNextEval ( model, cmd ) =
    if model.command /= NoCommand then
        ( model
        , Cmd.batch
            [ cmd
            , Task.perform (always EvalNextCommand) (Process.sleep 0)
            ]
        )

    else
        ( model, cmd )


blinkCursor : Model -> Model
blinkCursor model =
    { model | cursorVisible = not model.cursorVisible }


appendCommand : ScreenCommand -> Model -> ( Model, Cmd Msg )
appendCommand command model =
    case model.command of
        NoCommand ->
            ( { model | command = command }, Cmd.none )

        Batch commands ->
            ( { model | command = Batch (commands ++ [ command ]) }, Cmd.none )

        prevCommand ->
            ( { model | command = Batch (prevCommand :: [ command ]) }, Cmd.none )


evalCommand : Model -> ( Model, Cmd Msg )
evalCommand model =
    case model.command of
        NoCommand ->
            ( model, Cmd.none )

        LineBreak ->
            ( { model
                | visibleOutput = Line [] :: model.visibleOutput
                , command = NoCommand
                , cursorPosition =
                    ( Tuple.first model.cursorPosition + 1, 0 )
              }
                |> trimHeight
            , scrollToBottom
            )

        Print block ->
            case unconsBlock block of
                Just ( firstChar, lastChars ) ->
                    ( { model
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
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | command = NoCommand }, Cmd.none )

        Delete ->
            ( { model
                | visibleOutput = deleteChar model.cursorPosition model.visibleOutput
                , command = NoCommand
                , cursorPosition =
                    ( Tuple.first model.cursorPosition
                    , max 0 (Tuple.second model.cursorPosition - 1)
                    )
              }
            , Cmd.none
            )

        ClearScreen ->
            ( { model
                | visibleOutput = []
                , cursorPosition = ( 0, 0 )
                , command = NoCommand
              }
            , Cmd.none
            )

        ClearLn ->
            ( { model
                | visibleOutput = clearLine model.cursorPosition model.visibleOutput
                , command = NoCommand
                , cursorPosition = ( Tuple.first model.cursorPosition, 0 )
              }
            , Cmd.none
            )

        MoveCursor updatedPosition ->
            ( { model
                | command = NoCommand
                , cursorPosition = updatedPosition model.cursorPosition
              }
                |> trimHeight
            , scrollToBottom
            )

        Batch commands ->
            case commands of
                [] ->
                    ( { model | command = NoCommand }, Cmd.none )

                firstCommand :: restCommands ->
                    let
                        ( evaled, cmd ) =
                            evalCommand
                                { model | command = firstCommand }
                    in
                    if evaled.command == NoCommand then
                        let
                            ( evaledAgain, nextCmd ) =
                                evalCommand { evaled | command = Batch restCommands }
                        in
                        ( evaledAgain, Cmd.batch [ cmd, nextCmd ] )

                    else
                        ( { evaled | command = Batch (evaled.command :: restCommands) }, cmd )


pxToWidth : Int -> Int
pxToWidth pixels =
    (pixels - 40) // 10


pxToHeight : Int -> Int
pxToHeight pixels =
    (pixels - 40) // 20


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
        lastLn :: restUpperLns ->
            let
                ( currentLnLeft, currentLnRight ) =
                    splitLineAt (Tuple.second cursorPosition) lastLn
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


splitLineAt : Int -> Line -> ( List (Block (List Char)), List (Block (List Char)) )
splitLineAt index (Line line) =
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
                line

        reverseColIndex =
            length - index

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
    in
    case leftBlocksWithCol of
        ( colMin, _, lastBlock ) :: firstBlocksTuples ->
            let
                ( charsRight, charsLeft ) =
                    splitBlockAt (reverseColIndex - colMin) lastBlock
            in
            ( charsLeft :: List.map (\( _, _, block ) -> block) firstBlocksTuples
            , charsRight :: rightBlocks
            )

        [] ->
            ( [], rightBlocks )


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


scrollToBottom : Cmd Msg
scrollToBottom =
    Task.perform (\_ -> NoOp)
        (Browser.Dom.getViewport
            |> Task.andThen
                (\{ scene, viewport } ->
                    Browser.Dom.setViewport 0 (scene.height - viewport.height)
                )
        )


trimHeight : Model -> Model
trimHeight model =
    let
        bufferLength =
            model.screenHeight * 3

        trimmedLns =
            max (List.length model.visibleOutput - bufferLength) 0
    in
    { model
        | visibleOutput = List.take bufferLength model.visibleOutput
        , cursorPosition = Tuple.mapFirst (\lns -> lns - trimmedLns) model.cursorPosition
    }


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


clearLine : CursorPosition -> VisibleOutput -> VisibleOutput
clearLine cursorPosition visibleOutput =
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
                        { splitted | currentLnLeft = [], currentLnRight = [] }
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
    in
    global
        [ body
            [ backgroundColor (rgb 0 0 0)
            , color (Screen.Color.toCssColor LightGray)
            , margin zero
            , padding2 (px 20) (px 20)
            , fontFamilies [ "unscii-16", "monospace" ]
            , fontSize (px 20)
            , lineHeight (px 20)
            , whiteSpace Css.pre
            ]
        ]
        :: List.concat
            (List.indexedMap
                (viewLn model)
                lns
            )


viewLn : Model -> Int -> Line -> List (Html msg)
viewLn model ln (Line blocks) =
    let
        splitLongLine line =
            if List.foldl (blockLength >> (+)) 0 line <= model.screenWidth then
                [ line ]

            else
                let
                    ( fstLine, sndLine ) =
                        splitLineAt model.screenWidth (Line line)
                in
                fstLine :: splitLongLine sndLine
    in
    splitLongLine blocks
        |> List.indexedMap
            (\index splitted ->
                span [ css [ display block, height (px 20) ] ]
                    ((EmptyBlock :: splitted)
                        |> List.map (mapBlock List.reverse)
                        |> List.reverse
                        |> ListE.mapAccuml (viewBlock model ln) (index * model.screenWidth)
                        |> Tuple.second
                    )
            )


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
            , span [ css [ color (Screen.Color.toCssColor blockColor) ] ] elements
            )

        Link ( blockUrl, cols, target ) ->
            let
                ( nextCols, elements ) =
                    ListE.mapAccuml (viewCol model ln) prevCol cols
            in
            ( nextCols
            , a
                [ css
                    [ textDecoration none
                    , color (Screen.Color.toCssColor Screen.Color.Cyan)
                    , hover
                        [ color (Screen.Color.toCssColor Screen.Color.Blue)
                        , backgroundColor (Screen.Color.toCssColor Screen.Color.Cyan)
                        ]
                    ]
                , href blockUrl
                , Html.Styled.Attributes.target target
                ]
                elements
            )

        EmptyBlock ->
            viewCol model ln prevCol ' '


viewCol : Model -> Int -> Int -> Char -> ( Int, Html msg )
viewCol { cursorPosition, cursorVisible } ln prevCol char =
    ( prevCol + 1
    , if cursorPosition == ( ln, prevCol ) then
        span
            (if cursorVisible then
                [ css [ backgroundColor (rgb 0 255 255) ] ]

             else
                []
            )
            [ text (String.fromChar char)
            ]

      else
        text (String.fromChar char)
    )


print : String -> ScreenCommand
print string =
    Print (NormalBlock string)


printColored : { color : ScreenColor, text : String } -> ScreenCommand
printColored { color, text } =
    Print (Colored ( color, text ))


printLink : { url : String, label : String, target : String } -> ScreenCommand
printLink { url, label, target } =
    Print (Link ( url, label, target ))


printLn : String -> ScreenCommand
printLn string =
    Batch [ Print (NormalBlock string), LineBreak ]


printColoredLn : { color : ScreenColor, text : String } -> ScreenCommand
printColoredLn { color, text } =
    Batch [ Print (Colored ( color, text )), LineBreak ]


printLinkLn : { url : String, label : String, target : String } -> ScreenCommand
printLinkLn { url, label, target } =
    Batch [ Print (Link ( url, label, target )), LineBreak ]


lineBreak : ScreenCommand
lineBreak =
    LineBreak


clearScreen : ScreenCommand
clearScreen =
    ClearScreen


clearLn : ScreenCommand
clearLn =
    ClearLn


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
