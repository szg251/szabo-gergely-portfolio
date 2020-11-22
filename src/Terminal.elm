module Terminal exposing (..)

import Browser.Navigation as Nav
import Command exposing (Command, Environment(..))
import Css exposing (rgb)
import Dict exposing (Dict)
import Json.Decode as Decode
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Parser
    exposing
        ( (|.)
        , (|=)
        , DeadEnd
        , Parser
        , Step(..)
        , backtrackable
        , chompUntil
        , chompUntilEndOr
        , end
        , getChompedString
        , keyword
        , loop
        , oneOf
        , spaces
        , succeed
        , symbol
        )
import Screen exposing (Block(..), CursorPosition, ScreenCommand(..))
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


type Msg
    = KeyDown Key
    | ScreenWidthChanged Int
    | UrlChanged Url.Url


type Key
    = Character Char
    | ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Backspace
    | Enter
    | Invalid


type alias Model =
    { prompt : String
    , inputBuffer : List Char
    , history : List String
    , historyIndex : Maybe Int
    , commandDict : Dict String Command
    , promptCursor : Int
    , navKey : Nav.Key
    , screenWidth : Int
    }


init :
    { commands : List ( String, Command )
    , initCommand : Maybe (List TerminalCommand)
    , navKey : Nav.Key
    , screenWidth : Int
    }
    -> ( Model, ScreenCommand )
init { commands, initCommand, navKey, screenWidth } =
    let
        model =
            { prompt = "$"
            , inputBuffer = []
            , history = []
            , historyIndex = Nothing
            , commandDict = Dict.fromList commands
            , promptCursor = 0
            , navKey = navKey
            , screenWidth = screenWidth
            }
    in
    ( model
    , case initCommand of
        Nothing ->
            printPrompt model.prompt True

        Just command ->
            Screen.batch
                [ List.map (evalCommand model) command |> Screen.batch
                , printPrompt model.prompt True
                ]
    )


printPrompt : String -> Bool -> ScreenCommand
printPrompt prompt withLineBreak =
    if withLineBreak then
        Screen.batch
            [ Screen.lineBreak
            , Screen.printColored { color = rgb 0 255 0, text = prompt ++ " " }
            ]

    else
        Screen.printColored { color = rgb 0 255 0, text = prompt ++ " " }


type alias TerminalCommand =
    ( String, List String )


parseCommand : String -> Result (List DeadEnd) (List TerminalCommand)
parseCommand =
    Parser.run
        (loop [] commandParserHelp)


commandParserHelp : List TerminalCommand -> Parser (Step (List TerminalCommand) (List TerminalCommand))
commandParserHelp revCommands =
    oneOf
        [ backtrackable <|
            succeed (\command -> Loop (command :: revCommands))
                |. keyword "&&"
                |. spaces
                |= commandParser
                |. spaces
        , succeed (\command -> Loop (command :: revCommands))
            |. spaces
            |= commandParser
            |. spaces
        , succeed ()
            |. end
            |> Parser.map (\_ -> Done (List.reverse revCommands))
        ]


commandParser : Parser TerminalCommand
commandParser =
    Parser.succeed Tuple.pair
        |= wordParser
        |. spaces
        |= loop [] argHelp


wordParser : Parser String
wordParser =
    oneOf
        [ Parser.succeed identity
            |. symbol "\""
            |= getChompedString (chompUntil "\"")
            |. symbol "\""
        , getChompedString (chompUntilEndOr " ")
            |> Parser.andThen
                (\str ->
                    if str == "" then
                        Parser.problem "Empty string"

                    else if String.startsWith "&" str then
                        Parser.problem ("Forbidden character: " ++ str)

                    else
                        Parser.succeed str
                )
        ]


argHelp : List String -> Parser (Step (List String) (List String))
argHelp revArgs =
    oneOf
        [ backtrackable <|
            succeed (\arg -> Loop (arg :: revArgs))
                |. spaces
                |= wordParser
                |. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revArgs))
        ]


evalCommand : Model -> ( String, List String ) -> ScreenCommand
evalCommand model ( commandName, args ) =
    let
        environment =
            Environment
                { screenWidth = model.screenWidth
                , args = args
                , commandDict = model.commandDict
                }
    in
    Dict.get commandName model.commandDict
        |> Maybe.map (\command -> command environment Nothing)
        |> Maybe.withDefault (Screen.printLn ("command not found: " ++ commandName))


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            case string of
                "ArrowLeft" ->
                    ArrowLeft

                "ArrowRight" ->
                    ArrowRight

                "ArrowUp" ->
                    ArrowUp

                "ArrowDown" ->
                    ArrowDown

                "Backspace" ->
                    Backspace

                "Enter" ->
                    Enter

                _ ->
                    Invalid


update : Msg -> Model -> ( Model, ScreenCommand, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            keyDown model key

        ScreenWidthChanged screenWidth ->
            ( { model | screenWidth = screenWidth }
            , Screen.batch
                [ Screen.clearScreen
                , printPrompt model.prompt False
                ]
            , Cmd.none
            )

        UrlChanged url ->
            case parseCommandUrl url of
                Nothing ->
                    ( model
                    , Screen.noCommand
                    , Cmd.none
                    )

                Just command ->
                    let
                        parsedCommand =
                            parseCommand command

                        inputCommand =
                            parseCommand (List.foldl String.cons "" model.inputBuffer)
                    in
                    ( { model
                        | inputBuffer = []
                        , promptCursor = 0
                        , history = command :: model.history
                        , historyIndex = Nothing
                      }
                    , Screen.batch
                        ([]
                            ++ (if List.isEmpty model.inputBuffer then
                                    [ Screen.printLn command ]

                                else if parsedCommand == inputCommand then
                                    [ Screen.lineBreak ]

                                else
                                    [ printPrompt model.prompt True
                                    , Screen.printLn command
                                    ]
                               )
                            ++ [ parsedCommand
                                    |> Result.map (List.map (evalCommand model) >> Screen.batch)
                                    |> Result.withDefault
                                        (Screen.printLn ("command not found: " ++ command))
                               , printPrompt model.prompt True
                               ]
                        )
                    , Cmd.none
                    )


keyDown : Model -> Key -> ( Model, ScreenCommand, Cmd Msg )
keyDown model key =
    case key of
        Character char ->
            ( { model
                | inputBuffer =
                    let
                        revIndex =
                            List.length model.inputBuffer - model.promptCursor

                        ( left, right ) =
                            ListE.splitAt revIndex model.inputBuffer
                    in
                    left ++ char :: right
                , promptCursor = model.promptCursor + 1
              }
            , Screen.print (String.fromChar char)
            , Cmd.none
            )

        ArrowLeft ->
            if model.promptCursor > 0 then
                ( { model | promptCursor = model.promptCursor - 1 }
                , Screen.moveCursor (\( ln, col ) -> ( ln, col - 1 ))
                , Cmd.none
                )

            else
                ( model, Screen.noCommand, Cmd.none )

        ArrowRight ->
            if model.promptCursor < List.length model.inputBuffer then
                ( { model | promptCursor = model.promptCursor + 1 }
                , Screen.moveCursor (\( ln, col ) -> ( ln, col + 1 ))
                , Cmd.none
                )

            else
                ( model, Screen.noCommand, Cmd.none )

        ArrowUp ->
            let
                nextHistoryIndex =
                    case model.historyIndex of
                        Nothing ->
                            0

                        Just index ->
                            min (List.length model.history) (index + 1)

                selectedHistory =
                    ListE.getAt nextHistoryIndex model.history
            in
            case selectedHistory of
                Nothing ->
                    ( model, Screen.noCommand, Cmd.none )

                Just string ->
                    ( { model
                        | historyIndex = Just nextHistoryIndex
                        , inputBuffer = String.toList string |> List.reverse
                        , promptCursor = String.length string
                      }
                    , Screen.batch
                        [ Screen.clearLn
                        , printPrompt model.prompt False
                        , Screen.print string
                        ]
                    , Cmd.none
                    )

        ArrowDown ->
            let
                nextHistoryIndex =
                    Maybe.map (\index -> max 0 (index - 1)) model.historyIndex

                selectedHistory =
                    Maybe.andThen (\index -> ListE.getAt index model.history) nextHistoryIndex
            in
            case selectedHistory of
                Nothing ->
                    ( model, Screen.noCommand, Cmd.none )

                Just string ->
                    ( { model
                        | historyIndex = nextHistoryIndex
                        , inputBuffer = String.toList string |> List.reverse
                        , promptCursor = String.length string
                      }
                    , Screen.batch
                        [ Screen.clearLn
                        , printPrompt model.prompt False
                        , Screen.print string
                        ]
                    , Cmd.none
                    )

        Backspace ->
            if model.promptCursor > 0 then
                let
                    revIndex =
                        List.length model.inputBuffer - model.promptCursor
                in
                ( { model
                    | promptCursor = model.promptCursor - 1
                    , inputBuffer = ListE.removeAt revIndex model.inputBuffer
                  }
                , Screen.delete
                , Cmd.none
                )

            else
                ( model, Screen.noCommand, Cmd.none )

        Enter ->
            let
                command =
                    List.foldl String.cons "" model.inputBuffer

                parsedCommand =
                    parseCommand command
            in
            ( model
            , Screen.noCommand
            , parsedCommand
                |> Result.map (pushCommandUrl model.navKey)
                |> Result.withDefault Cmd.none
            )

        Invalid ->
            ( model, Screen.noCommand, Cmd.none )


pushCommandUrl : Nav.Key -> List TerminalCommand -> Cmd msg
pushCommandUrl navKey commands =
    Nav.pushUrl navKey (buildCommandUrl commands)


buildCommandUrl : List TerminalCommand -> String
buildCommandUrl commands =
    List.map
        (\( commandName, args ) ->
            (commandName
                :: List.map
                    (\arg ->
                        if String.contains " " arg then
                            "\"" ++ arg ++ "\""

                        else
                            arg
                    )
                    args
            )
                |> String.join " "
        )
        commands
        |> String.join " && "
        |> String.replace "\\" "%5C"
        |> (\command -> Url.Builder.absolute [ command ] [])


parseCommandUrl : Url.Url -> Maybe String
parseCommandUrl =
    Url.Parser.parse Url.Parser.string
        >> Maybe.andThen Url.percentDecode
