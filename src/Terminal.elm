module Terminal exposing (..)

import Browser.Navigation as Nav
import Command exposing (Command)
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
        , chompUntil
        , chompUntilEndOr
        , end
        , getChompedString
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
    , commandDict : Dict String Command
    , promptCursor : Int
    , navKey : Nav.Key
    }


init :
    { commands : List ( String, Command )
    , initCommand : Maybe ( String, List String )
    , navKey : Nav.Key
    }
    -> ( Model, ScreenCommand )
init { commands, initCommand, navKey } =
    let
        commandDict =
            Dict.fromList commands

        model =
            { prompt = "$"
            , inputBuffer = []
            , history = []
            , commandDict = commandDict
            , promptCursor = 0
            , navKey = navKey
            }
    in
    ( model
    , case initCommand of
        Nothing ->
            printPrompt model

        Just firstCommand ->
            Screen.batch
                [ evalCommand commandDict firstCommand
                , printPrompt model
                ]
    )


printPrompt : Model -> ScreenCommand
printPrompt model =
    Screen.batch
        [ Screen.lineBreak
        , Screen.printColored (rgb 0 255 0) (model.prompt ++ " ")
        ]


parseCommand : String -> Result (List DeadEnd) ( String, List String )
parseCommand =
    Parser.run commandParser


commandParser : Parser ( String, List String )
commandParser =
    Parser.succeed Tuple.pair
        |= wordParser
        |. spaces
        |= loop [] argHelp
        |. spaces
        |. end


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
                        Parser.problem "Empty string is invalid"

                    else
                        Parser.succeed str
                )
        ]


argHelp : List String -> Parser (Step (List String) (List String))
argHelp revArgs =
    oneOf
        [ succeed (\arg -> Loop (arg :: revArgs))
            |. spaces
            |= wordParser
            |. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revArgs))
        ]



-- evalCommand : Dict String Command -> String -> ScreenCommand
-- evalCommand commandDict input =
--     case parseCommand input of
--         Err _ ->
--             Screen.printLn (input ++ " not found")
--         Ok ( commandName, args ) ->
--             Dict.get commandName commandDict
--                 |> Maybe.map (\command -> command Nothing args |> Screen.printLn)
--                 |> Maybe.withDefault (Screen.printLn ("command not found: " ++ commandName))


evalCommand : Dict String Command -> ( String, List String ) -> ScreenCommand
evalCommand commandDict ( commandName, args ) =
    Dict.get commandName commandDict
        |> Maybe.map (\command -> command Nothing args |> Screen.printLn)
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


keyDown : Model -> Key -> ( Model, ScreenCommand, Cmd msg )
keyDown model key =
    case key of
        Character char ->
            ( { model
                | inputBuffer = char :: model.inputBuffer
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
            ( model, Screen.noCommand, Cmd.none )

        ArrowDown ->
            ( model, Screen.noCommand, Cmd.none )

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
            ( { model
                | inputBuffer = []
                , promptCursor = 0
              }
            , Screen.batch
                [ Screen.lineBreak
                , parsedCommand
                    |> Result.map (evalCommand model.commandDict)
                    |> Result.withDefault (Screen.printLn (command ++ " not found"))
                , printPrompt model
                ]
            , parsedCommand
                |> Result.map (pushCommandUrl model.navKey)
                |> Result.withDefault Cmd.none
            )

        Invalid ->
            ( model, Screen.noCommand, Cmd.none )


pushCommandUrl : Nav.Key -> ( String, List String ) -> Cmd msg
pushCommandUrl navKey ( commandName, args ) =
    Nav.pushUrl navKey
        (Url.Builder.absolute [ commandName ]
            (List.indexedMap
                (\index arg ->
                    Url.Builder.string (String.fromInt index) arg
                )
                args
            )
        )


parseCommandUrl : Url.Url -> Maybe ( String, List String )
parseCommandUrl url =
    let
        toTuple commandName arg0 arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 =
            ( commandName
            , [ arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 ] |> MaybeE.values
            )

        urlParser =
            Url.Parser.string
                <?> Url.Parser.Query.string "0"
                <?> Url.Parser.Query.string "1"
                <?> Url.Parser.Query.string "2"
                <?> Url.Parser.Query.string "3"
                <?> Url.Parser.Query.string "4"
                <?> Url.Parser.Query.string "5"
                <?> Url.Parser.Query.string "6"
                <?> Url.Parser.Query.string "7"
                <?> Url.Parser.Query.string "8"
                <?> Url.Parser.Query.string "9"
                |> Url.Parser.map toTuple
    in
    Url.Parser.parse urlParser url
