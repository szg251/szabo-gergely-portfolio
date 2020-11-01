module Terminal exposing (..)

import Command exposing (Command)
import Css exposing (rgb)
import Dict exposing (Dict)
import Json.Decode as Decode
import List.Extra as ListE
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
    }


init : List ( String, Command ) -> Maybe String -> ( Model, ScreenCommand )
init commands maybeCommand =
    let
        commandDict =
            Dict.fromList commands

        model =
            { prompt = "$"
            , inputBuffer = []
            , history = []
            , commandDict = commandDict
            , promptCursor = 0
            }
    in
    ( model
    , case maybeCommand of
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


evalCommand : Dict String Command -> String -> ScreenCommand
evalCommand commandDict input =
    case parseCommand input of
        Err _ ->
            Screen.printLn (input ++ " not found")

        Ok ( commandName, args ) ->
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


keyDown : Model -> Key -> ( Model, ScreenCommand )
keyDown model key =
    case key of
        Character char ->
            ( { model
                | inputBuffer = char :: model.inputBuffer
                , promptCursor = model.promptCursor + 1
              }
            , Screen.print (String.fromChar char)
            )

        ArrowLeft ->
            if model.promptCursor > 0 then
                ( { model | promptCursor = model.promptCursor - 1 }
                , Screen.moveCursor (\( ln, col ) -> ( ln, col - 1 ))
                )

            else
                ( model, Screen.noCommand )

        ArrowRight ->
            if model.promptCursor < List.length model.inputBuffer then
                ( { model | promptCursor = model.promptCursor + 1 }
                , Screen.moveCursor (\( ln, col ) -> ( ln, col + 1 ))
                )

            else
                ( model, Screen.noCommand )

        ArrowUp ->
            ( model, Screen.noCommand )

        ArrowDown ->
            ( model, Screen.noCommand )

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
                )

            else
                ( model, Screen.noCommand )

        Enter ->
            ( { model
                | inputBuffer = []
                , promptCursor = 0
              }
            , Screen.batch
                [ Screen.lineBreak
                , evalCommand model.commandDict (List.foldl String.cons "" model.inputBuffer)
                , printPrompt model
                ]
            )

        Invalid ->
            ( model, Screen.noCommand )
