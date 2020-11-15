module Script exposing (..)

import Command exposing (Command, Environment(..))
import Dict
import Screen exposing (ScreenCommand)
import Terminal exposing (TerminalCommand)


home : Command
home environment _ =
    [ ( "figlet", [ "-c", "-f", "slant", "Szabo", "Gergely" ] )
    , ( "echo", [] )
    , ( "menu", [ "home", "bio", "projects", "music" ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


evalCommand : Environment -> ( String, List String ) -> ScreenCommand
evalCommand (Environment environment) ( commandName, args ) =
    Dict.get commandName environment.commandDict
        |> Maybe.map (\command -> command (Environment { environment | args = args }) Nothing)
        |> Maybe.withDefault (Screen.printLn ("command not found: " ++ commandName))
