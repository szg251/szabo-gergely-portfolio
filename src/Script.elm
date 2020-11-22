module Script exposing (..)

import Command exposing (Command, Environment(..))
import Dict
import Screen exposing (ScreenCommand)
import Terminal exposing (TerminalCommand)


home : Command
home environment _ =
    let
        welcomeText =
            """
            Hi!
            My name is Gergely Szabo. I am a Full-Stack Web Developer and a functional programming enthusiast located in Japan.
            I'm also an ex-musician and a serious coffee lover.

            Github page: """
    in
    [ ( "figlet", [ "-c", "-f", "slant", "Szabo", "Gergely" ] )
    , ( "echo", [] )
    , ( "menu", [ "home", "bio", "projects", "music" ] )
    , ( "echo", [ toEchoArg welcomeText ] )
    , ( "link", [ "https://github.com/gege251" ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


bio : Command
bio environment _ =
    let
        workExperiencesText =
            """
            Work Experiences
            ----------------
            2019.04. - present  Kakekomu (Tokyo, Japan)
            $                   Full-Stack Engineer full time (React, Elm, TypeScript, Koa.js, Ruby on Rails, AWS)

            2018.05. - 2019.03  Kakekomu (Tokyo, Japan)
            $                   Front-End Engineer part time (React, Next.js)

            2018.01. - 2019.03. Yahoo Japan (Tokyo, Japan)
            $                   Front-End engineer (React, TypeScript)

            2017.04. - 2017.12. Happiness Technology (Tokyo, Japan)
            $                   System Engineer (Java, Oracle SQL)
            """
    in
    [ ( "figlet", [ "-f", "small", "Bio" ] )
    , ( "echo", [] )
    , ( "echo", [ toEchoArg workExperiencesText ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


projects : Command
projects environment _ =
    [ ( "figlet", [ "-f", "small", "Projects" ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


music : Command
music environment _ =
    [ ( "figlet", [ "-f", "small", "Music" ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


evalCommand : Environment -> ( String, List String ) -> ScreenCommand
evalCommand (Environment environment) ( commandName, args ) =
    Dict.get commandName environment.commandDict
        |> Maybe.map (\command -> command (Environment { environment | args = args }) Nothing)
        |> Maybe.withDefault (Screen.printLn ("command not found: " ++ commandName))


toEchoArg : String -> String
toEchoArg =
    String.split "\n"
        >> List.map (String.trim >> String.replace "$" " ")
        >> String.join "\\n"
