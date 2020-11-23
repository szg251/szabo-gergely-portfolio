module Script exposing (..)

import Command exposing (Command, Environment(..))
import Dict
import Screen exposing (ScreenCommand)
import Terminal exposing (TerminalCommand)


home : Command
home ((Environment { screenWidth }) as environment) _ =
    let
        welcomeText =
            """
            Hi!
            My name is Gergely Szabo. I am a Full-Stack Web Developer and a functional programming enthusiast located in Japan.
            I'm also an ex-musician and a serious coffee lover.

            Github page: """
    in
    (if screenWidth > 40 then
        [ ( "figlet", [ "-c", "-f", "slant", "Szabo", "Gergely" ] ) ]

     else if screenWidth > 30 then
        [ ( "figlet", [ "-c", "-f", "small", "Szabo", "Gergely" ] ) ]

     else
        [ ( "echo", [ "Szabo Gergely" ] )
        , ( "echo", [ "----- -------" ] )
        ]
    )
        ++ [ ( "echo", [] )
           , ( "menu", [ "home", "bio", "projects", "music" ] )
           , ( "echo", [ toEchoArg welcomeText ] )
           , ( "link", [ "https://github.com/gege251" ] )
           ]
        |> List.map (evalCommand environment)
        |> Screen.batch


bio : Command
bio ((Environment { screenWidth }) as environment) _ =
    let
        white =
            "\\033[1;37m"

        noColor =
            "\\033[0m"

        workExperiences =
            [ ( "2019.04. - present "
              , white ++ "Kakekomu (Tokyo, Japan)" ++ noColor
              , "Full-Stack Engineer full time (React, Elm, TypeScript, Koa.js, Ruby on Rails, AWS)"
              )
            , ( "2018.05. - 2019.03."
              , white ++ "Kakekomu (Tokyo, Japan)" ++ noColor
              , "Front-End Engineer part time (React, Next.js)"
              )
            , ( "2018.01. - 2019.03."
              , white ++ "Yahoo Japan (Tokyo, Japan)" ++ noColor
              , "Front-End engineer (React, TypeScript)"
              )
            , ( "2017.04. - 2017.12."
              , white ++ "Happiness Technology (Tokyo, Japan)" ++ noColor
              , "System Engineer (Java, Oracle SQL"
              )
            ]

        workExperiencesText =
            """
            Work Experiences
            ----------------
            """
                ++ (if screenWidth > 80 then
                        workExperiences
                            |> List.map
                                (\( year, title, content ) ->
                                    year ++ "  " ++ title ++ "\n$                    " ++ content
                                )
                            |> String.join "\n\n"

                    else
                        workExperiences
                            |> List.map
                                (\( year, title, content ) ->
                                    year ++ "\n\n" ++ title ++ "\n" ++ content
                                )
                            |> String.join "\n\n"
                   )
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
