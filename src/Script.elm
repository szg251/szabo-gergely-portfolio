module Script exposing (..)

import Command exposing (Command, Environment(..))
import Dict
import Maybe.Extra as MaybeE
import Screen exposing (ScreenCommand)


white : String
white =
    "\\033[1;37m"


green : String
green =
    "\\033[0;32m"


noColor : String
noColor =
    "\\033[0m"


home : Command
home ((Environment { screenWidth }) as environment) _ =
    let
        welcomeText =
            """
            Hi!
            My name is Gergely Szabo. Welcome to my portfolio website.
            I am a Software Developer located in Hungary.
            I'm also an ex-musician, you can find a few of my videos and albums under the music menu.

            This website is a bit unconventional. You can click on menu links, or use the keyboard to type in commands. If you got annoyed by the slow print, just hit Enter!

            One more tip: play around with figlet!
            """
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
           , ( "mainmenu", [] )
           , ( "echo", [ multiline welcomeText ] )
           , ( "echo", [ white ++ "Github page: " ] )
           , ( "link", [ "https://github.com/szg251" ] )
           , ( "echo", [ white ++ "LinkedIn: " ] )
           , ( "link", [ "https://www.linkedin.com/in/szg251" ] )
           ]
        |> List.map (evalCommand environment)
        |> Screen.batch


mainmenu : Command
mainmenu environment _ =
    evalCommand environment ( "menu", [ "home", "skills", "projects", "music" ] )


skills : Command
skills ((Environment { screenWidth }) as environment) _ =
    let
        mySkills =
            [ ( green ++ "Programming language skills: Rust/Haskell/TypeScript" ++ noColor
              , "I have several years of experience using Rust, Haskell, PureScript and TypeScript, but also worked on projects with Elm, Ruby and Python."
              )
            , ( green ++ "Backend developer experience" ++ noColor
              , "Designed and built REST and gRPC backend services with the following libraries (non-exclusive list): Rocket, Axum, Tonic, Servant, IHP, Koa.js, Ruby on Rails."
              )
            , ( green ++ "Databases" ++ noColor
              , "Mostly but not exclusively used relational databases like PostgreSQL for backend applications and blockchain indexers handling large amount of data."
              )
            , ( green ++ "Blockchain dApp development" ++ noColor
              , "Designed and implemented several Cardano dApps, including sidechains, voting protoocols, etc."
              )
            , ( green ++ "Frontend developer experience" ++ noColor
              , "Worked as a full stack engineer maintaining large scale frontend and backend applications, using React (TypeScript) and Elm."
              )
            , ( green ++ "DevOps" ++ noColor
              , "Configured and maintained projects using Nix (Hercules CI), bare metal Linux, AWS (ECS, Aurora, Lambda, S3), GCP"
              )
            ]

        skillsText =
            mySkills
                |> List.map
                    (\( name, description ) ->
                        name ++ "\n" ++ description
                    )
                |> String.join "\n\n"

        workExperiencesTitle =
            white
                ++ """
                   Work Experiences
                   ----------------
                   """
                ++ noColor

        workExperiences =
            [ ( "2021.08. - present "
              , green ++ "MLabs" ++ noColor
              , "Haskell and Plutus consultant"
              )
            , ( "2019.04. - 2021.07 "
              , green ++ "Kakekomu (Tokyo, Japan)" ++ noColor
              , "Full-Stack Engineer full time (React, Elm, TypeScript, Koa.js, Ruby on Rails, AWS)"
              )
            , ( "2018.05. - 2019.03."
              , green ++ "Kakekomu (Tokyo, Japan)" ++ noColor
              , "Front-End Engineer part time (React, Next.js)"
              )
            , ( "2018.01. - 2019.03."
              , green ++ "Yahoo Japan (Tokyo, Japan)" ++ noColor
              , "Front-End engineer (React, TypeScript)"
              )
            , ( "2017.04. - 2017.12."
              , green ++ "Happiness Technology (Tokyo, Japan)" ++ noColor
              , "System Engineer (Java, Oracle SQL"
              )
            ]

        workExperiencesText =
            workExperiencesTitle
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

        languages =
            [ ( green ++ "Hungarian" ++ noColor
              , "native language"
              )
            , ( green ++ "English" ++ noColor
              , "fluent - TOEIC 985"
              )
            , ( green ++ "Japanese" ++ noColor
              , "fluent - JLPT N1"
              )
            ]

        languagesText =
            white
                ++ """
                   Languages
                   ---------
                   """
                ++ noColor
                ++ (languages
                        |> List.map
                            (\( name, description ) ->
                                name ++ "\n" ++ description
                            )
                        |> String.join "\n\n"
                   )
    in
    [ ( "figlet", [ "-f", "small", "Skills" ] )
    , ( "link", [ "-u", "mainmenu", "<- back to main menu" ] )
    , ( "echo", [] )
    , ( "echo", [ multiline skillsText ] )
    , ( "echo", [] )
    , ( "echo", [ multiline workExperiencesText ] )
    , ( "echo", [] )
    , ( "echo", [ multiline languagesText ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


projects : Command
projects environment _ =
    let
        openSourceProjectsTitle =
            white
                ++ """
                   Open-Source
                   -----------
                   """
                ++ noColor

        openSourceProjects =
            [ { label = "LambdaBuffers (Haskell / Rust / Nix)"
              , description = "Schema language and code generator for polyglot projects"
              , source = Just "https://github.com/mlabs-haskell/lambda-buffers"
              , example = Nothing
              }
            , { label = "Plutus Ledger API Rust"
              , description = "Plutus types and useful tools for Cardano dApp development"
              , source = Just "https://github.com/mlabs-haskell/plutus-ledger-api-rust"
              , example = Nothing
              }
            , { label = "Tx Village (Rust)"
              , description = "Rust based toolkit for Cardano transaction building, verification and chain-indexing"
              , source = Just "https://github.com/mlabs-haskell/tx-village"
              , example = Nothing
              }
            , { label = "Bot Plutus Interface (Haskell)"
              , description = "Transaction builder framework based on plutus-apps Contract monad interface"
              , source = Just "https://github.com/mlabs-haskell/bot-plutus-interface"
              , example = Nothing
              }
            , { label = "Plutip (Haskell)"
              , description = "Cardano local test network executer and test framework"
              , source = Just "https://github.com/mlabs-haskell/plutip"
              , example = Nothing
              }
            , { label = "Cardano Devnet flake (Nix)"
              , description = "Cardano local test network executer based on process-compose"
              , source = Just "https://github.com/mlabs-haskell/cardano-devnet-flake"
              , example = Nothing
              }
            , { label = "Flake-lang.nix (Nix)"
              , description = "Nix tools powering polyglot mono-repositories"
              , source = Just "https://github.com/mlabs-haskell/flake-lang.nix"
              , example = Nothing
              }
            ]

        petProjectsTitle =
            white
                ++ """
                   Pet projects
                   ------------
                   """
                ++ noColor

        petProjects =
            [ { label = "Szabo Gergely portfolio (Elm)"
              , description = "This web page, simulating a terminal with a few commands like `echo` and `figlet`."
              , source = Just "https://github.com/szg251/szabo-gergely-portfolio"
              , example = Just "https://szabogergely.com"
              }
            , { label = "Activity Analyser (Rust)"
              , description = "FIT file analyser for cycling activities"
              , source = Just "https://github.com/szg251/activity-analyser"
              , example = Nothing
              }
            , { label = "Chess Clock (embedded Rust)"
              , description = "Chess clock implemented for an STM32 microcontroller"
              , source = Just "https://github.com/szg251/chesschock"
              , example = Nothing
              }
            ]
    in
    [ ( "figlet", [ "-f", "small", "Projects" ] )
    , ( "link", [ "-u", "mainmenu", "<- back to main menu" ] )
    , ( "echo", [ multiline openSourceProjectsTitle ] )
    ]
        ++ List.concatMap
            (\{ label, description, source, example } ->
                MaybeE.values
                    [ Just ( "echo", [ white, label, noColor, "\\n" ] )
                    , Just ( "echo", [ description, "\\n" ] )
                    , Maybe.map (\src -> ( "link", [ "-t", "_blank", "-u", src, src ] )) source
                    , Maybe.map (\src -> ( "link", [ "-t", "_blank", "-u", src, "See project" ] )) example
                    ]
            )
            openSourceProjects
        ++ [ ( "echo", [ multiline petProjectsTitle ] )
           ]
        ++ List.concatMap
            (\{ label, description, source, example } ->
                MaybeE.values
                    [ Just ( "echo", [ white, label, noColor, "\\n" ] )
                    , Just ( "echo", [ description, "\\n" ] )
                    , Maybe.map (\src -> ( "link", [ "-t", "_blank", "-u", src, src ] )) source
                    , Maybe.map (\src -> ( "link", [ "-t", "_blank", "-u", src, "See project" ] )) example
                    ]
            )
            petProjects
        |> List.map (evalCommand environment)
        |> Screen.batch


music : Command
music environment _ =
    let
        discographyTitle =
            white ++ """
                     Discography
                     -----------
                     """

        discography =
            [ ( green ++ "Whirl" ++ noColor ++ " Antal Gábor Trio (2013/3)"
              , "https://open.spotify.com/album/5Ndwpp6V6j5ItlQ8NuN0Mc?si=K8gGnZKPRt-gaphQF0PPWw"
              )
            , ( green ++ "Mindenen át" ++ noColor ++ " VálaszÚt (2015/11)"
              , "https://open.spotify.com/album/6Ptd0tEZiwoGrqscJdlzhz?si=FHJTrTdNTDu5kvhPh-VVNA"
              )
            , ( green ++ "Infinity" ++ noColor ++ " Infinity (2015/9)"
              , "https://infinity-fusion.bandcamp.com/releases"
              )
            ]

        discographyCommands =
            ( "echo"
            , [ multiline discographyTitle
              ]
            )
                :: List.concatMap
                    (\( label, link ) ->
                        [ ( "echo", [ green, label, noColor, "\\n" ] )
                        , ( "link", [ "-t", "_blank", link ] )
                        ]
                    )
                    discography

        videosTitle =
            white ++ """
                     Videos
                     ------
                     """

        videos =
            [ ( "Antal Gábor Trio - Whirl (Medley)", "https://youtu.be/kDLOY8DkD-E" )
            , ( "Tigranito (For Tigran Hamasyan)", "https://youtu.be/oy_qXHpdgrE" )
            , ( "Antal Gábor Trio - Journey", "https://youtu.be/jDP71XZxJQc" )
            , ( "Sári Parker and Friends", "https://youtu.be/Y0RYoEHEnVY" )
            , ( "Infinity - Szabó Gergely bass solo", "https://youtu.be/QzIe1U9GrBY" )
            , ( "Kenny Banks keyboard solo transcription - Szabó Gergely", "https://youtu.be/9t6Nw2TMR1k" )
            , ( "Antal Gábor Jazz Trio - Double Rise (Torockó)", "https://youtu.be/O21OkgaewU0" )
            , ( "Infinity - Chill Out [Official HD Video]", "https://youtu.be/_ZBZzeJP9Qo" )
            , ( "Bob Mintzer Solo Transcription - Runferyerlife (played on bass by Szabó Gergely)", "https://youtu.be/ym0XbqVyQ7o" )
            , ( "Love Me Like You Do Fifty Shades Of Grey Ellie Goulding", "https://youtu.be/yHjKm6EMwaE" )
            , ( "VálaszÚt - Egy jó nagy korty az életből (stúdiós werk videoklip)", "https://youtu.be/MygKYjxdWTg" )
            ]

        videosCommands =
            ( "echo"
            , [ multiline videosTitle
              ]
            )
                :: List.concatMap
                    (\( label, link ) ->
                        [ ( "echo", [ green, label, noColor, "\\n" ] )
                        , ( "link", [ "-t", "_blank", link ] )
                        ]
                    )
                    videos
    in
    [ ( "figlet", [ "-f", "small", "Music" ] )
    , ( "link", [ "-u", "mainmenu", "<- back to main menu" ] )
    , ( "echo", [ "SoundCloud page: " ] )
    , ( "link", [ "https://soundcloud.com/szabogergely" ] )
    ]
        ++ discographyCommands
        ++ videosCommands
        |> List.map (evalCommand environment)
        |> Screen.batch


evalCommand : Environment -> ( String, List String ) -> ScreenCommand
evalCommand (Environment environment) ( commandName, args ) =
    Dict.get commandName environment.commandDict
        |> Maybe.map (\command -> command (Environment { environment | args = args }) Nothing)
        |> Maybe.withDefault (Screen.printLn ("command not found: " ++ commandName))



-- | Trim the white space from the prefix of multiline strings. If the whitespace is intentional,
-- $ sign can be used as a stopper, this will be replaced


multiline : String -> String
multiline =
    String.split "\n"
        >> List.map (String.trim >> String.replace "$" " ")
        >> String.join "\\n"
