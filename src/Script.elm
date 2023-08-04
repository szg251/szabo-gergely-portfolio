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
            I am a Full-Stack Web Developer located in Hungary.
            I'm also an ex-musician, you can find a few of my videos and albums under the music menu.

            This website is a bit unconventional. You can click on menu links, or use the keyboard to type in commands. If you got annoyed by the slow print, just hit Enter!
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
           , ( "echo", [ toEchoArg welcomeText ] )
           , ( "echo", [ white ++ "Github page: " ] )
           , ( "link", [ "https://github.com/gege251" ] )
           , ( "echo", [ white ++ "Blogs: " ] )
           , ( "link", [ "https://dev.to/gege251" ] )
           , ( "echo", [ white ++ "Twitter: " ] )
           , ( "link", [ "https://twitter.com/gege251" ] )
           ]
        |> List.map (evalCommand environment)
        |> Screen.batch


mainmenu : Command
mainmenu environment _ =
    evalCommand environment ( "menu", [ "home", "bio", "projects", "music" ] )


bio : Command
bio ((Environment { screenWidth }) as environment) _ =
    let
        skills =
            [ ( green ++ "Haskell/PureScript" ++ noColor
              , "Experience building backend applications and CLI tools for blockchain tools and dApps."
              )
            , ( green ++ "Blockchain (Cardano, Plutus)" ++ noColor
              , "Writing specifications and building dApps with Plutus"
              )
            , ( green ++ "Elm" ++ noColor
              , "Several years of professional experience"
              )
            , ( green ++ "React, JavaScript, TypeScript" ++ noColor
              , "Working with React and TypeScript professionally to build and maintain large scale frontend and backend applications."
              )
            , ( green ++ "Servant, Express, Koa.js, Servant, Ruby on Rails" ++ noColor
              , "Experience building backend services."
              )
            , ( green ++ "Nix, AWS, GCP, Docker, MySQL, PostgreSQL" ++ noColor
              , "Work experience with the above tehcnologies."
              )
            ]

        skillsText =
            white
                ++ """
                   Skills (non-exhaustive list)
                   ----------------------------
                   """
                ++ noColor
                ++ (skills
                        |> List.map
                            (\( name, description ) ->
                                name ++ "\n" ++ description
                            )
                        |> String.join "\n\n"
                   )

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
            white
                ++ """
                   Work Experiences
                   ----------------
                   """
                ++ noColor
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
    [ ( "figlet", [ "-f", "small", "Bio" ] )
    , ( "link", [ "-u", "mainmenu", "<- back to main menu" ] )
    , ( "echo", [] )
    , ( "echo", [ toEchoArg skillsText ] )
    , ( "echo", [] )
    , ( "echo", [ toEchoArg workExperiencesText ] )
    , ( "echo", [] )
    , ( "echo", [ toEchoArg languagesText ] )
    ]
        |> List.map (evalCommand environment)
        |> Screen.batch


projects : Command
projects environment _ =
    let
        pjs =
            [ { label = "Chess"
              , description = "Chess game written in Elm."
              , source = Just "https://github.com/gege251/chess"
              , example = "https://chess.gege251.vercel.app/"
              }
            , { label = "Space Invaders"
              , description = "Space Invaders game in Elm."
              , source = Just "https://github.com/gege251/space_invaders"
              , example = "https://space-invaders.now.sh/"
              }
            , { label = "elm-validator-pipeline"
              , description = "Validate values and apply them to a user defined type. "
              , source = Just "https://github.com/gege251/elm-validator-pipeline"
              , example = "https://package.elm-lang.org/packages/gege251/elm-validator-pipeline/latest/"
              }
            , { label = "Saku English Cafe"
              , description = "English speaker's community in Saku, Japan. Written in Haskell and Elm."
              , source = Nothing
              , example = "https://sakuenglishcafe.com"
              }
            , { label = "Szabo Gergely portfolio"
              , description = "This web page."
              , source = Just "https://github.com/gege251/szabo-gergely-portfolio"
              , example = "https://szabogergely.com"
              }
            ]
    in
    [ ( "figlet", [ "-f", "small", "Projects" ] )
    , ( "link", [ "-u", "mainmenu", "<- back to main menu" ] )
    ]
        ++ List.concatMap
            (\{ label, description, source, example } ->
                MaybeE.values
                    [ Just ( "echo", [ white, label, noColor, "\\n" ] )
                    , Just ( "echo", [ description, "\\n" ] )
                    , Maybe.map (\src -> ( "link", [ "-t", "_blank", "-u", src, "Source" ] )) source
                    , Just ( "link", [ "-t", "_blank", "-u", example, "Running Example" ] )
                    ]
            )
            pjs
        |> List.map (evalCommand environment)
        |> Screen.batch


music : Command
music environment _ =
    let
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
            , [ toEchoArg
                    (white ++ """
                              Discography
                              -----------
                              """)
              ]
            )
                :: List.concatMap
                    (\( label, link ) ->
                        [ ( "echo", [ green, label, noColor, "\\n" ] )
                        , ( "link", [ "-t", "_blank", link ] )
                        ]
                    )
                    discography

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
            , [ toEchoArg
                    (white ++ """
                              Videos
                              ------
                              """)
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


toEchoArg : String -> String
toEchoArg =
    String.split "\n"
        >> List.map (String.trim >> String.replace "$" " ")
        >> String.join "\\n"
