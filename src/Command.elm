module Command exposing (..)

import Dict exposing (Dict)
import Screen exposing (ScreenCommand)
import Url.Builder


type alias Command =
    Environment -> Maybe String -> ScreenCommand


type Environment
    = Environment
        { screenWidth : Int
        , args : List String
        , commandDict : Dict String Command
        }


echo : Command
echo (Environment { args }) _ =
    String.join " " args
        |> String.split "\\n"
        |> List.map Screen.printLn
        |> Screen.batch


clear : Command
clear _ _ =
    Screen.clearScreen


menu : Command
menu (Environment { args, screenWidth }) _ =
    let
        defaultOptions =
            { urls = []
            , padding = 4
            , spacing = 2
            }

        options =
            List.foldl
                (\opt opts ->
                    case opt of
                        Urls urls ->
                            { opts | urls = urls }

                        Padding padding ->
                            { opts | padding = padding }

                        Spacing spacing ->
                            { opts | spacing = spacing }
                )
                defaultOptions
                (readMenuOptions args)

        fullLength =
            (options.padding * 2)
                + List.foldl (String.length >> (+)) 0 options.urls
                + (max (List.length options.urls - 1) 0 * options.spacing)

        links =
            List.map
                (\url ->
                    Screen.printLink
                        { url = Url.Builder.absolute [ url ] []
                        , label = url
                        }
                )
                options.urls

        lengths =
            List.map String.length options.urls

        underlines =
            List.map (\length -> Screen.print (String.repeat length "-")) lengths
    in
    if fullLength > screenWidth then
        Screen.batch
            (links
                |> List.map
                    (\printLink ->
                        [ Screen.print "-> "
                        , printLink
                        , Screen.lineBreak
                        , Screen.lineBreak
                        ]
                    )
                |> List.concat
            )

    else
        Screen.batch
            (Screen.print (String.repeat ((screenWidth - fullLength) // 2 + options.padding) " ")
                :: List.intersperse (Screen.print (String.repeat options.spacing " ")) links
                ++ [ Screen.lineBreak
                   , Screen.print (String.repeat ((screenWidth - fullLength) // 2 + options.padding) " ")
                   ]
                ++ List.intersperse (Screen.print (String.repeat options.spacing " ")) underlines
                ++ [ Screen.lineBreak, Screen.lineBreak ]
            )


type MenuOption
    = Urls (List String)
    | Padding Int
    | Spacing Int


readMenuOptions : List String -> List MenuOption
readMenuOptions args =
    case args of
        fst :: snd :: rest ->
            if String.startsWith "-" fst then
                case fst of
                    "-p" ->
                        case String.toInt snd of
                            Just int ->
                                Padding int :: readMenuOptions rest

                            Nothing ->
                                readMenuOptions rest

                    "-s" ->
                        case String.toInt snd of
                            Just int ->
                                Spacing int :: readMenuOptions rest

                            Nothing ->
                                readMenuOptions rest

                    _ ->
                        readMenuOptions (snd :: rest)

            else
                [ Urls args ]

        _ ->
            [ Urls args ]


type LinkOption
    = Url String
    | Label String


readLinkOptions : List String -> List LinkOption
readLinkOptions args =
    case args of
        fst :: snd :: rest ->
            if String.startsWith "-" fst then
                case fst of
                    "-u" ->
                        Url fst :: readLinkOptions rest

                    _ ->
                        readLinkOptions (snd :: rest)

            else
                [ Label (String.join " " args) ]

        _ ->
            [ Label (String.join " " args) ]


link : Command
link (Environment { args, screenWidth }) _ =
    let
        defaultOptions =
            { url = Nothing
            , label = ""
            }

        options =
            List.foldl
                (\opt opts ->
                    case opt of
                        Url url ->
                            { opts | url = Just url }

                        Label label ->
                            { opts | label = label }
                )
                defaultOptions
                (readLinkOptions args)
    in
    Screen.printLinkLn
        { url =
            case options.url of
                Just url ->
                    url

                Nothing ->
                    options.label
        , label = options.label
        }
