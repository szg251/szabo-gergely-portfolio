module Command exposing (..)

import Screen exposing (ScreenCommand)
import Url.Builder


type alias Command =
    Environment -> Maybe String -> ScreenCommand


type alias Environment =
    { screenWidth : Int
    , args : List String
    }


echo : Command
echo { args } _ =
    Screen.printLn (String.join " " args)


clear : Command
clear _ _ =
    Screen.clearScreen



-- alignText : Int -> Int -> Align -> String -> String
-- alignText maxWidth length align text =
--     case align of
--         Left ->
--             text
--         Right ->
--             String.trimRight (String.repeat (maxWidth - length) " " ++ text)
--         Center ->
--             String.trimRight (String.repeat ((maxWidth - length) // 2) " " ++ text)


menu : Command
menu { args, screenWidth } _ =
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
                (readOptions args)

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
            ((List.map2
                (\link length ->
                    [ Screen.print (String.repeat (screenWidth - length // 2) " ")
                    , link
                    ]
                )
                links
                lengths
                |> List.concat
             )
                ++ [ Screen.lineBreak ]
                ++ (List.map2
                        (\underline length ->
                            [ Screen.print (String.repeat (screenWidth - length // 2) " ")
                            , underline
                            ]
                        )
                        underlines
                        lengths
                        |> List.concat
                   )
            )

    else
        Screen.batch
            (Screen.print (String.repeat ((screenWidth - fullLength) // 2 + options.padding) " ")
                :: List.intersperse (Screen.print (String.repeat options.spacing " ")) links
                ++ [ Screen.lineBreak
                   , Screen.print (String.repeat ((screenWidth - fullLength) // 2 + options.padding) " ")
                   ]
                ++ List.intersperse (Screen.print (String.repeat options.spacing " ")) underlines
            )


type Option
    = Urls (List String)
    | Padding Int
    | Spacing Int


readOptions : List String -> List Option
readOptions args =
    case args of
        fst :: snd :: rest ->
            if String.startsWith "-" fst then
                case fst of
                    "-p" ->
                        case String.toInt snd of
                            Just int ->
                                Padding int :: readOptions rest

                            Nothing ->
                                readOptions rest

                    "-s" ->
                        case String.toInt snd of
                            Just int ->
                                Spacing int :: readOptions rest

                            Nothing ->
                                readOptions rest

                    _ ->
                        readOptions (snd :: rest)

            else
                [ Urls args ]

        _ ->
            [ Urls args ]
