module Command exposing (..)

import Screen exposing (ScreenCommand)


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
