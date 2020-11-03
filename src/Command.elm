module Command exposing (..)

import Screen exposing (ScreenCommand)


type alias Command =
    Maybe String -> List String -> ScreenCommand


echo : Command
echo _ args =
    Screen.printLn (String.join " " args)


clear : Command
clear _ _ =
    Screen.clearScreen
