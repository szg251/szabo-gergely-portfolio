module Command exposing (..)


type alias Command =
    Maybe String -> List String -> String


echo : Command
echo _ args =
    String.join " " args
