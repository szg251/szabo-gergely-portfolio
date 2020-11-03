module Figlet exposing (..)

import Command exposing (Command)
import Dict exposing (Dict)
import Figlet.Font as Font
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , Step(..)
        , backtrackable
        , chompIf
        , chompUntil
        , chompWhile
        , end
        , getChompedString
        , int
        , keyword
        , loop
        , number
        , oneOf
        , succeed
        , symbol
        )
import Screen


run : Command
run pipedInput args =
    let
        input =
            pipedInput
                |> Maybe.withDefault (String.join " " args)

        fonts =
            Dict.fromList
                [ ( "standard", Font.standard )
                , ( "starwars", Font.starwars )
                ]

        fontName =
            "starwars"

        parsedFont =
            Dict.get fontName fonts
                |> Maybe.andThen (Parser.run fontParser >> Result.toMaybe)
    in
    case parsedFont of
        Nothing ->
            Screen.printLn "Unable to open font file"

        Just font ->
            Screen.batch
                (toLines font input
                    |> List.map Screen.printLn
                )


toLines : Font -> String -> List String
toLines font input =
    let
        charCodes =
            input
                |> String.toList
                |> List.map Char.toCode

        empty =
            List.repeat font.params.height ""
    in
    List.foldl
        (\code lines ->
            let
                charLines =
                    Dict.get code font.chars
                        |> Maybe.map .lines
                        |> Maybe.withDefault empty
            in
            concatChars lines charLines
        )
        empty
        charCodes


concatChars : List String -> List String -> List String
concatChars =
    List.map2 (\xs ys -> xs ++ ys |> String.replace "$" " ")


type alias Font =
    { params : FontParams
    , chars : Dict Int FontChar
    }


type alias FontParams =
    { hardblank : Char
    , height : Int
    , baseline : Int
    , maxLength : Int
    , oldLayout : Int
    , commentLines : Int
    , printDirection : Maybe Int
    , fullLayout : Maybe Int
    , codetagCount : Maybe Int
    }


type alias FontChar =
    { charCode : Int
    , lines : List String
    }


fontParser : Parser Font
fontParser =
    succeed identity
        |. spaces
        |= fontParamsParser
        |> Parser.andThen
            (\params ->
                succeed Font
                    |= succeed params
                    |. skipLines params.commentLines
                    |= loop ( 0, Dict.empty ) charHelp
                    |. Parser.spaces
                    |. end
            )


charHelp : ( Int, Dict Int FontChar ) -> Parser (Step ( Int, Dict Int FontChar ) (Dict Int FontChar))
charHelp ( index, chars ) =
    oneOf
        [ succeed (\c -> Loop ( index + 1, Dict.insert c.charCode c chars ))
            |= fontCharParser index
        , succeed ()
            |> Parser.map
                (\_ -> Done chars)
        ]


char : Parser Char
char =
    getChompedString (chompIf (always True))
        |> Parser.andThen
            (\str ->
                case String.uncons str of
                    Nothing ->
                        Parser.problem "No char chomped."

                    Just ( c, _ ) ->
                        Parser.succeed c
            )


charCode : Parser Int
charCode =
    succeed identity
        |= number
            { int = Just identity
            , hex = Just identity
            , octal = Nothing
            , binary = Nothing
            , float = Nothing
            }
        |. chompUntil "\n"
        |. symbol "\n"


skipLines : Int -> Parser ()
skipLines n =
    let
        helper : Int -> Parser (Step Int ())
        helper counter =
            succeed
                (if counter == 0 then
                    Done ()

                 else
                    Loop (counter - 1)
                )
                |. chompUntil "\n"
                |. chompIf ((==) '\n')
    in
    loop n helper


spaces : Parser ()
spaces =
    chompWhile (\c -> c == ' ')


fontParamsParser : Parser FontParams
fontParamsParser =
    let
        optionalInt =
            oneOf
                [ Parser.map Just
                    (succeed identity
                        |= int
                        |. spaces
                    )
                , succeed Nothing
                ]
    in
    succeed FontParams
        |. keyword "flf2a"
        |= char
        |. spaces
        |= int
        |. spaces
        |= int
        |. spaces
        |= int
        |. spaces
        |= int
        |. spaces
        |= int
        |. spaces
        |= optionalInt
        |= optionalInt
        |= optionalInt
        |. chompIf ((==) '\n')


fontCharParser : Int -> Parser FontChar
fontCharParser index =
    succeed FontChar
        |= oneOf
            [ backtrackable charCode
            , if 0 <= index && index <= 94 then
                succeed (index + 32)

              else if index == 95 then
                succeed 196

              else if index == 96 then
                succeed 214

              else if index == 97 then
                succeed 220

              else if index == 98 then
                succeed 228

              else if index == 99 then
                succeed 246

              else if index == 100 then
                succeed 252

              else if index == 101 then
                succeed 223

              else
                Parser.problem "Char code needed."
            ]
        |= loop [] charLineHelp


type LineEnd
    = LineEnd
    | CharEnd


charLineHelp : List String -> Parser (Step (List String) (List String))
charLineHelp revArgs =
    let
        lineParser =
            getChompedString <|
                succeed ()
                    |. chompUntil "@"

        toLoop arg symbol =
            case symbol of
                LineEnd ->
                    Loop (arg :: revArgs)

                CharEnd ->
                    Done (List.reverse (arg :: revArgs))
    in
    succeed toLoop
        |= lineParser
        |= oneOf
            [ Parser.map (always LineEnd) (symbol "@\n")
            , Parser.map (always CharEnd) (symbol "@@\n")
            ]
