module Figlet exposing (..)

import Command exposing (Command, Environment(..))
import Dict exposing (Dict)
import Figlet.Font as Font
import List.Extra as ListE
import Maybe.Extra as MaybeE
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
import Result.Extra as ResultE
import Screen


run : Command
run (Environment { args, screenWidth }) pipedInput =
    case parseOptions screenWidth pipedInput args of
        Err errMsg ->
            Screen.printLn errMsg

        Ok options ->
            let
                fonts =
                    Dict.fromList
                        [ ( "standard", Font.standard )
                        , ( "starwars", Font.starwars )
                        , ( "mini", Font.mini )
                        , ( "small", Font.small )
                        , ( "block", Font.block )
                        , ( "slant", Font.slant )
                        ]

                parsedFont =
                    Dict.get options.fontName fonts
                        |> Maybe.andThen (Parser.run fontParser >> Result.toMaybe)
            in
            if options.help then
                Screen.batch
                    [ Screen.printLn "FIGlet - display large characters made up of ordinary scraeen characters"
                    , Screen.printLn "Usage: figlet [OPTIONS]"
                    , Screen.printLn "Options:"
                    , Screen.printLn "    -f, --fontname <standard | starwars | mini | small | block | slant>"
                    , Screen.printLn "        Font name [default: standard]"
                    , Screen.printLn "    -w, --width <INTEGER>"
                    , Screen.printLn "        Width used for text alignment [default: screen width]"
                    , Screen.printLn "    -c, --center"
                    , Screen.printLn "        Align to center"
                    , Screen.printLn "    -l, --left"
                    , Screen.printLn "        Align to left"
                    , Screen.printLn "    -r, --right"
                    , Screen.printLn "        Align to right"
                    , Screen.printLn "    -h, --help"
                    , Screen.printLn "        Print help"
                    ]

            else
                case parsedFont of
                    Nothing ->
                        Screen.printLn ("Unable to open font file: " ++ options.fontName)

                    Just font ->
                        Screen.batch
                            (toLines font options.width options.align options.input
                                |> List.map Screen.printLn
                            )


type Option
    = FontName String
    | Width Int
    | Input String
    | Align Align
    | Help


type Align
    = Left
    | Center
    | Right


type alias Options =
    { fontName : String
    , width : Int
    , input : String
    , align : Align
    , help : Bool
    }


readOptions : List String -> Result String (List Option)
readOptions args =
    case args of
        fst :: rest ->
            if String.startsWith "-" fst then
                if fst == "-f" || fst == "--fontname" then
                    case rest of
                        snd :: rest2 ->
                            readOptions rest2 |> Result.map ((::) (FontName snd))

                        _ ->
                            Err "a value is required for -f, for more information try -h"

                else if fst == "-w" || fst == "--width" then
                    case rest of
                        snd :: rest2 ->
                            case String.toInt snd of
                                Just int ->
                                    readOptions rest2 |> Result.map ((::) (Width int))

                                Nothing ->
                                    Err "a value is required for -w, for more information try -h"

                        _ ->
                            Err "a value is required for -w, for more information try -h"

                else if fst == "-c" || fst == "--center" then
                    readOptions rest |> Result.map ((::) (Align Center))

                else if fst == "-l" || fst == "--left" then
                    readOptions rest |> Result.map ((::) (Align Left))

                else if fst == "-r" || fst == "--right" then
                    readOptions rest |> Result.map ((::) (Align Right))

                else if fst == "-h" || fst == "--help" then
                    Ok [ Help ]

                else if fst == "" then
                    Err "no input"

                else
                    Err "invalid argument"

            else
                Ok [ Input (String.join " " args) ]

        [] ->
            Err "no input"


parseOptions : Int -> Maybe String -> List String -> Result String Options
parseOptions screenWidth pipedInput args =
    let
        defaultOptions =
            { fontName = "standard"
            , width = screenWidth
            , input = ""
            , align = Left
            , help = False
            }
    in
    Result.map
        (\optList ->
            List.foldl
                (\opt opts ->
                    case opt of
                        FontName name ->
                            { opts | fontName = name }

                        Width width ->
                            { opts | width = width }

                        Input input ->
                            { opts
                                | input =
                                    Maybe.withDefault input pipedInput
                            }

                        Align align ->
                            { opts | align = align }

                        Help ->
                            { opts | help = True }
                )
                defaultOptions
                optList
        )
        (readOptions args)


toLines : Font -> Int -> Align -> String -> List String
toLines font maxWidth align input =
    let
        emptyLines =
            List.repeat font.params.height ""
    in
    String.words input
        |> List.map (String.cons ' ' >> createWord font maxWidth)
        |> List.foldr (joinLines font.params maxWidth) emptyLines
        |> alignRows font.params align maxWidth
        |> List.map (String.replace (String.fromChar font.params.hardblank) " ")


alignRows : FontParams -> Align -> Int -> List String -> List String
alignRows fontParams align maxWidth rows =
    let
        alignText lines =
            let
                length =
                    List.map String.length lines
                        |> List.maximum
                        |> Maybe.withDefault 0
            in
            case align of
                Left ->
                    lines

                Right ->
                    List.map (\text -> String.trimRight (String.repeat (maxWidth - length) " " ++ text)) lines

                Center ->
                    List.map
                        (\text -> String.trimRight (String.repeat ((maxWidth - length) // 2) " " ++ text))
                        lines
    in
    ListE.groupsOf fontParams.height rows
        |> List.map alignText
        |> List.concat


createWord : Font -> Int -> String -> List String
createWord font maxWidth str =
    let
        emptyLines =
            List.repeat font.params.height ""
    in
    String.toList str
        |> List.map
            (\ch ->
                Dict.get (Char.toCode ch) font.chars
                    |> Maybe.map .lines
                    |> Maybe.withDefault emptyLines
            )
        |> List.foldr (joinLines font.params maxWidth) emptyLines


joinLines : FontParams -> Int -> List String -> List String -> List String
joinLines fontParams maxWidth inputL inputR =
    let
        ( restOfL, lastOfL ) =
            ListE.splitAt (List.length inputL - fontParams.height) inputL

        ( firstOfR, restOfR ) =
            ListE.splitAt fontParams.height inputR

        smushed =
            smushLines fontParams lastOfL firstOfR

        length =
            List.map String.length smushed
                |> List.maximum
                |> Maybe.withDefault 0
    in
    if length > maxWidth then
        inputL ++ inputR

    else
        restOfL ++ smushed ++ restOfR



-- inputL ++ inputR
-- restOfR ++ smushed ++ restOfL


smushLines : FontParams -> List String -> List String -> List String
smushLines fontParams inputL inputR =
    let
        stringUnconsLast =
            String.reverse
                >> String.uncons
                >> Maybe.map (Tuple.mapSecond String.reverse)

        stringConsLast ch =
            String.reverse
                >> String.cons ch
                >> String.reverse

        combineBothLists =
            List.foldr
                (\next prev ->
                    case ( prev, next ) of
                        ( Ok { leftLns, rightLns, fallbackLns, spacesOnlyLns }, Ok { left, right, fallback, spacesOnly } ) ->
                            Ok
                                { leftLns = left :: leftLns
                                , rightLns = right :: rightLns
                                , fallbackLns = fallback :: fallbackLns
                                , spacesOnlyLns = spacesOnlyLns && spacesOnly
                                }

                        ( Err list, Err elem ) ->
                            Err (elem :: list)

                        ( Err list, Ok { fallback } ) ->
                            Err (fallback :: list)

                        ( Ok { fallbackLns }, Err elem ) ->
                            Err (elem :: fallbackLns)
                )
                (Ok { leftLns = [], rightLns = [], fallbackLns = [], spacesOnlyLns = True })

        smushedLns =
            List.map2
                (\xs ys ->
                    case ( stringUnconsLast xs, String.uncons ys ) of
                        ( Just ( lastOfLeft, restOfLeft ), Just ( firstOfRight, restOfRight ) ) ->
                            ResultE.or
                                (smushSpaces ( lastOfLeft, firstOfRight )
                                    |> Result.map
                                        (\( smushedChar, smushDirection ) ->
                                            case smushDirection of
                                                LeftChar ->
                                                    { left = stringConsLast smushedChar restOfLeft
                                                    , right = restOfRight
                                                    , fallback = xs ++ ys
                                                    , spacesOnly = True
                                                    }

                                                RightChar ->
                                                    { left = restOfLeft
                                                    , right = String.cons smushedChar restOfRight
                                                    , fallback = xs ++ ys
                                                    , spacesOnly = True
                                                    }
                                        )
                                )
                                (smushChars fontParams ( lastOfLeft, firstOfRight )
                                    |> Result.map
                                        (\smushedChar ->
                                            { left = restOfLeft
                                            , right = String.cons smushedChar restOfRight
                                            , fallback = xs ++ ys
                                            , spacesOnly = False
                                            }
                                        )
                                )
                                |> Result.mapError (always (xs ++ ys))

                        _ ->
                            Err (xs ++ ys)
                )
                inputL
                inputR
    in
    case combineBothLists smushedLns of
        Ok { leftLns, rightLns, spacesOnlyLns } ->
            if spacesOnlyLns then
                smushLines fontParams leftLns rightLns

            else
                List.map2 (++) leftLns rightLns

        Err result ->
            result


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
            if counter == 0 then
                succeed (Done ())

            else
                succeed (Loop (counter - 1))
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


smushChars : FontParams -> ( Char, Char ) -> Result ( Char, Char ) Char
smushChars fontParams chars =
    equalCharacterSmushing chars
        |> ResultE.orElseLazy (\() -> underscoreSmushing chars)
        |> ResultE.orElseLazy (\() -> hierarchySmushing chars)
        |> ResultE.orElseLazy (\() -> oppositePairSmushing chars)
        |> ResultE.orElseLazy (\() -> bigXSmushing chars)
        |> ResultE.orElseLazy (\() -> hardblankSmushing fontParams.hardblank chars)


type SmushDirection
    = LeftChar
    | RightChar


smushSpaces : ( Char, Char ) -> Result ( Char, Char ) ( Char, SmushDirection )
smushSpaces ( ch1, ch2 ) =
    if ch1 == ' ' then
        Ok ( ch2, RightChar )

    else if ch2 == ' ' then
        Ok ( ch1, LeftChar )

    else
        Err ( ch1, ch2 )


equalCharacterSmushing : ( Char, Char ) -> Result ( Char, Char ) Char
equalCharacterSmushing ( ch1, ch2 ) =
    if ch1 == ch2 then
        Ok ch1

    else
        Err ( ch1, ch2 )


underscoreSmushing : ( Char, Char ) -> Result ( Char, Char ) Char
underscoreSmushing ( ch1, ch2 ) =
    if ch1 == '_' && isSubCharacter ch2 && ch2 /= '-' && ch2 /= '_' then
        Ok ch2

    else if ch2 == '_' && isSubCharacter ch1 && ch1 /= '-' && ch1 /= '_' then
        Ok ch1

    else
        Err ( ch1, ch2 )


hierarchySmushing : ( Char, Char ) -> Result ( Char, Char ) Char
hierarchySmushing ( ch1, ch2 ) =
    case ( subCharacterClass ch1, subCharacterClass ch2 ) of
        ( Just class1, Just class2 ) ->
            if class1 > class2 then
                Ok ch1

            else
                Ok ch2

        _ ->
            Err ( ch1, ch2 )


oppositePairSmushing : ( Char, Char ) -> Result ( Char, Char ) Char
oppositePairSmushing ( ch1, ch2 ) =
    if oppositePair ch1 == Just ch2 then
        Ok '|'

    else
        Err ( ch1, ch2 )


bigXSmushing : ( Char, Char ) -> Result ( Char, Char ) Char
bigXSmushing ( ch1, ch2 ) =
    if ch1 == '/' && ch2 == '\\' then
        Ok '|'

    else if ch1 == '\\' && ch2 == '/' then
        Ok 'V'

    else if ch1 == '>' && ch2 == '<' then
        Ok 'X'

    else
        Err ( ch1, ch2 )


hardblankSmushing : Char -> ( Char, Char ) -> Result ( Char, Char ) Char
hardblankSmushing hardblank ( ch1, ch2 ) =
    if ch1 == hardblank && ch2 == hardblank then
        Ok hardblank

    else
        Err ( ch1, ch2 )


isSubCharacter : Char -> Bool
isSubCharacter ch =
    List.member ch [ '-', '_', '|', '/', '\\', '[', ']', '{', '}', '(', ')', '<', '>' ]


subCharacterClass : Char -> Maybe Int
subCharacterClass ch =
    if ch == '|' then
        Just 1

    else if List.member ch [ '/', '\\' ] then
        Just 2

    else if List.member ch [ '[', ']' ] then
        Just 3

    else if List.member ch [ '{', '}' ] then
        Just 4

    else if List.member ch [ '(', ')' ] then
        Just 5

    else if List.member ch [ '<', '>' ] then
        Just 6

    else
        Nothing


oppositePair : Char -> Maybe Char
oppositePair ch =
    let
        pairs =
            [ ( '/', '\\' )
            , ( '[', ']' )
            , ( '{', '}' )
            , ( '(', ')' )
            , ( '<', '>' )
            ]
    in
    MaybeE.or
        (ListE.find (\( lefty, _ ) -> ch == lefty) pairs |> Maybe.map Tuple.second)
        (ListE.find (\( _, righty ) -> ch == righty) pairs |> Maybe.map Tuple.first)
