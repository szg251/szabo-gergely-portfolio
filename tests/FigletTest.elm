module FigletTest exposing (..)

import Expect exposing (Expectation)
import Figlet exposing (fontCharParser)
import Figlet.Font
import Html exposing (header)
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "Figlet"
        [ test "fontParamsParser"
            (\_ ->
                let
                    headerLine =
                        "flf2a$ 6 5 16 15 11 0 24463\n"

                    expected =
                        Ok
                            { hardblank = '$'
                            , height = 6
                            , baseline = 5
                            , maxLength = 16
                            , oldLayout = 15
                            , commentLines = 11
                            , printDirection = 0
                            , fullLayout = 24463
                            , codetagCount = Nothing
                            }
                in
                Expect.equal (Parser.run Figlet.fontParamsParser headerLine) expected
            )
        , test "fontCharParser with required character"
            (\_ ->
                let
                    fontChar =
                        """  ____  @
 |___ \\ @
   __) |@
  / __/ @
 |_____|@
        @@
"""

                    expected =
                        Ok
                            { charCode = 50
                            , lines =
                                [ "  ____  "
                                , " |___ \\ "
                                , "   __) |"
                                , "  / __/ "
                                , " |_____|"
                                , "        "
                                ]
                            }
                in
                Expect.equal (Parser.run (Figlet.fontCharParser 18) fontChar) expected
            )
        , test "fontCharParser with code tagged character"
            (\_ ->
                let
                    fontChar =
                        """161  INVERTED EXCLAMATION MARK
  _ @
 (_)@
 | |@
 | |@
 |_|@
    @@
"""

                    expected =
                        Ok
                            { charCode = 161
                            , lines =
                                [ "  _ "
                                , " (_)"
                                , " | |"
                                , " | |"
                                , " |_|"
                                , "    "
                                ]
                            }
                in
                Expect.equal (Parser.run (Figlet.fontCharParser 18) fontChar) expected
            )
        , test "fontCharParser with hex code tagged character"
            (\_ ->
                let
                    fontChar =
                        """0x0102  LATIN CAPITAL LETTER A WITH BREVE
  _   _ @
  \\\\_// @
   /_\\  @
  / _ \\ @
 /_/ \\_\\@
        @@
"""

                    expected =
                        Ok
                            { charCode = 258
                            , lines =
                                [ "  _   _ "
                                , "  \\\\_// "
                                , "   /_\\  "
                                , "  / _ \\ "
                                , " /_/ \\_\\"
                                , "        "
                                ]
                            }
                in
                Expect.equal (Parser.run (Figlet.fontCharParser 18) fontChar) expected
            )
        , test "parsing standard font succeeds"
            (\_ ->
                Expect.ok (Parser.run Figlet.fontParser Figlet.Font.standard)
            )
        , test "toLines"
            (\_ ->
                let
                    lines =
                        Result.map
                            (\font ->
                                Figlet.toLines font "str"
                            )
                            (Parser.run Figlet.fontParser Figlet.Font.standard)

                    expected =
                        Ok
                            [ "        _          "
                            , "  ___  | |_   _ __ "
                            , " / __| | __| | '__|"
                            , " \\__ \\ | |_  | |   "
                            , " |___/  \\__| |_|   "
                            , "                   "
                            ]
                in
                Expect.equal lines expected
            )
        ]
