module CommandTest exposing (..)

import Command exposing (TextPart(..), parseEchoString)
import Expect exposing (Expectation)
import Screen.Color
import Test exposing (..)


suite : Test
suite =
    describe "Command"
        [ describe "parseEchoString"
            [ test "text"
                (\_ ->
                    Expect.equal
                        (Command.parseEchoString "Hello World")
                        (Ok [ Text "Hello World" ])
                )
            , test "line break"
                (\_ ->
                    Expect.equal
                        (Command.parseEchoString "Hello\\nWorld")
                        (Ok [ Text "Hello", LineBreak, Text "World" ])
                )
            ]
        , test "color change"
            (\_ ->
                Expect.equal
                    (Command.parseEchoString "Hello \\033[0;31mWorld\\033[0m")
                    (Ok
                        [ Text "Hello "
                        , ColorChange (Just Screen.Color.Red)
                        , Text "World"
                        , ColorChange Nothing
                        ]
                    )
            )
        ]
