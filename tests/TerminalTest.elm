module TerminalTest exposing (..)

import Expect exposing (Expectation)
import Screen exposing (Block(..), ScreenCommand(..))
import Terminal exposing (Key(..))
import Test exposing (..)


for : Int -> (a -> a) -> a -> a
for n fn x =
    List.foldl (\nextFn prevX -> nextFn prevX) x (List.repeat n fn)


suite : Test
suite =
    describe "Terminal"
        [ test "converts keypress into screen command"
            (\_ ->
                let
                    ( terminal, _ ) =
                        Terminal.init [] Nothing

                    expectedTerminal =
                        { terminal
                            | inputBuffer = [ 'a' ]
                            , promptCursor = 1
                        }
                in
                Expect.equal (Terminal.keyDown terminal (Character 'a'))
                    ( expectedTerminal, Print (NormalBlock "a") )
            )
        , test "parse command"
            (\_ ->
                Expect.equal
                    (Terminal.parseCommand "echo \"Hello World\" and good night")
                    (Ok ( "echo", [ "Hello World", "and", "good", "night" ] ))
            )
        ]
