module TerminalTest exposing (..)

import Expect exposing (Expectation)
import Screen exposing (Block(..), ScreenCommand(..))
import Terminal exposing (Key(..))
import Test exposing (..)
import Url


for : Int -> (a -> a) -> a -> a
for n fn x =
    List.foldl (\nextFn prevX -> nextFn prevX) x (List.repeat n fn)


suite : Test
suite =
    describe "Terminal"
        [ test "parse command"
            (\_ ->
                Expect.equal
                    (Terminal.parseCommand "echo \"Hello World\" and good night")
                    (Ok ( "echo", [ "Hello World", "and", "good", "night" ] ))
            )

        -- , test "parse &&"
        --     (\_ ->
        --         Expect.equal
        --             (Terminal.parseCommand "echo Hello World && echo good night")
        --             (Ok ( "echo", [ "Hello World", "and", "good", "night" ] ))
        --     )
        , test "URL builder and parser are isomorphic for simple commands"
            (\_ ->
                let
                    originalCommand =
                        ( "echo", [ "Hello World", "and", "good", "night" ] )
                in
                Expect.equal
                    (("http://localhost" ++ Terminal.buildCommandUrl originalCommand)
                        |> Url.fromString
                        |> Maybe.andThen Terminal.parseCommandUrl
                    )
                    (Just originalCommand)
            )
        ]
