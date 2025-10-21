module ScreenTest exposing (..)

import Expect exposing (Expectation)
import Screen exposing (Block(..), Line(..), VisibleOutput, batch, clearScreen, delete, moveCursor, print, printLink, printLn)
import Test exposing (..)


for : Int -> (a -> a) -> a -> a
for n fn x =
    List.foldl (\nextFn prevX -> nextFn prevX) x (List.repeat n fn)


suite : Test
suite =
    let
        testScreen =
            Screen.init { command = Screen.noCommand, screenHeight = 40, screenWidth = 80 }
                |> Tuple.first
    in
    describe "Screen"
        [ test "displays 1 character/animation frame"
            (\_ ->
                let
                    screen =
                        { testScreen | command = print "0123456789" }
                            |> for 5 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line [ NormalBlock [ '4', '3', '2', '1', '0' ] ]
                        ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "displays line breaks"
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ printLn "0123"
                                    , print "4567"
                                    ]
                        }
                            |> for 9 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line [ NormalBlock [ '7', '6', '5', '4' ] ]
                        , Line [ NormalBlock [ '3', '2', '1', '0' ] ]
                        ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "moving cursor reorders blocks"
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ print "4567"
                                    , moveCursor (always ( 0, 0 ))
                                    , printLink { url = "https://example.com", label = "0123", target = "" }
                                    ]
                        }
                            |> for 9 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line
                            [ NormalBlock [ '7', '6', '5', '4' ]
                            , Link ( "https://example.com", [ '3', '2', '1', '0' ] , "")
                            ]
                        ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "moving cursor merges same block types"
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ print "4567"
                                    , moveCursor (always ( 0, 0 ))
                                    , print "0123"
                                    ]
                        }
                            |> for 9 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line [ NormalBlock [ '7', '6', '5', '4', '3', '2', '1', '0' ] ] ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "moving cursor splits different block types"
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ print "4567"
                                    , moveCursor (always ( 0, 2 ))
                                    , printLink { url = "https://example.com", label = "0123", target = "" }
                                    ]
                        }
                            |> for 9 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line
                            [ NormalBlock [ '7', '6' ]
                            , Link ( "https://example.com", [ '3', '2', '1', '0' ] , "")
                            , NormalBlock [ '5', '4' ]
                            ]
                        ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "moving cursor fills in empty columns "
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ print "4567"
                                    , moveCursor (always ( 0, 6 ))
                                    , print "0123"
                                    ]
                        }
                            |> for 9 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line [ NormalBlock [ '3', '2', '1', '0', ' ', ' ', '7', '6', '5', '4' ] ]
                        ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "moving cursor fills in empty columns and lines"
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ print "4567"
                                    , moveCursor (always ( 2, 2 ))
                                    , print "0123"
                                    ]
                        }
                            |> for 9 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line [ NormalBlock [ '3', '2', '1', '0', ' ', ' ' ] ]
                        , Line []
                        , Line [ NormalBlock [ '7', '6', '5', '4' ] ]
                        ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "clear screen"
            (let
                screen =
                    { testScreen | command = batch [ print "01234", clearScreen ] }
                        |> for 6 (Screen.update Screen.EvalNextCommand >> Tuple.first)
             in
             Expect.all
                [ \_ -> Expect.equal screen.visibleOutput []
                , \_ -> Expect.equal screen.cursorPosition ( 0, 0 )
                ]
            )
        , test "delete character before cursor pointer"
            (\_ ->
                let
                    screen =
                        { testScreen
                            | command =
                                batch
                                    [ print "01234"
                                    , moveCursor (always ( 0, 2 ))
                                    , delete
                                    ]
                        }
                            |> for 8 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    expectedOutput =
                        [ Line [ NormalBlock [ '4', '3', '2', '0' ] ] ]
                in
                Expect.equal screen.visibleOutput expectedOutput
            )
        , test "splitVisibleOutput and mergeVisibleOutput are isomorphic"
            (\_ ->
                let
                    screen =
                        { testScreen | command = batch [ printLn "0123", print "4567" ] }
                            |> for 10 (Screen.update Screen.EvalNextCommand >> Tuple.first)

                    remergedOutput =
                        screen.visibleOutput
                            |> Screen.splitVisibleOutputAt ( 0, 3 )
                            |> Screen.mergeVisibleOutput
                in
                Expect.equal screen.visibleOutput remergedOutput
            )
        ]
