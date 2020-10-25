module ScreenTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Screen exposing (Block(..), Command(..), Line(..))
import Test exposing (..)


for : Int -> (a -> a) -> a -> a
for n fn x =
    List.foldl (\nextFn prevX -> nextFn prevX) x (List.repeat n fn)


suite : Test
suite =
    describe "Screen"
        [ test "displays 1 character/animation frame"
            (\_ ->
                let
                    screen =
                        Screen.init (Batch [ Print (NormalBlock "012345678890123456789") ])

                    updated =
                        for 5 Screen.tick screen

                    expectedOutput =
                        [ Line [ NormalBlock [ '4', '3', '2', '1', '0' ] ]
                        ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        , test "displays line breaks"
            (\_ ->
                let
                    screen =
                        Screen.init
                            (Batch
                                [ Print (NormalBlock "0123")
                                , EndOfLine
                                , Print (NormalBlock "4567")
                                ]
                            )

                    updated =
                        for 10 Screen.tick screen

                    expectedOutput =
                        [ Line [ NormalBlock [ '7', '6', '5', '4' ] ]
                        , Line [ NormalBlock [ '3', '2', '1', '0' ] ]
                        ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        , test "moving cursor reorders blocks"
            (\_ ->
                let
                    screen =
                        Screen.init
                            (Batch
                                [ Print (NormalBlock "4567")
                                , MoveCursor ( 0, 0 )
                                , Print (Link ( "https://example.com", "0123" ))
                                ]
                            )

                    updated =
                        for 10 Screen.tick screen

                    expectedOutput =
                        [ Line
                            [ NormalBlock [ '7', '6', '5', '4' ]
                            , Link ( "https://example.com", [ '3', '2', '1', '0' ] )
                            ]
                        ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        , test "moving cursor merges same block types"
            (\_ ->
                let
                    screen =
                        Screen.init
                            (Batch
                                [ Print (NormalBlock "4567")
                                , MoveCursor ( 0, 0 )
                                , Print (NormalBlock "0123")
                                ]
                            )

                    updated =
                        for 10 Screen.tick screen

                    expectedOutput =
                        [ Line [ NormalBlock [ '7', '6', '5', '4', '3', '2', '1', '0' ] ] ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        , test "moving cursor splits different block types"
            (\_ ->
                let
                    screen =
                        Screen.init
                            (Batch
                                [ Print (NormalBlock "4567")
                                , MoveCursor ( 0, 2 )
                                , Print (Link ( "https://example.com", "0123" ))
                                ]
                            )

                    updated =
                        for 10 Screen.tick screen

                    expectedOutput =
                        [ Line
                            [ NormalBlock [ '7', '6' ]
                            , Link ( "https://example.com", [ '3', '2', '1', '0' ] )
                            , NormalBlock [ '5', '4' ]
                            ]
                        ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        , test "moving cursor fills in empty columns "
            (\_ ->
                let
                    screen =
                        Screen.init
                            (Batch
                                [ Print (NormalBlock "4567")
                                , MoveCursor ( 0, 6 )
                                , Print (NormalBlock "0123")
                                ]
                            )

                    updated =
                        for 10 Screen.tick screen

                    expectedOutput =
                        [ Line [ NormalBlock [ '3', '2', '1', '0', ' ', ' ', '7', '6', '5', '4' ] ]
                        ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        , test "moving cursor fills in empty columns and lines"
            (\_ ->
                let
                    screen =
                        Screen.init
                            (Batch
                                [ Print (NormalBlock "4567")
                                , MoveCursor ( 2, 2 )
                                , Print (NormalBlock "0123")
                                ]
                            )

                    updated =
                        for 10 Screen.tick screen

                    expectedOutput =
                        [ Line [ NormalBlock [ '3', '2', '1', '0', ' ', ' ' ] ]
                        , Line []
                        , Line [ NormalBlock [ '7', '6', '5', '4' ] ]
                        ]
                in
                Expect.equal updated.visibleOutput expectedOutput
            )
        ]
