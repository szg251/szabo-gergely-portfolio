module ProfileImage exposing (..)

import Screen exposing (Command, CursorPosition, batch, moveCursor)


printImage : CursorPosition -> String -> Command
printImage ( ln, col ) string =
    let
        printLine lineCount line =
            line
                |> String.foldl
                    (\next ( spaces, word ) ->
                        if next == ' ' && String.isEmpty word then
                            ( spaces + 1, "" )

                        else
                            ( spaces, word ++ String.fromChar next )
                    )
                    ( 0, "" )
                |> (\( spaces, word ) ->
                        batch
                            [ moveCursor ( ln + lineCount, col + spaces )
                            , Screen.print word
                            ]
                   )
    in
    String.lines string
        |> List.indexedMap printLine
        |> batch


print : CursorPosition -> Command
print cursorPosition =
    printImage cursorPosition """
                                  %(##%//*////,.
                           ,%,//(%##%%%%((*%(((*#&(%%/(%@@@.
                           /%(*.(@&%@@&&#@@%@##@#& %#%@&/&@(.
                       .,,*,,,%/((/##,.*/,//.*// *%,/*#%%#(#/.
                       ..*,,.#*(* ..,..  . .....,..***.*/*/#&**.
                    ,*,*,/%&*(.*.  . . .  ., ..,...,,.*/**///*,***
                   .,*,**(%%(#(, ,,..   .    ,......,,..,**,//*****
                    .*,.*,#&/##(*,,*,,..  .,... .,... ,.,..,,***,***.
                    ,*,.*%(///****,,**.,*,.,*,...,..,.., ....,.,,,/
                    ,,,(***,...,,,,,,,,............. ... ...,, ,,#
                     / .,*,. ...  .*,,*****,,,***,,... .....,,.,,,*/
                        **/,.,... &%/.,,,,  .**.,**   .. ... (,..,///,/
                       , (%*,,,,.@@#, ..,,,,*,,,,,,..    . ...,/..***,./
                       ,,(#(,,,,@@%#**, /.**////**,,,,,,,.  ..***,/*,.**
                      ..,/%%#(&&##//*.,,,*,,,*********,,,....*,*/*/(/
                      ,*(/#((/*///* //*,,***/**/*****,,,,,,.,**,/ ./
                  *(@@@(@#*(#%*,,***,,,,,****//*****,,,,*,*,,, .
                   @@*.%&(*#(##%,.,,...,,,***////***,,,.***.&,
                   @*@&%/ /*,,. ,,**/********/*//****,..   ,@@
                   @@@@@/#@,((//....,,.,***//////***.  .  ,(@@@%
               @@@@@,&@@(#@,,,**,,*////***/*/*//***.  .. ,,,%%%@@@@@%
     .@@@@@@@@@@&%/,/(&&/#%*,,,,,******,*,***,..   . ,..,%&@&@@@@@@/@@
    *(@@@@@&@%@&@&(#((,.,##(@//////**@&%**/@&.,....(**(&*#,(,*#%@&(&#@@%@
    @@@#(&@@*(&@&@&&((/@@@@@@@@@@@@&((###&&&@@@@@@@@@&&&&@@&&##(#&%&&.@@#@&#
    &@@@&.(@@%(&&#(/@@@@@@@@@@@@@@@@@@@@@&(##&%(#(#%#%%&&@@@&%&*%(%%%&#(##%#%
    &%@@@#,,(&%@%&*(////(/((((((((/(##%%##@&%#@&@&%##@@@@@@@@@@@@@/#((@@(%@%&
#,/*@%&(#@@@@./*(/&(&&/*,,,,,,*,,,,,,,,,*/%&@@@@@@@&%&&&&&&&%#%##%%&@&&*(*.,%@
(***@@@@,/#&#@.(##@@@@(*@@@@@/,,.,,(%&&%%&&@@@@@@@&%((%##****//((##((//*. * @@
.. @#&@@&#,.*/%@/#@@&@@&&@&&&&@@@%(/**(*/@@@@@@@@@&&/@@@@@*,*@@@@@@@@@@&,.%@(*
..*&%*((@#% .,.*&@%#//((/**//((###%%%%&&&&&%%@&@@@@(,,(/*///((&&&&%&&@@@@@@@,*
***%@@%,, /(%@.&#//********************//*//**/(((((,,*....,,,****///((##%%%%,
         """
