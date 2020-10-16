module Device exposing (Device(..), WindowSize, fromWindowSize, ifPhone)


type alias WindowSize =
    { width : Int
    , height : Int
    }


type Device
    = Desktop
    | Phone


fromWindowSize : WindowSize -> Device
fromWindowSize { width } =
    if width > 700 then
        Desktop

    else
        Phone


ifPhone : Device -> a -> a -> a
ifPhone device ifTrue ifFalse =
    case device of
        Phone ->
            ifTrue

        Desktop ->
            ifFalse
