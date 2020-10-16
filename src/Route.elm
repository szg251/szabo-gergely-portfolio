module Route exposing (Route(..), parse, toUrl)

import Maybe.Extra as MaybeE
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Error Int


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Home ->
            Url.Builder.absolute [] []

        Error _ ->
            Url.Builder.absolute [ "error" ] []


parse : Url -> Route
parse url =
    Url.Parser.parse routeParser url
        |> Maybe.withDefault (Error 404)
