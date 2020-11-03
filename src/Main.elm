module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Command
import Device exposing (Device, WindowSize)
import Figlet
import Html.Styled
import Http
import Json.Decode as Decode exposing (decodeValue)
import ProfileImage
import Route
import Screen exposing (Block(..), ScreenCommand(..))
import Terminal
import Url


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Flags =
    { windowSize : WindowSize
    }


type alias Model =
    { key : Nav.Key
    , page : Page
    , nextUrl : Url.Url
    , device : Device
    , screenModel : Screen.Model
    , terminalModel : Terminal.Model
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        initCommand =
            Terminal.parseCommandUrl url
                |> Maybe.withDefault ( "echo", [ "Hello World" ] )

        ( terminalModel, screenCmd ) =
            Terminal.init
                { commands =
                    [ ( "echo", Command.echo )
                    , ( "figlet", Figlet.run )
                    , ( "clear", Command.clear )
                    ]
                , initCommand = Just initCommand
                , navKey = key
                }
    in
    ( { key = key
      , page = Initializing
      , nextUrl = url
      , device = Device.fromWindowSize flags.windowSize
      , screenModel = Screen.init screenCmd
      , terminalModel = terminalModel
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | GotWindowSize { width : Int, height : Int }
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick
    | KeyPressed Terminal.Key


type Page
    = Initializing
    | Home
    | Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotWindowSize windowSize ->
            ( { model | device = Device.fromWindowSize windowSize }, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            loadPage { model | nextUrl = url }

        Tick ->
            ( { model | screenModel = Screen.tick model.screenModel }, Cmd.none )

        KeyPressed key ->
            let
                ( updatedTerminalModel, screenCmd, cmd ) =
                    Terminal.keyDown model.terminalModel key
            in
            ( { model
                | terminalModel = updatedTerminalModel
                , screenModel = Screen.appendCommand screenCmd model.screenModel
              }
            , cmd
            )


loadPage : Model -> ( Model, Cmd Msg )
loadPage model =
    let
        nextRoute =
            Route.parse model.nextUrl
    in
    case nextRoute of
        Route.Home ->
            ( { model | page = Home }, Cmd.none )

        Route.Error status ->
            ( { model | page = Error }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Szabo Gergely Portfolio"
    , body =
        Screen.view model.screenModel
            |> List.map Html.Styled.toUnstyled
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> GotWindowSize { width = w, height = h })
        , Browser.Events.onAnimationFrame (always Tick)
        , Browser.Events.onKeyDown (Decode.map KeyPressed Terminal.keyDecoder)
        , Browser.Events.onKeyPress (Decode.map KeyPressed Terminal.keyDecoder)
        ]
