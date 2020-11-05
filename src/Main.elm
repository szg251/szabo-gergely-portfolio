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
                , screenWidth = Screen.pxToWidth flags.windowSize.width
                }

        ( screenModel, cmd ) =
            Screen.init screenCmd
    in
    ( { key = key
      , page = Initializing
      , nextUrl = url
      , device = Device.fromWindowSize flags.windowSize
      , screenModel = screenModel
      , terminalModel = terminalModel
      }
    , cmd |> Cmd.map ScreenMsg
    )


type Msg
    = NoOp
    | WindowSizeChanged { width : Int, height : Int }
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | TerminalMsg Terminal.Msg
    | ScreenMsg Screen.Msg


type Page
    = Initializing
    | Home
    | Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowSizeChanged windowSize ->
            updateTerminal
                (Terminal.ScreenWidthChanged (Screen.pxToWidth windowSize.width))
                ( { model | device = Device.fromWindowSize windowSize }, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            loadPage { model | nextUrl = url }

        TerminalMsg terminalMsg ->
            updateTerminal terminalMsg ( model, Cmd.none )

        ScreenMsg screenMsg ->
            updateScreen screenMsg ( model, Cmd.none )


updateScreen : Screen.Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateScreen screenMsg ( model, prevCmd ) =
    let
        ( updatedScreenModel, cmd ) =
            Screen.update screenMsg model.screenModel
    in
    ( { model | screenModel = updatedScreenModel }
    , Cmd.batch
        [ Cmd.map ScreenMsg cmd
        , prevCmd
        ]
    )


updateTerminal : Terminal.Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateTerminal terminalMsg ( model, prevCmd ) =
    let
        ( updatedTerminalModel, screenMsg, cmdT ) =
            Terminal.update terminalMsg model.terminalModel
    in
    updateScreen (Screen.AppendCommand screenMsg)
        ( { model | terminalModel = updatedTerminalModel }
        , Cmd.batch
            [ prevCmd
            , Cmd.map TerminalMsg cmdT
            ]
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
        [ Browser.Events.onResize (\w h -> WindowSizeChanged { width = w, height = h })
        , Browser.Events.onAnimationFrame (always (ScreenMsg Screen.Tick))
        , Browser.Events.onKeyDown (Decode.map (Terminal.KeyDown >> TerminalMsg) Terminal.keyDecoder)
        ]
