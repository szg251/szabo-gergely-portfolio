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
import Screen exposing (Block(..), ScreenCommand(..))
import Script
import Terminal
import Time
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
        screenWidth =
            Screen.pxToWidth flags.windowSize.width

        screenHeight =
            Screen.pxToHeight flags.windowSize.height

        initCommand =
            Terminal.parseCommandUrl url
                |> Maybe.andThen (Terminal.parseCommand >> Result.toMaybe)
                |> Maybe.withDefault [ ( "home", [] ) ]

        ( terminalModel, screenCmd ) =
            Terminal.init
                { commands =
                    [ ( "echo", Command.echo )
                    , ( "figlet", Figlet.run )
                    , ( "clear", Command.clear )
                    , ( "menu", Command.menu )
                    , ( "link", Command.link )
                    , ( "home", Script.home )
                    , ( "bio", Script.bio )
                    , ( "projects", Script.projects )
                    , ( "music", Script.music )
                    ]
                , initCommand = Just initCommand
                , navKey = key
                , screenWidth = screenWidth
                }

        ( screenModel, cmd ) =
            Screen.init
                { command = screenCmd
                , screenWidth = screenWidth
                , screenHeight = screenHeight
                }
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
            let
                screenWidth =
                    Screen.pxToWidth windowSize.width

                screenHeight =
                    Screen.pxToHeight windowSize.height
            in
            ( { model | device = Device.fromWindowSize windowSize }, Cmd.none )
                |> updateTerminal (Terminal.ScreenWidthChanged screenWidth)
                |> updateScreen
                    (Screen.ScreenSizeChanged
                        { screenWidth = screenWidth, screenHeight = screenHeight }
                    )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            updateTerminal
                (Terminal.UrlChanged url)
                ( { model | nextUrl = url }, Cmd.none )

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
        , Time.every 500 (always (ScreenMsg Screen.BlinkCursor))
        , Browser.Events.onKeyDown
            (Decode.map
                (\key ->
                    if key == Terminal.Enter && model.screenModel.command /= Screen.NoCommand then
                        ScreenMsg Screen.Flush

                    else
                        TerminalMsg (Terminal.KeyDown key)
                )
                Terminal.keyDecoder
            )
        ]
