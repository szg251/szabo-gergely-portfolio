module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Device exposing (Device, WindowSize)
import Html.Styled
import Http
import Json.Decode exposing (decodeValue)
import ProfileImage
import Route
import Screen exposing (Block(..), Command(..))
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
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , page = Initializing
      , nextUrl = url
      , device = Device.fromWindowSize flags.windowSize
      , screenModel =
            Screen.init
                (Screen.batch
                    [ Screen.printLn "╔═══════════════╗"
                    , Screen.print "║ "
                    , Screen.printLink "https://szabogergely.com" "Szabo Gergely"
                    , Screen.printLn " ║"
                    , Screen.printLn "╚═══════════════╝"
                    , ProfileImage.print ( 10, 0 )
                    ]
                )
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | GotWindowSize { width : Int, height : Int }
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Tick


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
        ]
