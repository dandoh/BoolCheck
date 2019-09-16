module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Error)
import Browser.Navigation as Nav
import Expr exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Parser exposing (run)
import Styles exposing (..)
import Task
import Url


type Res
    = ParseError
    | Result Bool


type alias Model =
    { str : String, checked : Bool, result : Res }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { str = "a", checked = False, result = Result True }, Cmd.none )


type Msg
    = Change String
    | Check
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Symbol String
    | FocusResult (Result Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change str ->
            ( { model | str = str, checked = False }, Cmd.none )

        Check ->
            let
                result =
                    case run expr model.str of
                        Err _ ->
                            ParseError

                        Ok expression ->
                            Result <| isTrue expression
            in
            ( { model | checked = True, result = result }, Cmd.none )

        Symbol s ->
            ( { model | checked = False, str = model.str ++ s }, Dom.focus "input" |> Task.attempt FocusResult )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ colDiv [ stHeight "100vh", alignCenter, paddingLeft "60px", paddingRight "60px" ]
                [ div
                    [ fontFamily "monospace"
                    , fontSize "40pt"
                    , fontWeight "700"
                    , marginBottom "80px"
                    , marginTop "10%"
                    ]
                    [ text "Boolean Check" ]
                , rowDiv [ fullWidth ]
                    [ input
                        [ flex "1"
                        , id "input"
                        , onInput Change
                        , backgroundColor "#F1F1F8"
                        , fontFamily "monospace"
                        , fontSize "30pt"
                        , value model.str
                        , placeholder "Your boolean expression here"
                        , stHeight "70pt"
                        , borderRadius "5px"
                        , border "none"
                        , padding "10px"
                        ]
                        []
                    , button
                        [ stWidth "200px"
                        , marginLeft "20px"
                        , backgroundColor
                            (if model.checked then
                                "gray"

                             else
                                "#4CAF50"
                            )
                        , color "white"
                        , border "none"
                        , fontFamily "monospace"
                        , fontSize "30pt"
                        , borderRadius "5px"
                        , onClick Check
                        , disabled model.checked
                        ]
                        [ text "Check" ]
                    ]
                , rowDiv [ marginTop "20px" ]
                    [ button
                        [ stWidth "70px"
                        , backgroundColor "#F1F1F8"
                        , stHeight "70px"
                        , borderRadius "5px"
                        , margin "10px"
                        , onClick (Symbol "∨")
                        , fontFamily "monospace"
                        , fontSize "30pt"
                        ]
                        [ text "∨" ]
                    , button
                        [ stWidth "70px"
                        , backgroundColor "#F1F1F8"
                        , stHeight "70px"
                        , borderRadius "5px"
                        , margin "10px"
                        , fontFamily "monospace"
                        , fontSize "30pt"
                        , onClick (Symbol "∧")
                        ]
                        [ text "∧" ]
                    , button
                        [ stWidth "70px"
                        , backgroundColor "#F1F1F8"
                        , stHeight "70px"
                        , borderRadius "5px"
                        , margin "10px"
                        , fontFamily "monospace"
                        , fontSize "30pt"
                        , onClick (Symbol "¬")
                        ]
                        [ text "¬" ]
                    , button
                        [ stWidth "70px"
                        , backgroundColor "#F1F1F8"
                        , stHeight "70px"
                        , borderRadius "5px"
                        , margin "10px"
                        , fontFamily "monospace"
                        , onClick (Symbol "≡")
                        , fontSize "30pt"
                        ]
                        [ text "≡" ]
                    , button
                        [ stWidth "70px"
                        , backgroundColor "#F1F1F8"
                        , stHeight "70px"
                        , borderRadius "5px"
                        , margin "10px"
                        , fontFamily "monospace"
                        , fontSize "30pt"
                        , onClick (Symbol "⇒")
                        ]
                        [ text "⇒" ]
                    ]
                , if model.checked then
                    let
                        ( txt, clr ) =
                            case model.result of
                                ParseError ->
                                    ( "Parse error!", "blue" )

                                Result True ->
                                    ( "Always true", "green" )

                                Result False ->
                                    ( "Not always true", "red" )
                    in
                    div
                        [ fontFamily "monospace"
                        , color clr
                        , fontSize "40pt"
                        , fontWeight "700"
                        , marginBottom "80px"
                        , marginTop "40px"
                        ]
                        [ text txt ]

                  else
                    span [] []
                ]
            ]
    in
    { title = "Boolean check", body = body }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
