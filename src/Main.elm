module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (Error)
import Browser.Navigation as Nav
import Dict
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
    | CheckResult Verdict


type alias Model =
    { str : String, checked : Bool, result : Res }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { str = "a", checked = False, result = CheckResult AlwaysTrue }, Cmd.none )


type Msg
    = Change String
    | Check
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change str ->
            let
                replacedStr =
                    str
                        |> String.replace "and" "∧"
                        |> String.replace "or" "∨"
                        |> String.replace "not" "¬"
                        |> String.replace "imply" "⇒"
                        |> String.replace "implies" "⇒"
                        |> String.replace "eq" "≡"
                        |> String.replace "==" "≡"
            in
            ( { model | str = replacedStr, checked = False }, Cmd.none )

        Check ->
            let
                result =
                    case run expr model.str of
                        Err _ ->
                            ParseError

                        Ok expression ->
                            CheckResult <| check expression
            in
            ( { model | checked = True, result = result }, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        signButton options children =
            button
                ([ stWidth "40px"
                 , backgroundColor "#F1F1F8"
                 , stHeight "40px"
                 , borderRadius "5px"
                 , margin "10px"
                 , fontFamily "monospace"
                 , fontSize "14pt"
                 ]
                    ++ options
                )
                children

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
                , rowDiv [ marginTop "20px", alignCenter, fontFamily "monospace" ]
                    [ text "Type the following to input logic operators: " ]
                , rowDiv [ marginTop "10px", alignCenter, fontFamily "monospace" ]
                    [ signButton []
                        [ text "∨" ]
                    , div [ marginRight "18px" ] [ text "or" ]
                    , signButton []
                        [ text "∧" ]
                    , div [ marginRight "18px" ] [ text "and" ]
                    , signButton []
                        [ text "¬" ]
                    , div [ marginRight "18px" ] [ text "not" ]
                    , signButton []
                        [ text "≡" ]
                    , div [ marginRight "18px" ] [ text "eq/==" ]
                    , signButton []
                        [ text "⇒" ]
                    , div [ marginRight "18px" ] [ text "implies/imply" ]
                    ]
                , if model.checked then
                    let
                        ( txt, clr ) =
                            case model.result of
                                ParseError ->
                                    ( "Parse error!", "blue" )

                                CheckResult AlwaysTrue ->
                                    ( "Always true", "green" )

                                CheckResult (NotAlwaysTrue valMap) ->
                                    let
                                        toStr (k, v) =
                                            k
                                                ++ " = "
                                                ++ (if v then
                                                        "True"

                                                    else
                                                        "False"
                                                   )

                                        contradiction =
                                            Dict.toList valMap
                                                |> List.map toStr
                                                |> String.join ", "
                                    in
                                    ( "Not always true with " ++ contradiction, "red" )
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
