module Main exposing (..)

import Browser
import Color
import Html exposing (Html)
import Html.Attributes as HA
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (..)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = Update


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "ECS"
    , body =
        [ Html.div
            [ HA.id "MainWindow"
            , HA.style "width" "100%"
            , HA.style "height" "955px"
            , HA.style "background-color" "#b3b3b3"
            ]
            [ svg
                [ transform <| [ Translate 200 200 ]
                , width <| px 800
                , height <| px 600
                , viewBox 0 0 10 10
                ]
                [ viewUnitCircle
                ]
            ]
        ]
    }


viewUnitCircle : Svg msg
viewUnitCircle =
    g
        [ transform <| [ Translate 5 5 ]
        ]
        [ circle
            [ r <| px pi
            , fill <| Paint Color.grey
            ]
            []
        , line
            [ x1 <| px pi
            , y1 <| px 0
            , x2 <| px -pi
            , y2 <| px 0
            , stroke <| Paint Color.blue
            , strokeWidth <| px 0.05
            ]
            []
        , line
            [ x1 <| px 0
            , y1 <| px -pi
            , x2 <| px 0
            , y2 <| px pi
            , stroke <| Paint Color.blue
            , strokeWidth <| px 0.05
            ]
            []
        ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
