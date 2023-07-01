module Main exposing (..)

import Browser
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HA
import TypedSvg exposing (..)
import TypedSvg.Attributes as AT exposing (..)
import TypedSvg.Core exposing (Svg, text)
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
                , viewBox 0 0 4 4
                ]
                [ viewUnitCircle
                ]
            ]
        ]
    }


xp length =
    x <| px length


xp1 length =
    x1 <| px length


xp2 length =
    x2 <| px length


yp length =
    y <| px -length


yp1 length =
    y1 <| px -length


yp2 length =
    y2 <| px -length


type alias Vector2 =
    { x : Float
    , y : Float
    }


type alias StrokeWidth =
    Float


defaultSvgDiagVectorWithTextDivPi : Float -> Float -> Svg msg
defaultSvgDiagVectorWithTextDivPi numerator denominator =
    let
        radian =
            numerator * pi / denominator
    in
    defaultSvgVectorWithCustomText
        ("("
            ++ (if numerator /= 1 then
                    String.fromFloat numerator

                else
                    ""
               )
            ++ "π"
            ++ (if denominator /= 1 then
                    "/" ++ String.fromFloat denominator

                else
                    ""
               )
            ++ ")"
        )
        { x = cos radian
        , y = sin radian
        }


defaultSvgVectorWithText : Vector2 -> Svg msg
defaultSvgVectorWithText v =
    defaultSvgVectorWithCustomText
        ("("
            ++ String.fromFloat v.x
            ++ ", "
            ++ String.fromFloat v.y
            ++ ")"
        )
        v


defaultSvgVectorWithCustomText : String -> Vector2 -> Svg msg
defaultSvgVectorWithCustomText s v =
    g
        []
        [ defaultSvgVector v
        , text_
            [ xp v.x
            , yp v.y
            , fontSize <| px 0.12
            ]
            [ text s ]
        ]


defaultSvgVector : Vector2 -> Svg msg
defaultSvgVector =
    svgVector 0.02 Color.blue


svgVector : StrokeWidth -> Color -> Vector2 -> Svg msg
svgVector strokeWidth_ color v =
    line
        [ xp1 0
        , yp1 0
        , xp2 v.x
        , yp2 v.y
        , stroke <| Paint color
        , strokeWidth <| px strokeWidth_
        ]
        []


viewUnitCircle : Svg msg
viewUnitCircle =
    g
        [ transform <| [ Translate 2 2 ]
        ]
        [ circle
            [ r <| px 1
            , fill <| Paint Color.grey
            ]
            []
        , defaultSvgVectorWithText { x = -1, y = 0 }
        , defaultSvgVectorWithText { x = 1, y = 0 }
        , defaultSvgVectorWithText { x = 0, y = -1 }
        , defaultSvgVectorWithText { x = 0, y = 1 }
        , defaultSvgDiagVectorWithTextDivPi 1 6
        , defaultSvgDiagVectorWithTextDivPi 1 3
        , defaultSvgDiagVectorWithTextDivPi 4 6
        , defaultSvgDiagVectorWithTextDivPi 5 6
        , defaultSvgDiagVectorWithTextDivPi 1 1
        , defaultSvgDiagVectorWithTextDivPi -1 6
        , defaultSvgDiagVectorWithTextDivPi -1 3
        , defaultSvgDiagVectorWithTextDivPi -4 6
        , defaultSvgDiagVectorWithTextDivPi -5 6
        ]


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
