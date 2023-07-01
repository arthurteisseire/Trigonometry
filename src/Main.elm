module Main exposing (..)

import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
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
    = Discard
    | AddVector
    | ModifyVector VectorId Vector2


type alias VectorId =
    Int


type alias Model =
    { vectors : Dict VectorId Vector2
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { vectors = Dict.empty }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Discard ->
            model

        AddVector ->
            { model | vectors = Dict.insert (Dict.size model.vectors) { x = 0, y = 0 } model.vectors }

        ModifyVector id vector ->
            { model
                | vectors =
                    Dict.insert id vector model.vectors
            }


view : Model -> Browser.Document Msg
view model =
    let
        nbCellsX =
            4

        nbCellsY =
            4
    in
    { title = "Unit circle"
    , body =
        [ Html.div
            [ HA.id "MainWindow"
            , HA.style "width" "100%"
            , HA.style "height" "800px"
            , HA.style "background-color" "#b3b3b3"
            ]
            [ svg
                [ transform <| [ Translate 100 100 ]
                , width <| percent 50
                , height <| percent 50
                , viewBox 0 0 nbCellsX nbCellsY
                ]
                [ g
                    [ transform <| [ Translate (nbCellsX / 2) (nbCellsY / 2) ]
                    , width <| percent 100
                    , height <| percent 100
                    ]
                    [ viewUnitCircle
                    , drawVectors model
                    ]
                ]
            , Html.div
                [ HA.id "DisplayDebug"
                , HA.style "width" "30%"
                , HA.style "height" "100%"
                , HA.style "float" "right"
                , HA.style "border-style" "solid"
                , HA.style "border-width" "1px"
                ]
                [ Html.div
                    [ HA.id "ComponentsDebug"
                    , HA.style "height" "100%"
                    , HA.style "display" "flex"
                    , HA.style "flex-direction" "column"
                    , HA.style "justify-content" "start"
                    ]
                    [ Html.button
                        [ HE.onClick <| AddVector
                        ]
                        [ Html.text "Add vector"
                        ]
                    , Html.div
                        []
                        (dictMapToList
                            (\id vector ->
                                Html.section
                                    [ HA.style "border-style" "solid"
                                    , HA.style "border-width" "1px"
                                    ]
                                    [ v2ToHtml id vector
                                    ]
                            )
                            model.vectors
                        )
                    ]
                ]
            ]
        ]
    }


dictMapToList : (k -> v -> a) -> Dict k v -> List a
dictMapToList func dict =
    Dict.foldl
        (\k v list -> func k v :: list)
        []
        dict


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


drawVectors : Model -> Svg msg
drawVectors model =
    g
        []
        (dictMapToList (\_ -> svgVector 0.03 Color.purple) model.vectors)


v2ToHtml : VectorId -> Vector2 -> Html Msg
v2ToHtml id v =
    Html.div
        []
        [ Html.text <| "x=" ++ String.fromFloat v.x
        , Html.input
            [ HA.type_ "float"
            , HE.onInput
                (\str ->
                    case String.toFloat str of
                        Nothing ->
                            Discard

                        Just value ->
                            ModifyVector id { v | x = value }
                )
            ]
            []
        , Html.text <| "y=" ++ String.fromFloat v.y
        , Html.input
            [ HA.type_ "float"
            , HE.onInput
                (\str ->
                    case String.toFloat str of
                        Nothing ->
                            Discard

                        Just value ->
                            ModifyVector id { v | y = value }
                )
            ]
            []
        ]


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


defaultSvgVectorWithSquare : Vector2 -> Svg msg
defaultSvgVectorWithSquare v =
    g []
        [ defaultSvgVector v
        , svgVector 0.01 Color.green { x = v.x, y = 0 }
        , svgVector 0.01 Color.green { x = 0, y = v.y }
        , line
            [ xp1 v.x
            , yp1 0
            , xp2 v.x
            , yp2 v.y
            , stroke <| Paint Color.purple
            , strokeWidth <| px 0.01
            ]
            []
        , line
            [ xp1 0
            , yp1 v.y
            , xp2 v.x
            , yp2 v.y
            , stroke <| Paint Color.purple
            , strokeWidth <| px 0.01
            ]
            []
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
        []
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
