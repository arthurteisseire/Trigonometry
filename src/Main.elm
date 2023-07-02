module Main exposing (..)

import Browser
import Color exposing (Color)
import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (..)
import Vector2 exposing (Vector2)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { vectors : Dict VectorId (Vector2 Float)
    , vectorControllers : Dict VectorId VectorController
    }


type alias VectorId =
    Int


vectorIdToString : VectorId -> String
vectorIdToString =
    String.fromInt


type VectorController
    = PositionController
    | IdRefController VectorId VectorId



-- Init


init : () -> ( Model, Cmd SideMsg )
init _ =
    ( { vectors = Dict.empty, vectorControllers = Dict.empty }, Cmd.none )



-- Update


type SideMsg
    = Msg Msg
    | Save
    | RequestLoad
    | Load File
    | Loaded String
    | Reset


type Msg
    = Discard
    | AddVector
    | ModifyVector VectorId (Vector2 Float)
    | ChangeVectorController VectorId VectorController
    | ModifyIdRefController VectorId ( VectorId, VectorId )


update : SideMsg -> Model -> ( Model, Cmd SideMsg )
update msg model =
    case msg of
        Msg m ->
            ( updateModel m model |> updateIdRefsControllers, Cmd.none )

        RequestLoad ->
            ( model
            , File.Select.file
                [ "text/json" ]
                Load
            )

        Load file ->
            ( model, Task.perform Loaded (File.toString file) )

        Loaded s ->
            ( case Decode.decodeString modelJsonDecoder s of
                Ok loadedModel ->
                    loadedModel

                Err e ->
                    model
            , Cmd.none
            )

        Save ->
            ( model
            , File.Download.string
                "save.json"
                "text/json"
                (Encode.encode 4 (modelToJson model))
            )

        Reset ->
            init ()


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Discard ->
            model

        AddVector ->
            { model
                | vectors =
                    Dict.insert (Dict.size model.vectors) { x = 0, y = 0 } model.vectors
                , vectorControllers =
                    Dict.insert (Dict.size model.vectorControllers) PositionController model.vectorControllers
            }

        ModifyVector id vector ->
            { model
                | vectors =
                    Dict.insert id vector model.vectors
            }

        ChangeVectorController id controller ->
            { model | vectorControllers = Dict.insert id controller model.vectorControllers }

        ModifyIdRefController id ( id1, id2 ) ->
            { model
                | vectorControllers = Dict.insert id (IdRefController id1 id2) model.vectorControllers
            }


updateIdRefsControllers : Model -> Model
updateIdRefsControllers model =
    Dict.foldl
        (\id controller accModel ->
            case controller of
                IdRefController id1 id2 ->
                    Maybe.map2
                        (\v1 v2 ->
                            { accModel | vectors = Dict.insert id (Vector2.add v1 v2) accModel.vectors }
                        )
                        (Dict.get id1 model.vectors)
                        (Dict.get id2 model.vectors)
                        |> Maybe.withDefault accModel

                PositionController ->
                    accModel
        )
        model
        model.vectorControllers



-- View


view : Model -> Browser.Document SideMsg
view model =
    { title = "Unit circle"
    , body =
        [ Html.div
            [ HA.id "MainWindow"
            , HA.style "width" "100%"
            , HA.style "height" "800px"
            , HA.style "background-color" "#b3b3b3"
            ]
            [ optionView model
            , Html.map Msg (viewport model)
            ]
        ]
    }


optionView : Model -> Html SideMsg
optionView model =
    Html.div
        []
        [ Html.button
            [ HE.onClick Save ]
            [ Html.text "Save" ]
        , Html.button
            [ HE.onClick RequestLoad ]
            [ Html.text "Load" ]
        , Html.button
            [ HE.onClick Reset ]
            [ Html.text "Reset" ]
        ]


viewport : Model -> Html Msg
viewport model =
    let
        nbCellsX =
            4

        nbCellsY =
            4
    in
    Html.div
        [ HA.style "width" "100%"
        , HA.style "height" "100%"
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
                [ drawUnitCircle
                , drawVectors model
                ]
            ]
        , Html.div
            [ HA.id "DisplayDebug"
            , HA.style "width" "40%"
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
                        (\id _ ->
                            Html.section
                                [ HA.style "border-style" "solid"
                                , HA.style "border-width" "1px"
                                ]
                                [ v2ControllerToHtml model id
                                ]
                        )
                        model.vectorControllers
                    )
                ]
            ]
        ]


drawVectors : Model -> Svg msg
drawVectors model =
    g
        []
        (dictMapToList (\_ -> svgVector 0.03 Color.purple) model.vectors)


v2ControllerToHtml : Model -> VectorId -> Html Msg
v2ControllerToHtml model id =
    Maybe.map2
        (\v controller ->
            Html.div
                []
                [ case controller of
                    PositionController ->
                        positionController id v

                    IdRefController id1 id2 ->
                        idRefController id ( id1, id2 )
                , Html.select
                    []
                    [ Html.option
                        [ HE.onClick (ChangeVectorController id PositionController)
                        , HA.selected <|
                            case controller of
                                PositionController ->
                                    True

                                _ ->
                                    False
                        ]
                        [ Html.text "Position" ]
                    , Html.option
                        [ HE.onClick (ChangeVectorController id (IdRefController -1 -1))
                        , HA.selected <|
                            case controller of
                                IdRefController _ _ ->
                                    True

                                _ ->
                                    False
                        ]
                        [ Html.text "IdRef" ]
                    ]
                ]
        )
        (Dict.get id model.vectors)
        (Dict.get id model.vectorControllers)
        |> Maybe.withDefault (Html.text "")


idRefController : VectorId -> ( VectorId, VectorId ) -> Html Msg
idRefController id ( refId1, refId2 ) =
    Html.div
        []
        [ Html.text <| "(id:" ++ vectorIdToString id ++ ")"
        , Html.text <| "id1=" ++ vectorIdToString refId1
        , Html.input
            [ HA.type_ "number"
            , HE.onInput
                (\str ->
                    case String.toInt str of
                        Nothing ->
                            Discard

                        Just value ->
                            ModifyIdRefController id ( value, refId2 )
                )
            ]
            []
        , Html.text <| "id2=" ++ vectorIdToString refId2
        , Html.input
            [ HA.type_ "number"
            , HE.onInput
                (\str ->
                    case String.toInt str of
                        Nothing ->
                            Discard

                        Just value ->
                            ModifyIdRefController id ( refId1, value )
                )
            ]
            []
        ]


positionController : VectorId -> Vector2 Float -> Html Msg
positionController id v =
    Html.div
        []
        [ Html.text <| "(id:" ++ vectorIdToString id ++ ")"
        , Html.text <| "x=" ++ String.fromFloat v.x
        , Html.input
            [ HA.type_ "number"
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
            [ HA.type_ "number"
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


drawUnitCircle : Svg msg
drawUnitCircle =
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
subscriptions _ =
    Sub.none



-- Svg vector helpers


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


defaultSvgVectorWithText : Vector2 Float -> Svg msg
defaultSvgVectorWithText v =
    defaultSvgVectorWithCustomText
        ("("
            ++ String.fromFloat v.x
            ++ ", "
            ++ String.fromFloat v.y
            ++ ")"
        )
        v


defaultSvgVectorWithCustomText : String -> Vector2 Float -> Svg msg
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


defaultSvgVectorWithSquare : Vector2 Float -> Svg msg
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


defaultSvgVector : Vector2 Float -> Svg msg
defaultSvgVector =
    svgVector 0.02 Color.blue


svgVector : StrokeWidth -> Color -> Vector2 Float -> Svg msg
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



-- Model Json decoder


modelJsonDecoder : Decoder Model
modelJsonDecoder =
    Decode.map2 Model
        vectorsDecoder
        vectorControllersDecoder


vectorControllersDecoder : Decoder (Dict VectorId VectorController)
vectorControllersDecoder =
    Decode.field "vectorControllers" (Decode.dict vectorControllerDecoder)
        |> vectorIdDictDecoder


vectorControllerDecoder : Decoder VectorController
vectorControllerDecoder =
    Decode.field "vectorController" <|
        Decode.oneOf
            [ positionControllerDecoder
            , idRefControllerDecoder
            ]


idRefControllerDecoder : Decoder VectorController
idRefControllerDecoder =
    Decode.field "idRef" <|
        Decode.map
            (\( id1, id2 ) -> IdRefController id1 id2)
            tupleDecoder


tupleDecoder : Decoder ( Int, Int )
tupleDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "id1" Decode.int)
        (Decode.field "id2" Decode.int)


positionControllerDecoder : Decoder VectorController
positionControllerDecoder =
    Decode.field "position" <|
        Decode.map
            (\_ -> PositionController)
            (Decode.null {})


vectorsDecoder : Decoder (Dict VectorId (Vector2 Float))
vectorsDecoder =
    Decode.field "vectors" (Decode.dict vectorDecoder)
        |> vectorIdDictDecoder


vectorIdDictDecoder : Decoder (Dict String a) -> Decoder (Dict VectorId a)
vectorIdDictDecoder =
    Decode.map
        (\dict ->
            Dict.foldl
                (\id value result ->
                    case String.toInt id of
                        Just i ->
                            Dict.insert i value result

                        Nothing ->
                            result
                )
                Dict.empty
                dict
        )


vectorDecoder : Decoder (Vector2 Float)
vectorDecoder =
    Decode.map2 Vector2
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)



-- Model Json Encoder


modelToJson : Model -> Encode.Value
modelToJson model =
    Encode.object
        [ ( "vectors"
          , Encode.dict
                vectorIdToString
                vector2FloatToJson
                model.vectors
          )
        , ( "vectorControllers"
          , Encode.dict
                vectorIdToString
                vectorControllerToJson
                model.vectorControllers
          )
        ]


vectorControllerToJson : VectorController -> Encode.Value
vectorControllerToJson controller =
    Encode.object
        [ ( "vectorController"
          , case controller of
                PositionController ->
                    positionControllerToJson

                IdRefController id1 id2 ->
                    idRefControllerToJson ( id1, id2 )
          )
        ]


positionControllerToJson : Encode.Value
positionControllerToJson =
    Encode.object
        [ ( "position", Encode.null )
        ]


idRefControllerToJson : ( VectorId, VectorId ) -> Encode.Value
idRefControllerToJson ( id1, id2 ) =
    Encode.object
        [ ( "idRef"
          , Encode.object
                [ ( "id1", Encode.int id1 )
                , ( "id2", Encode.int id2 )
                ]
          )
        ]


vector2FloatToJson : Vector2 Float -> Encode.Value
vector2FloatToJson v =
    Encode.object
        [ ( "x", Encode.float v.x )
        , ( "y", Encode.float v.y )
        ]



-- Svg helpers


xp : Float -> TypedSvg.Core.Attribute msg
xp length =
    x <| px length


xp1 : Float -> TypedSvg.Core.Attribute msg
xp1 length =
    x1 <| px length


xp2 : Float -> TypedSvg.Core.Attribute msg
xp2 length =
    x2 <| px length


yp : Float -> TypedSvg.Core.Attribute msg
yp length =
    y <| px -length


yp1 : Float -> TypedSvg.Core.Attribute msg
yp1 length =
    y1 <| px -length


yp2 : Float -> TypedSvg.Core.Attribute msg
yp2 length =
    y2 <| px -length



-- Dict helpers


dictMapToList : (k -> v -> a) -> Dict k v -> List a
dictMapToList func dict =
    Dict.foldl
        (\k v list -> func k v :: list)
        []
        dict
