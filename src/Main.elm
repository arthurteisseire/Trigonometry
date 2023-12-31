module Main exposing (..)

import Bitwise exposing (shiftLeftBy)
import Browser
import Color exposing (Color(..))
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
    , vectorColors : Dict VectorId Color
    }


type alias VectorId =
    Int


vectorIdToString : VectorId -> String
vectorIdToString =
    String.fromInt


type VectorController
    = PositionController
    | OperationController Operation VectorId VectorId


type Operation
    = Add
    | Sub



-- Init


init : () -> ( Model, Cmd SideMsg )
init _ =
    ( { vectors = Dict.empty
      , vectorControllers = Dict.empty
      , vectorColors = Dict.empty
      }
    , Cmd.none
    )



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
    | RemoveVector VectorId
    | ModifyVector VectorId (Vector2 Float)
    | ChangeVectorColor VectorId Color
    | ChangeVectorController VectorId VectorController
    | ModifyOperationController VectorId ( VectorId, VectorId )
    | ChangeOperationInController VectorId Operation


update : SideMsg -> Model -> ( Model, Cmd SideMsg )
update msg model =
    case msg of
        Msg m ->
            ( updateModel m model |> updateOperationsControllers, Cmd.none )

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
                , vectorColors =
                    Dict.insert (Dict.size model.vectorColors) Color.purple model.vectorColors
            }

        RemoveVector id ->
            { model
                | vectors = Dict.remove id model.vectors
                , vectorControllers = Dict.remove id model.vectorControllers
                , vectorColors = Dict.remove id model.vectorColors
            }

        ModifyVector id vector ->
            { model
                | vectors =
                    Dict.insert id vector model.vectors
            }

        ChangeVectorColor id color ->
            { model
                | vectorColors =
                    Dict.insert id color model.vectorColors
            }

        ChangeVectorController id controller ->
            { model | vectorControllers = Dict.insert id controller model.vectorControllers }

        ModifyOperationController id ( id1, id2 ) ->
            { model
                | vectorControllers =
                    Maybe.map
                        (\controller ->
                            case controller of
                                OperationController op _ _ ->
                                    Dict.insert id
                                        (OperationController op id1 id2)
                                        model.vectorControllers

                                _ ->
                                    model.vectorControllers
                        )
                        (Dict.get id model.vectorControllers)
                        |> Maybe.withDefault model.vectorControllers
            }

        ChangeOperationInController id op ->
            { model
                | vectorControllers =
                    Maybe.map
                        (\controller ->
                            case controller of
                                OperationController _ id1 id2 ->
                                    Dict.insert id
                                        (OperationController op id1 id2)
                                        model.vectorControllers

                                _ ->
                                    model.vectorControllers
                        )
                        (Dict.get id model.vectorControllers)
                        |> Maybe.withDefault model.vectorControllers
            }


updateOperationsControllers : Model -> Model
updateOperationsControllers model =
    Dict.foldl
        (\id controller accModel ->
            case controller of
                OperationController op id1 id2 ->
                    Maybe.map2
                        (\v1 v2 ->
                            case op of
                                Add ->
                                    { accModel | vectors = Dict.insert id (Vector2.add v1 v2) accModel.vectors }

                                Sub ->
                                    { accModel | vectors = Dict.insert id (Vector2.sub v1 v2) accModel.vectors }
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
        (dictMapToList
            (\id ->
                case Dict.get id model.vectorColors of
                    Just color ->
                        svgVector 0.03 color

                    Nothing ->
                        svgVector 0.03 Color.purple
            )
            model.vectors
        )


v2ControllerToHtml : Model -> VectorId -> Html Msg
v2ControllerToHtml model id =
    Maybe.map2
        (\v controller ->
            Html.div
                []
                [ case controller of
                    PositionController ->
                        positionController id v

                    OperationController op id1 id2 ->
                        operationController id op ( id1, id2 )
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
                        [ HE.onClick (ChangeVectorController id (OperationController Add -1 -1))
                        , HA.selected <|
                            case controller of
                                OperationController _ _ _ ->
                                    True

                                _ ->
                                    False
                        ]
                        [ Html.text "Operation" ]
                    ]
                , Html.input
                    [ HA.type_ "color"
                    , case Dict.get id model.vectorColors of
                        Just color ->
                            HA.value (toHex color).hex

                        Nothing ->
                            HA.value "#000000"
                    , HE.onInput
                        (\s ->
                            case colorFromHex s of
                                Just color ->
                                    ChangeVectorColor id color

                                Nothing ->
                                    Discard
                        )
                    ]
                    []
                , Html.button
                    [ HE.onClick (RemoveVector id)
                    , HA.style "color" "red"
                    ]
                    [ Html.text "Remove" ]
                ]
        )
        (Dict.get id model.vectors)
        (Dict.get id model.vectorControllers)
        |> Maybe.withDefault (Html.text "")


operationController : VectorId -> Operation -> ( VectorId, VectorId ) -> Html Msg
operationController id op ( refId1, refId2 ) =
    Html.div
        []
        [ Html.text <| "(id:" ++ vectorIdToString id ++ ")"
        , Html.text <| "id1="
        , Html.input
            [ HA.type_ "number"
            , HA.value (vectorIdToString refId1)
            , HE.onInput
                (\str ->
                    case String.toInt str of
                        Nothing ->
                            Discard

                        Just value ->
                            ModifyOperationController id ( value, refId2 )
                )
            ]
            []
        , Html.text <| "id2="
        , Html.input
            [ HA.type_ "number"
            , HA.value (vectorIdToString refId2)
            , HE.onInput
                (\str ->
                    case String.toInt str of
                        Nothing ->
                            Discard

                        Just value ->
                            ModifyOperationController id ( refId1, value )
                )
            ]
            []
        , Html.select
            []
            [ Html.option
                [ HE.onClick (ChangeOperationInController id Add)
                ]
                [ Html.text "Add" ]
            , Html.option
                [ HE.onClick (ChangeOperationInController id Sub)
                ]
                [ Html.text "Sub" ]
            ]
        ]


positionController : VectorId -> Vector2 Float -> Html Msg
positionController id v =
    Html.div
        []
        [ Html.text <| "(id:" ++ vectorIdToString id ++ ")"
        , Html.text <| "x="
        , Html.input
            [ HA.type_ "number"
            , HA.value (String.fromFloat v.x)
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
        , Html.text <| "y="
        , Html.input
            [ HA.type_ "number"
            , HA.value (String.fromFloat v.y)
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
    Decode.map3 Model
        vectorsDecoder
        vectorControllersDecoder
        vectorColorDecoder


vectorColorDecoder : Decoder (Dict VectorId Color)
vectorColorDecoder =
    Decode.field "vectorColors" (Decode.dict colorDecoder)
        |> vectorIdDictDecoder


colorDecoder : Decoder Color
colorDecoder =
    Decode.map4 Color.rgba
        (Decode.field "r" Decode.float)
        (Decode.field "g" Decode.float)
        (Decode.field "b" Decode.float)
        (Decode.field "a" Decode.float)


vectorControllersDecoder : Decoder (Dict VectorId VectorController)
vectorControllersDecoder =
    Decode.field "vectorControllers" (Decode.dict vectorControllerDecoder)
        |> vectorIdDictDecoder


vectorControllerDecoder : Decoder VectorController
vectorControllerDecoder =
    Decode.field "vectorController" <|
        Decode.oneOf
            [ positionControllerDecoder
            , operationControllerDecoder
            ]


operationControllerDecoder : Decoder VectorController
operationControllerDecoder =
    Decode.field "operation" <|
        Decode.map2
            (\( id1, id2 ) op -> OperationController op id1 id2)
            tupleDecoder
            operationDecoder


operationDecoder : Decoder Operation
operationDecoder =
    Decode.field "op" <|
        Decode.oneOf
            [ Decode.field "add" <| Decode.map (\_ -> Add) (Decode.null {})
            , Decode.field "sub" <| Decode.map (\_ -> Sub) (Decode.null {})
            ]


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
        , ( "vectorColors"
          , Encode.dict
                vectorIdToString
                colorToJson
                model.vectorColors
          )
        ]


colorToJson : Color -> Encode.Value
colorToJson color =
    let
        rgba =
            Color.toRgba color
    in
    Encode.object
        [ ( "r", Encode.float rgba.red )
        , ( "g", Encode.float rgba.green )
        , ( "b", Encode.float rgba.blue )
        , ( "a", Encode.float rgba.alpha )
        ]


vectorControllerToJson : VectorController -> Encode.Value
vectorControllerToJson controller =
    Encode.object
        [ ( "vectorController"
          , case controller of
                PositionController ->
                    positionControllerToJson

                OperationController op id1 id2 ->
                    operationControllerToJson op ( id1, id2 )
          )
        ]


positionControllerToJson : Encode.Value
positionControllerToJson =
    Encode.object
        [ ( "position", Encode.null )
        ]


operationControllerToJson : Operation -> ( VectorId, VectorId ) -> Encode.Value
operationControllerToJson op ( id1, id2 ) =
    Encode.object
        [ ( "operation"
          , Encode.object
                [ ( "op", operationEncoder op )
                , ( "id1", Encode.int id1 )
                , ( "id2", Encode.int id2 )
                ]
          )
        ]


operationEncoder : Operation -> Encode.Value
operationEncoder operation =
    case operation of
        Add ->
            Encode.object
                [ ( "add", Encode.null ) ]

        Sub ->
            Encode.object
                [ ( "sub", Encode.null ) ]


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



-- Color helpers


colorFromHex : String -> Maybe Color
colorFromHex hexString =
    case String.toList hexString of
        [ '#', r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ r, g, b ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

        [ '#', r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ r, g, b, a ] ->
            fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

        [ '#', r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ r1, r2, g1, g2, b1, b2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

        [ '#', r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
            fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

        _ ->
            Nothing


fromHex8 : ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Maybe Color
fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    Maybe.map4
        (\r g b a ->
            Color.fromRgba
                { red = toFloat r / 255
                , green = toFloat g / 255
                , blue = toFloat b / 255
                , alpha = toFloat a / 255
                }
        )
        (hex2ToInt r1 r2)
        (hex2ToInt g1 g2)
        (hex2ToInt b1 b2)
        (hex2ToInt a1 a2)


hex2ToInt : Char -> Char -> Maybe Int
hex2ToInt c1 c2 =
    Maybe.map2 (\v1 v2 -> shiftLeftBy 4 v1 + v2) (hexToInt c1) (hexToInt c2)


hexToInt : Char -> Maybe Int
hexToInt char =
    case Char.toLower char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


toHex : Color -> { hex : String, alpha : Float }
toHex c =
    let
        components =
            Color.toRgba c
    in
    { hex =
        [ components.red, components.green, components.blue ]
            |> List.map ((*) 255)
            |> List.map round
            |> List.map int255ToHex
            |> String.concat
            |> (++) "#"
    , alpha = components.alpha
    }


int255ToHex : Int -> String
int255ToHex n =
    if n < 0 then
        "00"

    else if n > 255 then
        "ff"

    else
        unsafeInt255Digits n
            |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
            |> (\( a, b ) -> String.cons a (String.cons b ""))


unsafeInt255Digits : Int -> ( Int, Int )
unsafeInt255Digits n =
    let
        digit1 =
            n // 16

        digit0 =
            if digit1 /= 0 then
                modBy (digit1 * 16) n

            else
                n
    in
    ( digit1, digit0 )


unsafeIntToChar : Int -> Char
unsafeIntToChar i =
    if i < 10 then
        String.fromInt i
            |> String.uncons
            |> Maybe.map Tuple.first
            |> Maybe.withDefault '0'

    else
        case i of
            10 ->
                'a'

            11 ->
                'b'

            12 ->
                'c'

            13 ->
                'd'

            14 ->
                'e'

            15 ->
                'f'

            _ ->
                '0'
