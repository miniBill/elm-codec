module VariantObject exposing (suite)

import Codec exposing (Codec)
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe, fuzz, test)


type Shape
    = Rectangle RectangleData
    | Circle CircleData


type alias RectangleData =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type alias CircleData =
    { r : Int
    , cx : Int
    , cy : Int
    }


shapeCodec : Codec Shape
shapeCodec =
    Codec.customWithTag "kind"
        (\fr fc v ->
            case v of
                Rectangle d ->
                    fr d

                Circle d ->
                    fc d
        )
        |> Codec.variantObject "rect"
            Rectangle
            (Codec.object RectangleData
                |> Codec.field "x" .x Codec.int
                |> Codec.field "y" .y Codec.int
                |> Codec.field "w" .w Codec.int
                |> Codec.field "h" .h Codec.int
            )
        |> Codec.variantObject "circle"
            Circle
            (Codec.object CircleData
                |> Codec.field "r" .r Codec.int
                |> Codec.field "cx" .cx Codec.int
                |> Codec.field "cy" .cy Codec.int
            )
        |> Codec.buildCustom


suite : Test
suite =
    describe "Testing  variantObject"
        [ roundtrips shapeFuzzer shapeCodec
        , test "it produces the correct json for a rectangle" <|
            \_ ->
                Rectangle { x = 1, y = 2, w = 3, h = 4 }
                    |> Codec.encodeToString 0 shapeCodec
                    |> Expect.equal """{"kind":"rect","x":1,"y":2,"w":3,"h":4}"""
        , test "it produces the correct json for a circle" <|
            \_ ->
                Circle { r = 1, cy = 2, cx = 3 }
                    |> Codec.encodeToString 0 shapeCodec
                    |> Expect.equal """{"kind":"circle","r":1,"cx":3,"cy":2}"""
        ]


shapeFuzzer : Fuzzer Shape
shapeFuzzer =
    Fuzz.frequency
        [ ( 1, Fuzz.map4 (\x y w h -> Rectangle { x = x, y = y, w = w, h = h }) Fuzz.int Fuzz.int Fuzz.int Fuzz.int )
        , ( 1, Fuzz.map3 (\r cx cy -> Circle { r = r, cx = cx, cy = cy }) Fuzz.int Fuzz.int Fuzz.int )
        ]


roundtrips : Fuzzer a -> Codec a -> Test
roundtrips fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            value
                |> Codec.encoder codec
                |> Codec.decodeValue codec
                |> Expect.equal (Ok value)
