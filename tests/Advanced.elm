module Advanced exposing (..)

import Codec exposing (Codec)
import Codec.Advanced as Codec
import Expect
import Fuzz exposing (Fuzzer)
import Json.Encode as JE
import Test exposing (Test, describe, fuzz)


type Semaphore
    = Red Int String
    | Yellow
    | Green Float


semaphoreCodec : Codec Semaphore
semaphoreCodec =
    Codec.customObject "color"
        (\red yellow green value ->
            case value of
                Red i s ->
                    red i s

                Yellow ->
                    yellow

                Green f ->
                    green f
        )
        |> Codec.objectVariant2 "red" Red ( "first", Codec.int ) ( "second", Codec.string )
        |> Codec.objectVariant0 "yellow" Yellow
        |> Codec.objectVariant1 "green" Green ( "value", Codec.float )
        |> Codec.buildCustomObject


suite : Test
suite =
    describe "Testing customObjectCodec"
        [ describe "Roundtrip" [ roundtripTest ]
        , describe "Correct shapes" shapesTests
        ]


roundtripTest : Test
roundtripTest =
    fuzz semaphoreFuzzer "is a roundtrip" <| \value ->
    value
        |> Codec.encoder semaphoreCodec
        |> Codec.decodeValue semaphoreCodec
        |> Expect.equal (Ok value)


semaphoreFuzzer : Fuzzer Semaphore
semaphoreFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Red Fuzz.int Fuzz.string
        , Fuzz.constant Yellow
        , Fuzz.map Green Fuzz.float
        ]


shapesTests : List Test
shapesTests =
    [ ( "Red decode"
      , [ ( "color", JE.string "red" )
        , ( "first", JE.int 42 )
        , ( "second", JE.string "413" )
        ]
      , Ok <| Red 42 "413"
      )
    , ( "Yellow decode"
      , [ ( "color", JE.string "yellow" )
        , ( "extra", JE.null )
        ]
      , Ok Yellow
      )
    , ( "Green decode"
      , [ ( "color", JE.string "green" )
        , ( "value", JE.float -42 )
        ]
      , Ok <| Green -42
      )
    , ( "Missing fields fail"
      , [ ( "color", JE.string "green" ) ]
      , Codec.decodeString (Codec.fail "Expecting an OBJECT with a field named `value`") """{"color": "green"}"""
      )
    , ( "Wrong tag fail"
      , [ ( "color", JE.string "gray" ) ]
      , Codec.decodeString (Codec.fail "color \"gray\" did not match") """{"color": "gray"}"""
      )
    ]
        |> List.map
            (\( name, fields, expected ) ->
                Test.test name <| \_ ->
                fields
                    |> JE.object
                    |> Codec.decodeValue semaphoreCodec
                    |> Expect.equal expected
            )
