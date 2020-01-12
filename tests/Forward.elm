module Forward exposing (suite)

import Base
import Codec exposing (Codec)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as JD
import Test exposing (Test, describe, fuzz)


suite : Test
suite =
    describe "Testing forward and backward compat"
        [ -- describe "Adding a variant" addVariant
          --, describe "Remove parameters" removeParameters
          describe "Any to succeed" anyToSucceed
        , describe "Add optional field" addMaybeField
        ]


compatible : Fuzzer a -> (a -> b) -> Codec a -> Codec b -> Test
compatible fuzzer map oldCodec newCodec =
    fuzz fuzzer "compatible" <|
        \value ->
            value
                |> Codec.encoder oldCodec
                |> JD.decodeValue (Codec.decoder newCodec)
                |> Expect.equal (Ok <| map value)


forward : Fuzzer old -> (old -> new) -> Codec old -> Codec new -> Test
forward fuzzer map oldCodec newCodec =
    describe "forward"
        [ describe "old"
            [ Base.roundtrips fuzzer oldCodec
            ]
        , describe "new"
            [ Base.roundtrips (Fuzz.map map fuzzer) newCodec
            ]
        , describe "old value with new codec"
            [ compatible fuzzer map oldCodec newCodec
            ]
        ]


both :
    Fuzzer old
    -> (old -> new)
    -> Codec old
    -> Fuzzer new
    -> (new -> old)
    -> Codec new
    -> List Test
both oldFuzzer oldToNew oldCodec newFuzzer newToOld newCodec =
    [ describe "old"
        [ Base.roundtrips oldFuzzer oldCodec
        ]
    , describe "new"
        [ Base.roundtrips newFuzzer newCodec
        ]
    , describe "old value with new codec"
        [ compatible oldFuzzer oldToNew oldCodec newCodec
        ]
    , describe "new value with old codec"
        [ compatible newFuzzer newToOld newCodec oldCodec
        ]
    ]


anyToSucceed : List Test
anyToSucceed =
    [ forward Fuzz.string (always 3) Codec.string (Codec.succeed 3)
    ]


type alias Point2 =
    { x : Int
    , y : Int
    }


point2Fuzzer : Fuzzer Point2
point2Fuzzer =
    Fuzz.map2 Point2 Fuzz.int Fuzz.int


point2Codec : Codec Point2
point2Codec =
    Codec.object Point2
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.buildObject


type alias Point2_5 =
    { x : Int
    , y : Int
    , z : Maybe Int
    }


point2_5Fuzzer : Fuzzer Point2_5
point2_5Fuzzer =
    Fuzz.map3 Point2_5 Fuzz.int Fuzz.int (Fuzz.maybe Fuzz.int)


point2_5Codec : Codec Point2_5
point2_5Codec =
    Codec.object Point2_5
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.maybeField "z" .z Codec.int
        |> Codec.buildObject


addMaybeField : List Test
addMaybeField =
    both
        point2Fuzzer
        (\{ x, y } -> { x = x, y = y, z = Nothing })
        point2Codec
        point2_5Fuzzer
        (\{ x, y } -> { x = x, y = y })
        point2_5Codec
