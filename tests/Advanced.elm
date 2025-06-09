module Advanced exposing (suite)

import Base
import Codec exposing (Codec)
import Codec.Advanced
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test, test)


suite : List Test
suite =
    [ Base.roundtrips semaphoreFuzzer semaphoreCodec
    , test "It doesn't make sure the variants are separate" <|
        \_ ->
            Green 0
                |> Codec.encodeToValue wrongSemaphoreCodec
                |> Codec.decodeValue wrongSemaphoreCodec
                |> Expect.equal (Ok Yellow)
    ]


type Semaphore
    = Red Int String
    | Yellow
    | Green Float


semaphoreCodec : Codec Semaphore
semaphoreCodec =
    Codec.Advanced.custom
        (\red green yellow value ->
            case value of
                Red i s ->
                    red ( i, s )

                Yellow ->
                    yellow ()

                Green f ->
                    green f
        )
        |> Codec.Advanced.variant
            (\( i, s ) -> Red i s)
            (Codec.tuple Codec.int Codec.string)
        |> Codec.Advanced.variant
            Green
            Codec.float
        |> Codec.Advanced.variant
            (\() -> Yellow)
            (Codec.succeed ())
        |> Codec.Advanced.buildCustom


wrongSemaphoreCodec : Codec Semaphore
wrongSemaphoreCodec =
    Codec.Advanced.custom
        (\red yellow green value ->
            case value of
                Red i s ->
                    red ( i, s )

                Yellow ->
                    yellow ()

                Green f ->
                    green f
        )
        |> Codec.Advanced.variant
            (\( i, s ) -> Red i s)
            (Codec.tuple Codec.int Codec.string)
        |> Codec.Advanced.variant
            (\() -> Yellow)
            (Codec.succeed ())
        |> Codec.Advanced.variant
            Green
            Codec.float
        |> Codec.Advanced.buildCustom


semaphoreFuzzer : Fuzzer Semaphore
semaphoreFuzzer =
    Fuzz.oneOf
        [ Fuzz.map2 Red Fuzz.int Fuzz.string
        , Fuzz.constant Yellow
        , Fuzz.map Green Fuzz.float
        ]
