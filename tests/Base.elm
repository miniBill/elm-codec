module Base exposing (..)

import Codec
import Dict
import Expect
import Fuzz
import Set
import Test


tuplesTest : Test.Test
tuplesTest =
    Test.describe "Tuples."
        [ Test.describe "Codec.tuple"
            [ roundTripTest
                (Fuzz.tuple ( Fuzz.int, Fuzz.int ))
                (Codec.tuple Codec.int Codec.int)
            ]
        , Test.describe "Codec.tuple3"
            [ roundTripTest
                (Fuzz.tuple3 ( Fuzz.int, Fuzz.int, Fuzz.int ))
                (Codec.tuple3 Codec.int Codec.int Codec.int)
            ]
        ]


recordsTest : Test.Test
recordsTest =
    Test.describe "Records."
        [ Test.describe "with 0 fields"
            [ roundTripTest
                (Fuzz.constant {})
                (Codec.record {}
                    |> Codec.buildRecord
                )
            ]
        , Test.describe "with 1 field"
            [ roundTripTest
                (Fuzz.map
                    (\a -> { a = a })
                    Fuzz.int
                )
                (Codec.record
                    (\a -> { a = a })
                    |> Codec.field "a" .a Codec.int
                    |> Codec.buildRecord
                )
            ]
        , Test.describe "with 2 fields"
            [ roundTripTest
                (Fuzz.map2
                    (\a b -> { a = a, b = b })
                    Fuzz.int
                    Fuzz.int
                )
                (Codec.record
                    (\a b -> { a = a, b = b })
                    |> Codec.field "a" .a Codec.int
                    |> Codec.field "b" .b Codec.int
                    |> Codec.buildRecord
                )
            ]
        , Test.describe "with maybeField"
            (let
                recordCodec : Codec.Codec { a : Maybe Int }
                recordCodec =
                    Codec.record
                        (\a -> { a = a })
                        |> Codec.maybeField "a" .a Codec.int
                        |> Codec.buildRecord
             in
             [ roundTripTest
                (Fuzz.map
                    (\a -> { a = a })
                    (Fuzz.maybe Fuzz.int)
                )
                recordCodec
             , Test.test "maybeField is optional"
                (\_ ->
                    Expect.equal
                        (Ok { a = Nothing })
                        (Codec.decodeString recordCodec "{}")
                )
             , Test.test "maybeField doesn't produce a field on encoding Nothing"
                (\_ ->
                    Expect.equal
                        "{}"
                        (Codec.encodeToString 0 recordCodec { a = Nothing })
                )
             ]
            )
        ]


customTypesTest : Test.Test
customTypesTest =
    Test.describe "Custom types."
        [ Test.describe "with 1 variant, 0 args"
            [ roundTripTest
                (Fuzz.constant ())
                (Codec.custom
                    (\fn x ->
                        case x of
                            () ->
                                fn
                    )
                    |> Codec.variant0 "()" ()
                    |> Codec.buildCustom
                )
            ]
        , Test.describe "with 1 variant, 1 arg"
            [ roundTripTest
                (Fuzz.map
                    SomeType
                    Fuzz.int
                )
                (Codec.custom
                    (\fn x ->
                        case x of
                            SomeType a ->
                                fn a
                    )
                    |> Codec.variant1 "SomeType" SomeType Codec.int
                    |> Codec.buildCustom
                )
            ]
        ]


opaqueCustomTypesTest : Test.Test
opaqueCustomTypesTest =
    Test.describe "Opaque custom types."
        [ Test.describe "Codec.int" [ roundTripTest Fuzz.int Codec.int ]
        , Test.describe "Codec.float" [ roundTripTest Fuzz.float Codec.float ]
        , Test.describe "Codec.char" [ roundTripTest Fuzz.char Codec.char ]
        , Test.describe "Codec.string" [ roundTripTest Fuzz.string Codec.string ]
        , Test.describe "Codec.list" [ roundTripTest (Fuzz.list Fuzz.int) (Codec.list Codec.int) ]
        , Test.describe "Codec.array" [ roundTripTest (Fuzz.array Fuzz.int) (Codec.array Codec.int) ]
        , Test.describe "Codec.dict" [ roundTripTest (dictFuzzer Fuzz.int Fuzz.string) (Codec.dict Codec.int Codec.string) ]
        , Test.describe "Codec.set" [ roundTripTest (setFuzzer Fuzz.int) (Codec.set Codec.int) ]
        ]


commonCustomTypesTest : Test.Test
commonCustomTypesTest =
    Test.describe "Common custom types."
        [ Test.describe "Bool."
            [ roundTripTest
                Fuzz.bool
                Codec.bool
            ]
        , Test.describe "Maybe."
            [ Test.describe "single"
                [ roundTripTest
                    (Fuzz.maybe Fuzz.int)
                    (Codec.maybe Codec.int)
                ]
            , Test.describe "double"
                [ roundTripTest
                    (Fuzz.maybe (Fuzz.maybe Fuzz.int))
                    (Codec.maybe (Codec.maybe Codec.int))
                ]
            ]
        , Test.describe "Result."
            [ roundTripTest
                (Fuzz.result Fuzz.string Fuzz.int)
                (Codec.result Codec.string Codec.int)
            ]
        ]


helperTest : Test.Test
helperTest =
    Test.describe "Helper functions."
        [ Test.describe "Codec.map"
            [ roundTripTest
                (Fuzz.intRange -10000 10000)
                (Codec.map (\x -> x - 1) (\x -> x + 1) Codec.int)
            ]
        , Test.test "Codec.succeed"
            (\_ ->
                Expect.equal
                    (Ok 632)
                    (Codec.decodeString (Codec.succeed 632) "{}")
            )
        ]



--


type SomeType a
    = SomeType a



--


roundTripTest : Fuzz.Fuzzer a -> Codec.Codec a -> Test.Test
roundTripTest fuzzer a =
    Test.fuzz
        fuzzer
        "round trip"
        (\x ->
            Expect.equal (Ok x) (x |> Codec.encodeToValue a |> Codec.decodeValue a)
        )



--


dictFuzzer : Fuzz.Fuzzer comparable -> Fuzz.Fuzzer v -> Fuzz.Fuzzer (Dict.Dict comparable v)
dictFuzzer k v =
    Fuzz.map2 Tuple.pair k v |> Fuzz.list |> Fuzz.map Dict.fromList


setFuzzer : Fuzz.Fuzzer comparable -> Fuzz.Fuzzer (Set.Set comparable)
setFuzzer a =
    Fuzz.list a |> Fuzz.map Set.fromList
