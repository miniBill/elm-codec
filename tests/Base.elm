module Base exposing (roundtrips, suite)

import Codec exposing (Codec)
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as JD
import Set
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "Testing roundtrips"
        [ describe "Basic" basicTests
        , describe "Containers" containersTests
        , describe "Object" objectTests
        , describe "Custom" customTests
        , describe "bimap" bimapTests
        , describe "maybe" maybeTests
        , describe "optional" optionalTests
        , describe "succeed"
            [ test "roundtrips"
                (\_ ->
                    Codec.succeed 632
                        |> (\d -> Codec.decodeString d "{}")
                        |> Expect.equal (Ok 632)
                )
            ]
        , describe "recursive" recursiveTests
        , describe "map,andThen" mapAndThenTests
        ]


roundtrips : Fuzzer a -> Codec a -> Test
roundtrips fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            value
                |> Codec.encoder codec
                |> Codec.decodeValue codec
                |> Expect.equal (Ok value)


roundtripsWithin : Fuzzer Float -> Codec Float -> Test
roundtripsWithin fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            value
                |> Codec.encoder codec
                |> Codec.decodeValue codec
                |> Result.withDefault -999.1234567
                |> Expect.within (Expect.Relative 0.000001) value


basicTests : List Test
basicTests =
    [ describe "Codec.string"
        [ roundtrips Fuzz.string Codec.string
        ]
    , describe "Codec.int"
        [ roundtrips Fuzz.int Codec.int
        ]
    , describe "Codec.float"
        [ roundtrips Fuzz.float Codec.float
        ]
    , describe "Codec.bool"
        [ roundtrips Fuzz.bool Codec.bool
        ]
    ]


containersTests : List Test
containersTests =
    [ describe "Codec.array"
        [ roundtrips (Fuzz.array Fuzz.int) (Codec.array Codec.int)
        ]
    , describe "Codec.list"
        [ roundtrips (Fuzz.list Fuzz.int) (Codec.list Codec.int)
        ]
    , describe "Codec.dict"
        [ roundtrips
            (Fuzz.map2 Tuple.pair Fuzz.string Fuzz.int
                |> Fuzz.list
                |> Fuzz.map Dict.fromList
            )
            (Codec.dict Codec.int)
        ]
    , describe "Codec.set"
        [ roundtrips
            (Fuzz.list Fuzz.int |> Fuzz.map Set.fromList)
            (Codec.set Codec.int)
        ]
    , describe "Codec.tuple"
        [ roundtrips
            (Fuzz.tuple ( Fuzz.int, Fuzz.int ))
            (Codec.tuple Codec.int Codec.int)
        ]
    ]


objectTests : List Test
objectTests =
    [ describe "with 0 fields"
        [ roundtrips (Fuzz.constant {})
            (Codec.object {}
                |> Codec.buildObject
            )
        ]
    , describe "with 1 field"
        [ roundtrips (Fuzz.map (\i -> { fname = i }) Fuzz.int)
            (Codec.object (\i -> { fname = i })
                |> Codec.field "fname" .fname Codec.int
                |> Codec.buildObject
            )
        ]
    , describe "with 2 fields"
        [ roundtrips
            (Fuzz.map2
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                Fuzz.int
                Fuzz.int
            )
            (Codec.object
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                |> Codec.field "a" .a Codec.int
                |> Codec.field "b" .b Codec.int
                |> Codec.buildObject
            )
        ]
    , describe "nullableField vs maybeField" <|
        let
            nullableCodec =
                Codec.object
                    (\f -> { f = f })
                    |> Codec.nullableField "f" .f Codec.int
                    |> Codec.buildObject

            maybeCodec =
                Codec.object
                    (\f -> { f = f })
                    |> Codec.maybeField "f" .f Codec.int
                    |> Codec.buildObject
        in
        [ test "a nullableField is required" <|
            \_ ->
                "{}"
                    |> Codec.decodeString nullableCodec
                    |> (\r ->
                            case r of
                                Ok _ ->
                                    Expect.fail "Should have failed"

                                Err _ ->
                                    Expect.pass
                       )
        , test "a nullableField produces a field with a null value on encoding Nothing" <|
            \_ ->
                { f = Nothing }
                    |> Codec.encodeToString 0 nullableCodec
                    |> Expect.equal "{\"f\":null}"
        , test "a maybeField is optional" <|
            \_ ->
                "{}"
                    |> Codec.decodeString maybeCodec
                    |> Expect.equal (Ok { f = Nothing })
        , test "a maybeField doesn't produce a field on encoding Nothing" <|
            \_ ->
                { f = Nothing }
                    |> Codec.encodeToString 0 maybeCodec
                    |> Expect.equal "{}"
        ]
    , describe "optionalField" <|
        let
            optionalCodec =
                Codec.object
                    (\f -> { f = f })
                    |> Codec.optionalField "f" .f Codec.int
                    |> Codec.buildObject
        in
        [ optionalCodec
            |> roundtrips
                (Fuzz.map
                    (\maybeInt -> { f = maybeInt })
                    (Fuzz.maybe Fuzz.int)
                )
        , test "an optionalField fallbacks to Nothing when field is missing" <|
            \_ ->
                "{}"
                    |> Codec.decodeString optionalCodec
                    |> Expect.equal (Ok { f = Nothing })
        , test "an optionalField decodes to Nothing when JSON value is null" <|
            \_ ->
                """{ "f": null }"""
                    |> Codec.decodeString optionalCodec
                    |> Expect.equal (Ok { f = Nothing })
        , test "an optionalField validates provided data" <|
            \_ ->
                """{ "f": "invalid" }"""
                    |> Codec.decodeString optionalCodec
                    |> Result.mapError JD.errorToString
                    |> (\res ->
                            case res of
                                Ok _ ->
                                    Expect.fail "Shouldn't have succeeded"

                                Err msg ->
                                    Expect.true "Expected error message found" (String.contains "Expecting an INT" msg)
                       )
        ]
    , describe "optionalFieldWithDefault" <|
        let
            optionalWithDefaultCodec default =
                Codec.object
                    (\f -> { f = f })
                    |> Codec.optionalFieldWithDefault "f" .f Codec.int default
                    |> Codec.buildObject
        in
        [ optionalWithDefaultCodec 0
            |> roundtrips (Fuzz.map (\int -> { f = int }) Fuzz.int)
        , test "an optionalFieldWithDefault applies a default value when field is missing" <|
            \_ ->
                "{}"
                    |> Codec.decodeString (optionalWithDefaultCodec 42)
                    |> Expect.equal (Ok { f = 42 })
        , test "an optionalFieldWithDefault applies a default value when field value is null" <|
            \_ ->
                """{ "f": null }"""
                    |> Codec.decodeString (optionalWithDefaultCodec 42)
                    |> Expect.equal (Ok { f = 42 })
        , test "an optionalFieldWithDefault validates provided data" <|
            \_ ->
                """{ "f": "invalid" }"""
                    |> Codec.decodeString (optionalWithDefaultCodec 42)
                    |> Result.mapError JD.errorToString
                    |> (\res ->
                            case res of
                                Ok _ ->
                                    Expect.fail "Shouldn't have succeeded"

                                Err msg ->
                                    Expect.true "Expected error message found" (String.contains "Expecting an INT" msg)
                       )
        ]
    ]


type Newtype a
    = Newtype a


customTests : List Test
customTests =
    [ describe "with 1 ctor, 0 args"
        [ roundtrips (Fuzz.constant ())
            (Codec.custom
                (\f v ->
                    case v of
                        () ->
                            f
                )
                |> Codec.variant0 "()" ()
                |> Codec.buildCustom
            )
        ]
    , describe "with 1 ctor, 1 arg"
        [ roundtrips (Fuzz.map Newtype Fuzz.int)
            (Codec.custom
                (\f v ->
                    case v of
                        Newtype a ->
                            f a
                )
                |> Codec.variant1 "Newtype" Newtype Codec.int
                |> Codec.buildCustom
            )
        ]
    , describe "with 2 ctors, 0,1 args" <|
        let
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just v ->
                        fjust v

            codec =
                Codec.custom match
                    |> Codec.variant0 "Nothing" Nothing
                    |> Codec.variant1 "Just" Just Codec.int
                    |> Codec.buildCustom

            fuzzers =
                [ ( "1st ctor", Fuzz.constant Nothing )
                , ( "2nd ctor", Fuzz.map Just Fuzz.int )
                ]
        in
        fuzzers
            |> List.map
                (\( name, fuzz ) ->
                    describe name
                        [ roundtrips fuzz codec ]
                )
    ]


bimapTests : List Test
bimapTests =
    [ roundtripsWithin Fuzz.float <|
        Codec.map
            (\x -> x * 2)
            (\x -> x / 2)
            Codec.float
    ]


maybeTests : List Test
maybeTests =
    [ describe "single"
        [ roundtrips
            (Fuzz.oneOf
                [ Fuzz.constant Nothing
                , Fuzz.map Just Fuzz.int
                ]
            )
          <|
            Codec.maybe Codec.int
        ]

    {-
       This is a known limitation: using null as Nothing and identity as Just means that nesting two maybes squashes Just Nothing with Nothing
       , describe "double"
          [ roundtrips
              (Fuzz.oneOf
                  [ Fuzz.constant Nothing
                  , Fuzz.constant <| Just Nothing
                  , Fuzz.map (Just << Just) Fuzz.int
                  ]
              )
            <|
              Codec.maybe <|
                  Codec.maybe Codec.int
          ]
    -}
    ]


optionalTests : List Test
optionalTests =
    let
        testCodec =
            Codec.optional Codec.int
    in
    [ testCodec
        |> roundtrips (Fuzz.maybe Fuzz.int)
    , test "should accept a provided value" <|
        \_ ->
            "42"
                |> Codec.decodeString testCodec
                |> Expect.equal (Ok (Just 42))
    , test "should resolve to Nothing when null is provided" <|
        \_ ->
            "null"
                |> Codec.decodeString testCodec
                |> Expect.equal (Ok Nothing)
    , test "should fail when provided value is invalid" <|
        \_ ->
            "[]"
                |> Codec.decodeString testCodec
                |> Result.mapError JD.errorToString
                |> (\res ->
                        case res of
                            Ok _ ->
                                Expect.fail "Shouldn't have succeeded"

                            Err msg ->
                                Expect.true "Expected error message found" (String.contains "Expecting an INT" msg)
                   )
    , test "should encode available data" <|
        \_ ->
            Just 1
                |> Codec.encodeToString 0 testCodec
                |> Expect.equal "1"
    , test "should encode missing data" <|
        \_ ->
            Nothing
                |> Codec.encodeToString 0 testCodec
                |> Expect.equal "null"
    ]


recursiveTests : List Test
recursiveTests =
    [ describe "list"
        [ roundtrips (Fuzz.list Fuzz.int) <|
            Codec.recursive
                (\c ->
                    Codec.custom
                        (\fempty fcons value ->
                            case value of
                                [] ->
                                    fempty

                                x :: xs ->
                                    fcons x xs
                        )
                        |> Codec.variant0 "[]" []
                        |> Codec.variant2 "(::)" (::) Codec.int c
                        |> Codec.buildCustom
                )
        ]
    ]


mapAndThenTests : List Test
mapAndThenTests =
    [ describe "Codec.map"
        [ roundtrips (Fuzz.intRange -10000 10000) <|
            Codec.map (\x -> x - 1) (\x -> x + 1) Codec.int
        ]
    ]
