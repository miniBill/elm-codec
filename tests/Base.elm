module Base exposing (roundtrips, suite)

import Codec exposing (Codec)
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
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
        , describe "constant"
            [ test "roundtrips"
                (\_ ->
                    Codec.constant 632
                        |> (\d -> Codec.decodeString d "{}")
                        |> Expect.equal (Ok 632)
                )
            ]
        , describe "recursive" recursiveTests
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


type RecA
    = RecA (Maybe RecB)


type RecB
    = RecB (Result () RecA)


intToRec : Int -> RecA
intToRec =
    let
        a n =
            RecA <|
                if n > 0 then
                    Just (b <| n - 1)

                else
                    Nothing

        b n =
            RecB <|
                if n > 0 then
                    Ok (a <| n - 1)

                else
                    Err ()
    in
    a


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
    , describe "coRecursion"
        [ roundtrips (Fuzz.intRange 0 200 |> Fuzz.map intToRec) <|
            Tuple.first <|
                Codec.recursive2
                    (\( a, b ) ->
                        ( Codec.map
                            RecA
                            (\(RecA v) -> v)
                          <|
                            Codec.maybe b
                        , Codec.map
                            RecB
                            (\(RecB v) -> v)
                          <|
                            Codec.result (Codec.constant ()) a
                        )
                    )
        ]
    ]
