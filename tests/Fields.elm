module Fields exposing (suite)

import Codec
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    [ testField
        "field"
        Codec.field
        { good = Expect.equal (Ok 3)
        , missing = Expect.err
        , null = Expect.err
        , bad = Expect.err
        , notObject = Expect.err
        }
    , testField
        "maybeField"
        Codec.maybeField
        { good = Expect.equal (Ok (Just 3))
        , missing = Expect.equal (Ok Nothing)
        , null = Expect.equal (Ok Nothing)
        , bad = Expect.equal (Ok Nothing)
        , notObject = Expect.equal (Ok Nothing)
        }
    , testField
        "nullableField"
        Codec.nullableField
        { good = Expect.equal (Ok (Just 3))
        , missing = Expect.err
        , null = Expect.equal (Ok Nothing)
        , bad = Expect.equal (Ok Nothing)
        , notObject = Expect.err
        }
    , testField
        "optionalField"
        Codec.optionalField
        { good = Expect.equal (Ok (Just 3))
        , missing = Expect.equal (Ok Nothing)
        , null = Expect.err
        , bad = Expect.err
        , notObject = Expect.err
        }
    , testField
        "optionalNullableField"
        Codec.optionalNullableField
        { good = Expect.equal (Ok (Just 3))
        , missing = Expect.equal (Ok Nothing)
        , null = Expect.equal (Ok Nothing)
        , bad = Expect.err
        , notObject = Expect.err
        }
    ]
        |> describe "Testing the various field variants"


testField :
    String
    -> (String -> (t -> t) -> Codec.Codec Int -> Codec.ObjectCodec b (c -> c) -> Codec.ObjectCodec t t)
    ->
        { good : Result Codec.Error t -> Expect.Expectation
        , missing : Result Codec.Error t -> Expect.Expectation
        , null : Result Codec.Error t -> Expect.Expectation
        , bad : Result Codec.Error t -> Expect.Expectation
        , notObject : Result Codec.Error t -> Expect.Expectation
        }
    -> Test
testField label field expects =
    let
        inner : String -> String -> (Result Codec.Error t -> Expect.Expectation) -> Test
        inner testLabel input expect =
            test (testLabel ++ ": " ++ input) <|
                \_ ->
                    input
                        |> Codec.decodeString
                            (Codec.object identity
                                |> field "a" identity Codec.int
                                |> Codec.buildObject
                            )
                        |> expect
    in
    [ inner "Good field" "{ \"a\": 3 }" expects.good
    , inner "Missing field" "{}" expects.missing
    , inner "Null field" "{ \"a\": null }" expects.null
    , inner "Bad field" "{ \"a\": \"b\" }" expects.bad
    , inner "Not object" "3" expects.notObject
    ]
        |> describe label
