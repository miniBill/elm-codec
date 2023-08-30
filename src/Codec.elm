module Codec exposing
    ( Codec, Value, Error
    , Decoder, decoder, decodeString, decodeValue
    , encoder, encodeToString, encodeToValue
    , string, bool, int, float, char, enum
    , maybe, nullable, list, array, dict, set, tuple, triple, result
    , ObjectCodec, object, field, optionalField, optionalNullableField, buildObject
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , oneOf
    , map
    , succeed, recursive, fail, andThen, lazy, value, build, constant
    , nullableField, maybeField
    )

{-| A `Codec a` contain a JSON `Decoder a` and the corresponding `a -> Value` encoder.


# Definition

@docs Codec, Value, Error


# Decode

@docs Decoder, decoder, decodeString, decodeValue


# Encode

@docs encoder, encodeToString, encodeToValue


# Primitives

@docs string, bool, int, float, char, enum


# Data Structures

@docs maybe, nullable, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, optionalField, optionalNullableField, buildObject


# Custom Types

@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Inconsistent structure

@docs oneOf


# Mapping

@docs map


# Fancy Codecs

@docs succeed, recursive, fail, andThen, lazy, value, build, constant


# Deprecated

@docs nullableField, maybeField

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Set exposing (Set)



-- DEFINITION


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a
    = Codec
        { encoder : a -> Value
        , decoder : Decoder a
        }


{-| Represents a JavaScript value.
-}
type alias Value =
    JE.Value


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire JSON object and show the part causing the failure in
red.
-}
type alias Error =
    JD.Error



-- DECODE


{-| A value that knows how to decode JSON values.
-}
type alias Decoder a =
    JD.Decoder a


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> Decoder a
decoder (Codec m) =
    m.decoder


{-| Parse the given string into a JSON value and then run the `Codec` on it.
This will fail if the string is not well-formed JSON or if the `Codec`
fails for some reason.
-}
decodeString : Codec a -> String -> Result Error a
decodeString codec =
    JD.decodeString (decoder codec)


{-| Run a `Codec` to decode some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
-}
decodeValue : Codec a -> Value -> Result Error a
decodeValue codec =
    JD.decodeValue (decoder codec)



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> a -> Value
encoder (Codec m) =
    m.encoder


{-| Convert a value into a prettified JSON string. The first argument specifies
the amount of indentation in the result string.
-}
encodeToString : Int -> Codec a -> a -> String
encodeToString indentation codec =
    encoder codec >> JE.encode indentation


{-| Convert a value into a Javascript `Value`.
-}
encodeToValue : Codec a -> a -> Value
encodeToValue codec =
    encoder codec



-- BASE


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : (a -> Value) -> Decoder a -> Codec a
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        }


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    build JE.string JD.string


{-| `Codec` between a JSON boolean and an Elm `Bool`
-}
bool : Codec Bool
bool =
    build JE.bool JD.bool


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    build JE.int JD.int


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    build JE.float JD.float


{-| `Codec` between a JSON string of length 1 and an Elm `Char`
-}
char : Codec Char
char =
    build
        (String.fromChar >> JE.string)
        (JD.string
            |> JD.andThen
                (\s ->
                    case String.uncons s of
                        Just ( h, "" ) ->
                            JD.succeed h

                        _ ->
                            JD.fail "Expected a single char"
                )
        )


{-| `Codec` for a fixed list of Elm values.

This can be used for custom types, if they are really simple:

    type Semaphore = Red | Yellow | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        enum Codec.string
            [ ("Red", Red)
            , ("Yellow", Yellow)
            , ("Green", Green)
            ]

    encodeToString 0 semaphoreCodec Red
    --> "\"Red\""
    decodeString semaphoreCodec "\"Red\""
    --> Ok Red

    type Count = One | Two | Three

    countCodec : Codec Count
    countCodec =
        enum Codec.int
            [ (1, One)
            , (2, Two)
            , (3, Three)
            ]

    encodeToString 0 countCodec Two
    --> "2"
    decodeString countCodec "2"
    --> Ok Two

    incompleteCodec : Codec Count
    incompleteCodec =
        enum Codec.int
            [ (1, One)
            , (2, Two)
            ]

    encodeToString 0 incompleteCodec Three
    --> "null"

    decodeString incompleteCodec "3" |> Result.mapError (\_ -> "...")
    --> Err "..."

-}
enum : Codec a -> List ( a, b ) -> Codec b
enum (Codec codec) options =
    let
        enc : List ( a, b ) -> b -> JE.Value
        enc queue val =
            case queue of
                [] ->
                    JE.null

                ( k, v ) :: tail ->
                    if v == val then
                        codec.encoder k

                    else
                        enc tail val

        dec : List ( a, b ) -> a -> JD.Decoder b
        dec queue key =
            case queue of
                [] ->
                    JD.fail "Key not found"

                ( k, v ) :: tail ->
                    if k == key then
                        JD.succeed v

                    else
                        dec tail key
    in
    Codec
        { encoder = enc options
        , decoder =
            codec.decoder
                |> JD.andThen
                    (\k -> dec options k)
        }



-- DATA STRUCTURES


composite : ((b -> Value) -> (a -> Value)) -> (Decoder b -> Decoder a) -> Codec b -> Codec a
composite enc dec (Codec codec) =
    Codec
        { encoder = enc codec.encoder
        , decoder = dec codec.decoder
        }


{-| Represents an optional value.

This is encoded as `null` when the input is `Nothing`, and the same as `x` when `Just x`.

If the decoding using the inner `Codec` fails, it will _succeed_ with `Nothing`. If you want it to fail use `nullable` instead.

    encodeToString 0 (maybe int) (Just 3)
    --> "3"
    encodeToString 0 (maybe int) Nothing
    --> "null"

    decodeString (maybe int) "3"
    --> Ok (Just 3)
    decodeString (maybe int) "null"
    --> Ok Nothing
    decodeString (maybe int) "\"hello\""
    --> Ok Nothing

-}
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    Codec
        { decoder = JD.maybe <| decoder codec
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        JE.null

                    Just x ->
                        encoder codec x
        }


{-| Represents an optional value.

This is encoded as `null` when the input is `Nothing`, and the same as `x` when `Just x`.

When decoding, it decodes `null` to `Nothing`. Otherwise, if the decoding using the inner `Codec` fails, it will fail. If you want it to succeed with `Nothing` use `maybe` instead.

    encodeToString 0 (nullable int) (Just 3)
    --> "3"
    encodeToString 0 (nullable int) Nothing
    --> "null"

    decodeString (nullable int) "3"
    --> Ok (Just 3)
    decodeString (nullable int) "null"
    --> Ok Nothing
    decodeString (nullable int) "\"hello\"" |> Result.mapError (\_ -> "...")
    --> Err "..."

-}
nullable : Codec a -> Codec (Maybe a)
nullable codec =
    Codec
        { decoder = JD.nullable <| decoder codec
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        JE.null

                    Just x ->
                        encoder codec x
        }


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list =
    composite JE.list JD.list


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array =
    composite JE.array JD.array


{-| `Codec` between a JSON object and an Elm `Dict`.
-}
dict : Codec a -> Codec (Dict String a)
dict =
    composite
        (\e -> JE.object << Dict.toList << Dict.map (\_ -> e))
        JD.dict


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set =
    composite
        (\e -> JE.list e << Set.toList)
        (JD.map Set.fromList << JD.list)


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            \( v1, v2 ) ->
                JE.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    ]
        , decoder =
            JD.map2
                (\a b -> ( a, b ))
                (JD.index 0 <| decoder m1)
                (JD.index 1 <| decoder m2)
        }


{-| `Codec` between a JSON array of length 3 and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            \( v1, v2, v3 ) ->
                JE.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    , encoder m3 v3
                    ]
        , decoder =
            JD.map3
                (\a b c -> ( a, b, c ))
                (JD.index 0 <| decoder m1)
                (JD.index 1 <| decoder m2)
                (JD.index 2 <| decoder m3)
        }


{-| `Codec` for `Result` values.
-}
result : Codec error -> Codec value -> Codec (Result error value)
result errorCodec valueCodec =
    custom
        (\ferr fok v ->
            case v of
                Err err ->
                    ferr err

                Ok ok ->
                    fok ok
        )
        |> variant1 "Err" Err errorCodec
        |> variant1 "Ok" Ok valueCodec
        |> buildCustom



-- OBJECTS


{-| A partially built `Codec` for an object.
-}
type ObjectCodec a b
    = ObjectCodec
        { encoder : a -> List ( String, Value )
        , decoder : Decoder b
        }


{-| Start creating a `Codec` for an object. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an object.

Example with constructor:

    type alias Point =
        { x : Float
        , y : Float
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.object Point
            |> Codec.field "x" .x Codec.float
            |> Codec.field "y" .y Codec.float
            |> Codec.buildObject

Example without constructor:

    pointCodec : Codec { x : Int, y : Bool }
    pointCodec =
        Codec.object (\x y -> { x = x, y = y })
            |> Codec.field "x" .x Codec.int
            |> Codec.field "y" .y Codec.bool
            |> Codec.buildObject

-}
object : b -> ObjectCodec a b
object ctor =
    ObjectCodec
        { encoder = \_ -> []
        , decoder = JD.succeed ctor
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : String -> (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder = \v -> ( name, encoder codec <| getter v ) :: ocodec.encoder v
        , decoder = JD.map2 (\f x -> f x) ocodec.decoder (JD.field name (decoder codec))
        }


{-| Specify the name getter and `Codec` for an optional field.

**Warning! This is a footgun and thus deprecated, you should probably use `optionalField` instead.**

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`. \_If the field value cannot be decoded by the given `Codec` it also gets decoded to `Nothing`. Even worse, if the input is not an object, this `Codec` still succeeds!

When encoding, if the optional field's value is `Nothing` then the resulting object will not contain that field.

-}
maybeField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
maybeField name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder =
            \v ->
                case getter v of
                    Just present ->
                        ( name, encoder codec present ) :: ocodec.encoder v

                    Nothing ->
                        ocodec.encoder v
        , decoder =
            decoder codec
                |> JD.field name
                |> JD.maybe
                |> JD.map2 (\f x -> f x) ocodec.decoder
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`. If the field cannot be decoded this will fail. If the value is `null` then this will fail, use `optionalNullableField` if you want it to succeed instad.
If the optional field's value is `Nothing` then the resulting object will not contain that field.

-}
optionalField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
optionalField name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder =
            \v ->
                case getter v of
                    Just present ->
                        ( name, encoder codec present ) :: ocodec.encoder v

                    Nothing ->
                        ocodec.encoder v
        , decoder =
            -- Decoder inspired by https://github.com/elm-community/json-extra/blob/4.3.0/src/Json/Decode/Extra.elm#L272
            JD.keyValuePairs JD.value
                |> JD.andThen
                    (\json ->
                        if List.any (\( k, _ ) -> k == name) json then
                            --The field exist, actually run the decoder
                            JD.map Just (JD.field name (decoder codec))

                        else
                            -- The field is missing
                            JD.succeed Nothing
                    )
                |> JD.map2 (\f x -> f x) ocodec.decoder
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`. If the field cannot be decoded this will fail. If the value is `null` then this will succeed with `Nothing`, use `optionalField` if you want it to fail instad.
If the optional field's value is `Nothing` then the resulting object will not contain that field.

-}
optionalNullableField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
optionalNullableField name getter codec (ObjectCodec ocodec) =
    ObjectCodec
        { encoder =
            \v ->
                case getter v of
                    Just present ->
                        ( name, encoder codec present ) :: ocodec.encoder v

                    Nothing ->
                        ocodec.encoder v
        , decoder =
            -- Decoder inspired by https://github.com/elm-community/json-extra/blob/4.3.0/src/Json/Decode/Extra.elm#L272
            JD.keyValuePairs JD.value
                |> JD.andThen
                    (\json ->
                        if List.any (\( k, _ ) -> k == name) json then
                            --The field exist, actually run the decoder
                            JD.field name (JD.nullable <| decoder codec)

                        else
                            -- The field is missing
                            JD.succeed Nothing
                    )
                |> JD.map2 (\f x -> f x) ocodec.decoder
        }


{-| Specify the name getter and `Codec` for a required field, whose value can be `null`.

**Warning! This is a footgun and thus deprecated, you should probably use `field` with `nullable` instead.**

If the field is not present in the input then _the decoding fails_. If the field is present but can't be decoded then _the decoding succeeds with `Nothing`_.
If the field's value is `Nothing` then the resulting object will contain the field with a `null` value.

This is a shorthand for a field having a `Codec` built using `maybe`.

-}
nullableField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
nullableField name getter codec ocodec =
    field name getter (maybe codec) ocodec


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
buildObject : ObjectCodec a a -> Codec a
buildObject (ObjectCodec om) =
    Codec
        { encoder = \v -> JE.object <| List.reverse <| om.encoder v
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : match
        , decoder : Dict String (Decoder v)
        }


{-| Starts building a `Codec` for a custom type.

You need to pass a pattern matching function, built like this:

    type Semaphore
        = Red Int String
        | Yellow
        | Green Float

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.custom
            (\red yellow green value ->
                case value of
                    Red i s ->
                        red i s

                    Yellow ->
                        yellow

                    Green f ->
                        green f
            )
            |> Codec.variant2 "Red" Red Codec.int Codec.string
            |> Codec.variant0 "Yellow" Yellow
            |> Codec.variant1 "Green" Green Codec.float
            |> Codec.buildCustom

-}
custom : match -> CustomCodec match value
custom match =
    CustomCodec
        { match = match
        , decoder = Dict.empty
        }


variant :
    String
    -> ((List Value -> Value) -> a)
    -> Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant name matchPiece decoderPiece (CustomCodec am) =
    let
        enc : List JE.Value -> JE.Value
        enc v =
            JE.object
                [ ( "tag", JE.string name )
                , ( "args", JE.list identity v )
                ]
    in
    CustomCodec
        { match = am.match <| matchPiece enc
        , decoder = Dict.insert name decoderPiece am.decoder
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    String
    -> v
    -> CustomCodec (Value -> a) v
    -> CustomCodec a v
variant0 name ctor =
    variant name
        (\c -> c [])
        (JD.succeed ctor)


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    String
    -> (a -> v)
    -> Codec a
    -> CustomCodec ((a -> Value) -> b) v
    -> CustomCodec b v
variant1 name ctor m1 =
    variant name
        (\c v ->
            c
                [ encoder m1 v
                ]
        )
        (JD.map ctor
            (JD.index 0 <| decoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    String
    -> (a -> b -> v)
    -> Codec a
    -> Codec b
    -> CustomCodec ((a -> b -> Value) -> c) v
    -> CustomCodec c v
variant2 name ctor m1 m2 =
    variant name
        (\c v1 v2 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                ]
        )
        (JD.map2 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
        )


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    String
    -> (a -> b -> c -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> CustomCodec ((a -> b -> c -> Value) -> partial) v
    -> CustomCodec partial v
variant3 name ctor m1 m2 m3 =
    variant name
        (\c v1 v2 v3 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                ]
        )
        (JD.map3 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
            (JD.index 2 <| decoder m3)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    String
    -> (a -> b -> c -> d -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> CustomCodec ((a -> b -> c -> d -> Value) -> partial) v
    -> CustomCodec partial v
variant4 name ctor m1 m2 m3 m4 =
    variant name
        (\c v1 v2 v3 v4 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                ]
        )
        (JD.map4 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
            (JD.index 2 <| decoder m3)
            (JD.index 3 <| decoder m4)
        )


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    String
    -> (a -> b -> c -> d -> e -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> CustomCodec ((a -> b -> c -> d -> e -> Value) -> partial) v
    -> CustomCodec partial v
variant5 name ctor m1 m2 m3 m4 m5 =
    variant name
        (\c v1 v2 v3 v4 v5 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                ]
        )
        (JD.map5 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
            (JD.index 2 <| decoder m3)
            (JD.index 3 <| decoder m4)
            (JD.index 4 <| decoder m5)
        )


{-| Define a variant with 6 parameters for a custom type.
-}
variant6 :
    String
    -> (a -> b -> c -> d -> e -> f -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> Value) -> partial) v
    -> CustomCodec partial v
variant6 name ctor m1 m2 m3 m4 m5 m6 =
    variant name
        (\c v1 v2 v3 v4 v5 v6 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                , encoder m6 v6
                ]
        )
        (JD.map6 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
            (JD.index 2 <| decoder m3)
            (JD.index 3 <| decoder m4)
            (JD.index 4 <| decoder m5)
            (JD.index 5 <| decoder m6)
        )


{-| Define a variant with 7 parameters for a custom type.
-}
variant7 :
    String
    -> (a -> b -> c -> d -> e -> f -> g -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> Value) -> partial) v
    -> CustomCodec partial v
variant7 name ctor m1 m2 m3 m4 m5 m6 m7 =
    variant name
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                , encoder m6 v6
                , encoder m7 v7
                ]
        )
        (JD.map7 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
            (JD.index 2 <| decoder m3)
            (JD.index 3 <| decoder m4)
            (JD.index 4 <| decoder m5)
            (JD.index 5 <| decoder m6)
            (JD.index 6 <| decoder m7)
        )


{-| Define a variant with 8 parameters for a custom type.
-}
variant8 :
    String
    -> (a -> b -> c -> d -> e -> f -> g -> h -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Codec h
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Value) -> partial) v
    -> CustomCodec partial v
variant8 name ctor m1 m2 m3 m4 m5 m6 m7 m8 =
    variant name
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                , encoder m3 v3
                , encoder m4 v4
                , encoder m5 v5
                , encoder m6 v6
                , encoder m7 v7
                , encoder m8 v8
                ]
        )
        (JD.map8 ctor
            (JD.index 0 <| decoder m1)
            (JD.index 1 <| decoder m2)
            (JD.index 2 <| decoder m3)
            (JD.index 3 <| decoder m4)
            (JD.index 4 <| decoder m5)
            (JD.index 5 <| decoder m6)
            (JD.index 6 <| decoder m7)
            (JD.index 7 <| decoder m8)
        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> Value) a -> Codec a
buildCustom (CustomCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            JD.field "tag" JD.string
                |> JD.andThen
                    (\tag ->
                        case Dict.get tag am.decoder of
                            Nothing ->
                                JD.fail <| "tag " ++ tag ++ " did not match"

                            Just dec ->
                                JD.field "args" dec
                    )
        }



-- INCONSISTENT STRUCTURE


{-| Try a set of decoders (in order).
The first argument is used for encoding and decoding, the list of other codecs is used as a fallback while decoding.

This is particularly useful for backwards compatibility. You would pass the current codec as the first argument,
and the old ones (eventually `map`ped) as a fallback list to use while decoding.

-}
oneOf : Codec a -> List (Codec a) -> Codec a
oneOf main alts =
    Codec
        { encoder = encoder main
        , decoder = JD.oneOf <| decoder main :: List.map decoder alts
        }



-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map go back codec =
    Codec
        { decoder = JD.map go <| decoder codec
        , encoder = \v -> back v |> encoder codec
        }



-- FANCY


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case. The encoder will produce `null`.
-}
fail : String -> Codec a
fail msg =
    Codec
        { decoder = JD.fail msg
        , encoder = always JE.null
        }


{-| Create codecs that depend on previous results.
-}
andThen : (a -> Codec b) -> (b -> a) -> Codec a -> Codec b
andThen dec enc c =
    Codec
        { decoder = decoder c |> JD.andThen (dec >> decoder)
        , encoder = encoder c << enc
        }


{-| Create a `Codec` for a recursive data structure.
The argument to the function you need to pass is the fully formed `Codec`.
-}
recursive : (Codec a -> Codec a) -> Codec a
recursive f =
    f <| lazy (\_ -> recursive f)


{-| Create a `Codec` that produces null as JSON and always decodes as the same value.
-}
succeed : a -> Codec a
succeed default_ =
    Codec
        { decoder = JD.succeed default_
        , encoder = \_ -> JE.null
        }


{-| Create a `Codec` that produces null as JSON and always decodes as the same value. Obsolete alias of `succeed`, will be removed in a future version.
-}
constant : a -> Codec a
constant =
    succeed


{-| This is useful for recursive structures that are not easily modeled with `recursive`.
Have a look at the Json.Decode docs for examples.
-}
lazy : (() -> Codec a) -> Codec a
lazy f =
    Codec
        { decoder = JD.lazy (\_ -> decoder <| f ())
        , encoder = \v -> encoder (f ()) v
        }


{-| Create a `Codec` that doesn't transform the JSON value, just brings it to and from Elm as a `Value`.
-}
value : Codec Value
value =
    Codec
        { encoder = identity
        , decoder = JD.value
        }
