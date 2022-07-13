module Codec exposing
    ( Codec, build
    , encoder, encodeToString, encodeToValue
    , decoder, decodeString, decodeValue
    , bool, int, float, char, string
    , Record, record, field, maybeField, nullableField, buildObject
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , maybe, list, array, dict, set, tuple, triple, result
    , oneOf
    , map
    , succeed, recursive, fail, andThen, lazy, value, constant
    )

{-| A `Codec` contains a JSON encoder and decoder.

@docs Codec, build


# Encode

@docs encoder, encodeToString, encodeToValue


# Decode

@docs decoder, decodeString, decodeValue


# Primitives

@docs bool, int, float, char, string


# Records

@docs Record, record, field, maybeField, nullableField, buildObject


# Custom Types

@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Inconsistent structure

@docs oneOf


# Mapping

@docs map


# Fancy Codecs

@docs succeed, recursive, fail, andThen, lazy, value, constant

-}

import Array
import Dict
import Json.Decode
import Json.Encode
import Set


{-| A value that knows how to encode and decode JSON values.
-}
type Codec a
    = Codec
        { encoder : a -> Json.Decode.Value
        , decoder : Json.Decode.Decoder a
        }


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : (a -> Json.Decode.Value) -> Json.Decode.Decoder a -> Codec a
build a b =
    Codec
        { encoder = a
        , decoder = b
        }



-- ENCODE


{-| Extracts the encoding function contained inside the `Codec`.
-}
encoder : Codec a -> a -> Json.Decode.Value
encoder (Codec a) =
    a.encoder


{-| Convert a value into a prettified JSON string. The first argument specifies
the amount of indentation in the result string.
-}
encodeToString : Int -> Codec a -> a -> String
encodeToString indentation a =
    encoder a >> Json.Encode.encode indentation


{-| Convert a value into a Javascript `Value`.
-}
encodeToValue : Codec a -> a -> Json.Decode.Value
encodeToValue a =
    encoder a



-- DECODE


{-| Extracts the `Decoder` contained inside the `Codec`.
-}
decoder : Codec a -> Json.Decode.Decoder a
decoder (Codec a) =
    a.decoder


{-| Parse the given string into a JSON value and then run the `Codec` on it.
This will fail if the string is not well-formed JSON or if the `Codec`
fails for some reason.
-}
decodeString : Codec a -> String -> Result Json.Decode.Error a
decodeString a =
    Json.Decode.decodeString (decoder a)


{-| Run a `Codec` to decode some JSON `Value`. You can send these JSON values
through ports, so that is probably the main time you would use this function.
-}
decodeValue : Codec a -> Json.Decode.Value -> Result Json.Decode.Error a
decodeValue a =
    Json.Decode.decodeValue (decoder a)



-- PRIMITIVES


{-| `Codec` between a JSON boolean and an Elm `Bool`
-}
bool : Codec Bool
bool =
    build Json.Encode.bool Json.Decode.bool


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    build Json.Encode.int Json.Decode.int


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    build Json.Encode.float Json.Decode.float


{-| `Codec` between a JSON string of length 1 and an Elm `Char`
-}
char : Codec Char
char =
    build
        (String.fromChar >> Json.Encode.string)
        (Json.Decode.string
            |> Json.Decode.andThen
                (\x ->
                    case String.uncons x of
                        Just ( h, "" ) ->
                            Json.Decode.succeed h

                        _ ->
                            Json.Decode.fail "Expecting character."
                )
        )


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    build Json.Encode.string Json.Decode.string



-- RECORDS


{-| A partially built `Codec` for an record.
-}
type Record a b
    = Record
        { encoder : a -> List ( String, Json.Decode.Value )
        , decoder : Json.Decode.Decoder b
        }


{-| Start creating a `Codec` for an record. You should pass the main constructor as argument.
If you don't have one (for example it's a simple type with no name), you should pass a function that given the field values builds an record.

Example with constructor:

    type alias Point =
        { x : Float
        , y : Float
        }

    pointCodec : Codec Point
    pointCodec =
        Codec.record Point
            |> Codec.field "x" .x Codec.float
            |> Codec.field "y" .y Codec.float
            |> Codec.buildObject

Example without constructor:

    pointCodec : Codec { x : Int, y : Bool }
    pointCodec =
        Codec.record (\x y -> { x = x, y = y })
            |> Codec.field "x" .x Codec.int
            |> Codec.field "y" .y Codec.bool
            |> Codec.buildObject

-}
record : b -> Record a b
record ctor =
    Record
        { encoder = \_ -> []
        , decoder = Json.Decode.succeed ctor
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : String -> (a -> f) -> Codec f -> Record a (f -> b) -> Record a b
field name getter codec (Record ocodec) =
    Record
        { encoder = \v -> ( name, encoder codec <| getter v ) :: ocodec.encoder v
        , decoder = Json.Decode.map2 (\f x -> f x) ocodec.decoder (Json.Decode.field name (decoder codec))
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`.
If the optional field's value is `Nothing` then the resulting record will not contain that field.

-}
maybeField : String -> (a -> Maybe f) -> Codec f -> Record a (Maybe f -> b) -> Record a b
maybeField name getter codec (Record ocodec) =
    Record
        { encoder =
            \v ->
                case getter v of
                    Just present ->
                        ( name, encoder codec present ) :: ocodec.encoder v

                    Nothing ->
                        ocodec.encoder v
        , decoder =
            decoder codec
                |> Json.Decode.field name
                |> Json.Decode.maybe
                |> Json.Decode.map2 (\f x -> f x) ocodec.decoder
        }


{-| Specify the name getter and `Codec` for a required field, whose value can be `null`.

If the field is not present in the input then _the decoding fails_.
If the field's value is `Nothing` then the resulting record will contain the field with a `null` value.

This is a shorthand for a field having a codec built using `Codec.maybe`.

-}
nullableField : String -> (a -> Maybe f) -> Codec f -> Record a (Maybe f -> b) -> Record a b
nullableField name getter codec ocodec =
    field name getter (maybe codec) ocodec


{-| Create a `Codec` from a fully specified `Record`.
-}
buildObject : Record a a -> Codec a
buildObject (Record om) =
    Codec
        { encoder = \v -> Json.Encode.object <| List.reverse <| om.encoder v
        , decoder = om.decoder
        }



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : match
        , decoder : Dict.Dict String (Json.Decode.Decoder v)
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
    -> ((List Json.Decode.Value -> Json.Decode.Value) -> a)
    -> Json.Decode.Decoder v
    -> CustomCodec (a -> b) v
    -> CustomCodec b v
variant name matchPiece decoderPiece (CustomCodec am) =
    let
        enc v =
            Json.Encode.object
                [ ( "tag", Json.Encode.string name )
                , ( "args", Json.Encode.list identity v )
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
    -> CustomCodec (Json.Decode.Value -> a) v
    -> CustomCodec a v
variant0 name ctor =
    variant name
        (\c -> c [])
        (Json.Decode.succeed ctor)


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    String
    -> (a -> v)
    -> Codec a
    -> CustomCodec ((a -> Json.Decode.Value) -> b) v
    -> CustomCodec b v
variant1 name ctor m1 =
    variant name
        (\c v ->
            c
                [ encoder m1 v
                ]
        )
        (Json.Decode.map ctor
            (Json.Decode.index 0 <| decoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    String
    -> (a -> b -> v)
    -> Codec a
    -> Codec b
    -> CustomCodec ((a -> b -> Json.Decode.Value) -> c) v
    -> CustomCodec c v
variant2 name ctor m1 m2 =
    variant name
        (\c v1 v2 ->
            c
                [ encoder m1 v1
                , encoder m2 v2
                ]
        )
        (Json.Decode.map2 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
        )


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    String
    -> (a -> b -> c -> v)
    -> Codec a
    -> Codec b
    -> Codec c
    -> CustomCodec ((a -> b -> c -> Json.Decode.Value) -> partial) v
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
        (Json.Decode.map3 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
            (Json.Decode.index 2 <| decoder m3)
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
    -> CustomCodec ((a -> b -> c -> d -> Json.Decode.Value) -> partial) v
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
        (Json.Decode.map4 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
            (Json.Decode.index 2 <| decoder m3)
            (Json.Decode.index 3 <| decoder m4)
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
    -> CustomCodec ((a -> b -> c -> d -> e -> Json.Decode.Value) -> partial) v
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
        (Json.Decode.map5 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
            (Json.Decode.index 2 <| decoder m3)
            (Json.Decode.index 3 <| decoder m4)
            (Json.Decode.index 4 <| decoder m5)
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
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> Json.Decode.Value) -> partial) v
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
        (Json.Decode.map6 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
            (Json.Decode.index 2 <| decoder m3)
            (Json.Decode.index 3 <| decoder m4)
            (Json.Decode.index 4 <| decoder m5)
            (Json.Decode.index 5 <| decoder m6)
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
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> Json.Decode.Value) -> partial) v
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
        (Json.Decode.map7 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
            (Json.Decode.index 2 <| decoder m3)
            (Json.Decode.index 3 <| decoder m4)
            (Json.Decode.index 4 <| decoder m5)
            (Json.Decode.index 5 <| decoder m6)
            (Json.Decode.index 6 <| decoder m7)
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
    -> CustomCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Json.Decode.Value) -> partial) v
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
        (Json.Decode.map8 ctor
            (Json.Decode.index 0 <| decoder m1)
            (Json.Decode.index 1 <| decoder m2)
            (Json.Decode.index 2 <| decoder m3)
            (Json.Decode.index 3 <| decoder m4)
            (Json.Decode.index 4 <| decoder m5)
            (Json.Decode.index 5 <| decoder m6)
            (Json.Decode.index 6 <| decoder m7)
            (Json.Decode.index 7 <| decoder m8)
        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> Json.Decode.Value) a -> Codec a
buildCustom (CustomCodec am) =
    Codec
        { encoder = \v -> am.match v
        , decoder =
            Json.Decode.field "tag" Json.Decode.string
                |> Json.Decode.andThen
                    (\tag ->
                        case Dict.get tag am.decoder of
                            Nothing ->
                                Json.Decode.fail <| "tag " ++ tag ++ " did not match"

                            Just dec ->
                                Json.Decode.field "args" dec
                    )
        }



-- DATA STRUCTURES


composite : ((b -> Json.Decode.Value) -> (a -> Json.Decode.Value)) -> (Json.Decode.Decoder b -> Json.Decode.Decoder a) -> Codec b -> Codec a
composite enc dec (Codec codec) =
    Codec
        { encoder = enc codec.encoder
        , decoder = dec codec.decoder
        }


{-| Represents an optional value.
-}
maybe : Codec a -> Codec (Maybe a)
maybe codec =
    Codec
        { decoder = Json.Decode.maybe <| decoder codec
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        Json.Encode.null

                    Just x ->
                        encoder codec x
        }


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list =
    composite Json.Encode.list Json.Decode.list


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array.Array a)
array =
    composite Json.Encode.array Json.Decode.array


{-| `Codec` between a JSON object and an Elm `Dict`.
-}
dict : Codec a -> Codec (Dict.Dict String a)
dict =
    composite
        (\e -> Json.Encode.object << Dict.toList << Dict.map (\_ -> e))
        Json.Decode.dict


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set.Set comparable)
set =
    composite
        (\e -> Json.Encode.list e << Set.toList)
        (Json.Decode.map Set.fromList << Json.Decode.list)


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple m1 m2 =
    Codec
        { encoder =
            \( v1, v2 ) ->
                Json.Encode.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    ]
        , decoder =
            Json.Decode.map2
                (\a b -> ( a, b ))
                (Json.Decode.index 0 <| decoder m1)
                (Json.Decode.index 1 <| decoder m2)
        }


{-| `Codec` between a JSON array of length 3 and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple m1 m2 m3 =
    Codec
        { encoder =
            \( v1, v2, v3 ) ->
                Json.Encode.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    , encoder m3 v3
                    ]
        , decoder =
            Json.Decode.map3
                (\a b c -> ( a, b, c ))
                (Json.Decode.index 0 <| decoder m1)
                (Json.Decode.index 1 <| decoder m2)
                (Json.Decode.index 2 <| decoder m3)
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
        , decoder = Json.Decode.oneOf <| decoder main :: List.map decoder alts
        }



-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map go back codec =
    Codec
        { decoder = Json.Decode.map go <| decoder codec
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
        { decoder = Json.Decode.fail msg
        , encoder = always Json.Encode.null
        }


{-| Create codecs that depend on previous results.
-}
andThen : (a -> Codec b) -> (b -> a) -> Codec a -> Codec b
andThen dec enc c =
    Codec
        { decoder = decoder c |> Json.Decode.andThen (dec >> decoder)
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
        { decoder = Json.Decode.succeed default_
        , encoder = \_ -> Json.Encode.null
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
        { decoder = Json.Decode.lazy (\_ -> decoder <| f ())
        , encoder = \v -> encoder (f ()) v
        }


{-| Create a `Codec` that doesn't transform the JSON value, just brings it to and from Elm as a `Value`.
-}
value : Codec Json.Decode.Value
value =
    Codec
        { encoder = identity
        , decoder = Json.Decode.value
        }
