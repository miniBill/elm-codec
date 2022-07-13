module Codec exposing
    ( Codec, build
    , encoder, encodeToString, encodeToValue
    , decoder, decodeString, decodeValue
    , tuple, tuple3
    , Record, record, field, maybeField, buildRecord
    , Custom, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , int, float, char, string, list, array, dict, set
    , bool, maybe, result
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


# Tuples

@docs tuple, tuple3


# Records

@docs Record, record, field, maybeField, buildRecord


# Custom Types

@docs Custom, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Opaque Custom Types

@docs int, float, char, string, list, array, dict, set


# Common Custom Types

@docs bool, maybe, result


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



-- TUPLES


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple a b =
    Codec
        { encoder =
            \( x1, x2 ) ->
                Json.Encode.list identity
                    [ encoder a x1
                    , encoder b x2
                    ]
        , decoder =
            Json.Decode.map2
                Tuple.pair
                (Json.Decode.index 0 (decoder a))
                (Json.Decode.index 1 (decoder b))
        }


{-| `Codec` between a JSON array of length 3 and an Elm tuple3.
-}
tuple3 : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
tuple3 a b c =
    Codec
        { encoder =
            \( x1, x2, x3 ) ->
                Json.Encode.list identity
                    [ encoder a x1
                    , encoder b x2
                    , encoder c x3
                    ]
        , decoder =
            Json.Decode.map3
                (\x1 x2 x3 -> ( x1, x2, x3 ))
                (Json.Decode.index 0 (decoder a))
                (Json.Decode.index 1 (decoder b))
                (Json.Decode.index 2 (decoder c))
        }



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
            |> Codec.buildRecord

Example without constructor:

    pointCodec : Codec { x : Int, y : Bool }
    pointCodec =
        Codec.record (\x y -> { x = x, y = y })
            |> Codec.field "x" .x Codec.int
            |> Codec.field "y" .y Codec.bool
            |> Codec.buildRecord

-}
record : b -> Record a b
record a =
    Record
        { encoder = \_ -> []
        , decoder = Json.Decode.succeed a
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : String -> (a -> c) -> Codec c -> Record a (c -> b) -> Record a b
field name getter codec (Record a) =
    Record
        { encoder = \x -> ( name, encoder codec (getter x) ) :: a.encoder x
        , decoder = Json.Decode.map2 (\f x -> f x) a.decoder (Json.Decode.field name (decoder codec))
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`.
If the optional field's value is `Nothing` then the resulting record will not contain that field.

-}
maybeField : String -> (a -> Maybe c) -> Codec c -> Record a (Maybe c -> b) -> Record a b
maybeField name getter codec (Record a) =
    Record
        { encoder =
            \x ->
                case getter x of
                    Just x2 ->
                        ( name, encoder codec x2 ) :: a.encoder x

                    Nothing ->
                        a.encoder x
        , decoder =
            Json.Decode.oneOf
                [ Json.Decode.field name Json.Decode.value
                    |> Json.Decode.andThen (\_ -> decoder (maybe codec))
                , Json.Decode.succeed Nothing
                ]
                |> Json.Decode.map2 (\f x -> f x) a.decoder
        }


{-| Create a `Codec` from a fully specified `Record`.
-}
buildRecord : Record a a -> Codec a
buildRecord (Record a) =
    Codec
        { encoder = \x -> a.encoder x |> List.reverse |> Json.Encode.object
        , decoder = a.decoder
        }



-- CUSTOM TYPES


{-| A partially built `Codec` for a custom type.
-}
type Custom x a
    = Custom
        { encoder : x
        , decoder : Dict.Dict String (Json.Decode.Decoder a)
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
custom : x -> Custom x a
custom a =
    Custom
        { encoder = a
        , decoder = Dict.empty
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    String
    -> a
    -> Custom (Json.Decode.Value -> x) a
    -> Custom x a
variant0 name fn (Custom a) =
    let
        encoder_ : Json.Encode.Value
        encoder_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                ]

        decoder_ : Json.Decode.Decoder a
        decoder_ =
            Json.Decode.succeed fn
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    String
    -> (a -> value)
    -> Codec a
    -> Custom ((a -> Json.Decode.Value) -> x) value
    -> Custom x value
variant1 name fn codecA (Custom a) =
    let
        encoder_ : a -> Json.Encode.Value
        encoder_ a_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map fn
                (Json.Decode.index 1 (decoder codecA))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 2 parameters for a custom type.
-}
variant2 :
    String
    -> (a -> b -> value)
    -> Codec a
    -> Codec b
    -> Custom ((a -> b -> Json.Decode.Value) -> x) value
    -> Custom x value
variant2 name fn codecA codecB (Custom a) =
    let
        encoder_ : a -> b -> Json.Encode.Value
        encoder_ a_ b_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map2 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 3 parameters for a custom type.
-}
variant3 :
    String
    -> (a -> b -> c -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Custom ((a -> b -> c -> Json.Decode.Value) -> x) value
    -> Custom x value
variant3 name fn codecA codecB codecC (Custom a) =
    let
        encoder_ : a -> b -> c -> Json.Encode.Value
        encoder_ a_ b_ c_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map3 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 4 parameters for a custom type.
-}
variant4 :
    String
    -> (a -> b -> c -> d -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Custom ((a -> b -> c -> d -> Json.Decode.Value) -> x) value
    -> Custom x value
variant4 name fn codecA codecB codecC codecD (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map4 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 5 parameters for a custom type.
-}
variant5 :
    String
    -> (a -> b -> c -> d -> e -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Custom ((a -> b -> c -> d -> e -> Json.Decode.Value) -> x) value
    -> Custom x value
variant5 name fn codecA codecB codecC codecD codecE (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map5 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 6 parameters for a custom type.
-}
variant6 :
    String
    -> (a -> b -> c -> d -> e -> f -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Custom ((a -> b -> c -> d -> e -> f -> Json.Decode.Value) -> x) value
    -> Custom x value
variant6 name fn codecA codecB codecC codecD codecE codecF (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> f -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ f_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                , encoder codecF f_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map6 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
                (Json.Decode.index 6 (decoder codecF))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 7 parameters for a custom type.
-}
variant7 :
    String
    -> (a -> b -> c -> d -> e -> f -> g -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Custom ((a -> b -> c -> d -> e -> f -> g -> Json.Decode.Value) -> x) value
    -> Custom x value
variant7 name fn codecA codecB codecC codecD codecE codecF codecG (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> f -> g -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ f_ g_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                , encoder codecF f_
                , encoder codecG g_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map7 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
                (Json.Decode.index 6 (decoder codecF))
                (Json.Decode.index 7 (decoder codecG))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Define a variant with 8 parameters for a custom type.
-}
variant8 :
    String
    -> (a -> b -> c -> d -> e -> f -> g -> h -> value)
    -> Codec a
    -> Codec b
    -> Codec c
    -> Codec d
    -> Codec e
    -> Codec f
    -> Codec g
    -> Codec h
    -> Custom ((a -> b -> c -> d -> e -> f -> g -> h -> Json.Decode.Value) -> x) value
    -> Custom x value
variant8 name fn codecA codecB codecC codecD codecE codecF codecG codecH (Custom a) =
    let
        encoder_ : a -> b -> c -> d -> e -> f -> g -> h -> Json.Encode.Value
        encoder_ a_ b_ c_ d_ e_ f_ g_ h_ =
            Json.Encode.list identity
                [ Json.Encode.string name
                , encoder codecA a_
                , encoder codecB b_
                , encoder codecC c_
                , encoder codecD d_
                , encoder codecE e_
                , encoder codecF f_
                , encoder codecG g_
                , encoder codecH h_
                ]

        decoder_ : Json.Decode.Decoder value
        decoder_ =
            Json.Decode.map8 fn
                (Json.Decode.index 1 (decoder codecA))
                (Json.Decode.index 2 (decoder codecB))
                (Json.Decode.index 3 (decoder codecC))
                (Json.Decode.index 4 (decoder codecD))
                (Json.Decode.index 5 (decoder codecE))
                (Json.Decode.index 6 (decoder codecF))
                (Json.Decode.index 7 (decoder codecG))
                (Json.Decode.index 8 (decoder codecH))
    in
    Custom
        { encoder = a.encoder encoder_
        , decoder = Dict.insert name decoder_ a.decoder
        }


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : Custom (a -> Json.Decode.Value) a -> Codec a
buildCustom (Custom a) =
    Codec
        { encoder = \x -> a.encoder x
        , decoder =
            Json.Decode.index 0 Json.Decode.string
                |> Json.Decode.andThen
                    (\x ->
                        case Dict.get x a.decoder of
                            Nothing ->
                                Json.Decode.fail ("Unknown variant \"" ++ x ++ "\".")

                            Just x2 ->
                                x2
                    )
        }



-- OPAQUE CUSTOM TYPES


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


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list a =
    build (Json.Encode.list (encoder a)) (Json.Decode.list (decoder a))


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array.Array a)
array a =
    build (Json.Encode.array (encoder a)) (Json.Decode.array (decoder a))


{-| `Codec` between a JSON array and an Elm `Dict`.
-}
dict : Codec comparable -> Codec v -> Codec (Dict.Dict comparable v)
dict k v =
    list (tuple k v) |> map Dict.fromList Dict.toList


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set.Set comparable)
set a =
    list a |> map Set.fromList Set.toList



-- COMMON CUSTOM TYPES


{-| `Codec` for `Bool` values.
-}
bool : Codec Bool
bool =
    custom
        (\fn1 fn2 x ->
            case x of
                True ->
                    fn1

                False ->
                    fn2
        )
        |> variant0 "True" True
        |> variant0 "False" False
        |> buildCustom


{-| `Codec` for `Maybe` values.
-}
maybe : Codec a -> Codec (Maybe a)
maybe a =
    custom
        (\fn1 fn2 x ->
            case x of
                Just x1 ->
                    fn1 x1

                Nothing ->
                    fn2
        )
        |> variant1 "Just" Just a
        |> variant0 "Nothing" Nothing
        |> buildCustom


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
