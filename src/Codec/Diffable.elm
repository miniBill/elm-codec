module Codec.Diffable exposing
    ( Codec, Value, Error
    , Decoder, decoder, decodeString, decodeValue
    , encoder, encodeToString, encodeToValue
    , string, bool, int, float, char
    , maybe, list, array, dict, set, tuple, triple, result
    , ObjectCodec, object, field, maybeField, nullableField, buildObject
    , CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom
    , oneOf
    , map
    , succeed, fail, andThen, value, build
    , diff, patch
    ,  constant
       -- , recursive, lazy

    )

{-| A `Codec a` contain a JSON `Decoder a` and the corresponding `a -> Value` encoder.


# Definition

@docs Codec, Value, Error


# Decode

@docs Decoder, decoder, decodeString, decodeValue


# Encode

@docs encoder, encodeToString, encodeToValue


# Primitives

@docs string, bool, int, float, char


# Data Structures

@docs maybe, list, array, dict, set, tuple, triple, result


# Object Primitives

@docs ObjectCodec, object, field, maybeField, nullableField, buildObject


# Custom Types

@docs CustomCodec, custom, variant0, variant1, variant2, variant3, variant4, variant5, variant6, variant7, variant8, buildCustom


# Inconsistent structure

@docs oneOf


# Mapping

@docs map


# Fancy Codecs

@docs succeed, fail, andThen, value, build, constant --, recursive, lazy


# Diff and Patch

@docs diff, patch

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
        , diff : a -> a -> Maybe Value
        , patch : a -> Decoder a
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



-- DIFF and PATCH


{-| Encodes the difference between two values for later use with `patch`.

The `Codec`s defined in elm-codec have the guarantees that:

    Json.Decode.decodeValue (patch codec old) (diff codec old new) == Ok new

-}
diff : Codec a -> a -> a -> Value
diff (Codec m) old new =
    m.diff old new |> Maybe.withDefault JE.null


{-| Applies a difference calculated with `diff` to an old value.

The `Codec`s defined in elm-codec have the guarantees that:

    Json.Decode.decodeValue (patch codec old) (diff codec old new) == Ok new

-}
patch : Codec a -> a -> Decoder a
patch (Codec m) =
    m.patch



-- BASE


{-| Build your own custom `Codec`.
Useful if you have pre-existing `Decoder`s you need to use.
-}
build : (a -> Value) -> Decoder a -> Codec a
build encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        , diff = \_ -> Just << encoder_
        , patch = always decoder_
        }


basic : (a -> Value) -> Decoder a -> Codec a
basic encoder_ decoder_ =
    Codec
        { encoder = encoder_
        , decoder = decoder_
        , diff =
            \old new ->
                if old == new then
                    Nothing

                else
                    Just <| encoder_ new
        , patch = \old -> JD.oneOf [ decoder_, JD.succeed old ]
        }


{-| `Codec` between a JSON string and an Elm `String`
-}
string : Codec String
string =
    basic JE.string JD.string


{-| `Codec` between a JSON boolean and an Elm `Bool`
-}
bool : Codec Bool
bool =
    basic JE.bool JD.bool


{-| `Codec` between a JSON number and an Elm `Int`
-}
int : Codec Int
int =
    basic JE.int JD.int


{-| `Codec` between a JSON number and an Elm `Float`
-}
float : Codec Float
float =
    basic JE.float JD.float


{-| `Codec` between a JSON string of length 1 and an Elm `Char`
-}
char : Codec Char
char =
    basic
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



-- DATA STRUCTURES


{-| Represents an optional value.
-}
maybe : Codec a -> Codec (Maybe a)
maybe (Codec codec) =
    let
        patch_ : Maybe a -> Decoder (Maybe a)
        patch_ old =
            JD.oneOf
                [ JD.list JD.value
                    |> JD.andThen
                        (\l ->
                            case ( l, old ) of
                                ( [], _ ) ->
                                    JD.succeed Nothing

                                ( [ v ], _ ) ->
                                    JD.map Just <| toDecoder <| JD.decodeValue codec.decoder v

                                ( [ _, d ], Just o ) ->
                                    JD.map Just <| toDecoder <| JD.decodeValue (codec.patch o) d

                                _ ->
                                    JD.fail "unexpected patch for Maybe"
                        )
                , JD.succeed old
                ]
    in
    Codec
        { decoder = JD.maybe codec.decoder
        , encoder =
            \v ->
                case v of
                    Nothing ->
                        JE.null

                    Just x ->
                        codec.encoder x
        , diff =
            \old new ->
                case ( old, new ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( Just _, Nothing ) ->
                        Just <| JE.list identity []

                    ( Nothing, Just n ) ->
                        Just <| JE.list identity [ codec.encoder n ]

                    ( Just o, Just n ) ->
                        case codec.diff o n of
                            Just d ->
                                Just <| JE.list identity [ JE.int 0, d ]

                            Nothing ->
                                Nothing
        , patch = patch_
        }


toDecoder : Result JD.Error x -> Decoder x
toDecoder x =
    case x of
        Ok r ->
            JD.succeed r

        Err e ->
            JD.fail <| JD.errorToString e


traverse : List (Result e a) -> Result e (List a)
traverse =
    List.foldr (Result.map2 (::)) (Ok [])


{-| `Codec` between a JSON array and an Elm `List`.
-}
list : Codec a -> Codec (List a)
list (Codec c) =
    Codec
        { decoder = JD.list c.decoder
        , encoder = JE.list c.encoder
        , diff =
            \old new ->
                let
                    raw =
                        List.map2 c.diff old new

                    added =
                        List.drop (List.length old) new

                    diffs =
                        if List.all (\d -> d == Nothing) raw then
                            []

                        else
                            List.map (Maybe.withDefault JE.null) raw

                    removed =
                        List.length old - List.length new

                    addif cond v l =
                        if cond then
                            v :: l

                        else
                            l
                in
                []
                    -- If diffs is not empty, it implies the number of removed items
                    |> addif (removed > 0 && List.isEmpty diffs) ( "r", JE.int removed )
                    |> addif (not <| List.isEmpty diffs) ( "d", JE.list identity <| diffs )
                    |> addif (not <| List.isEmpty added) ( "a", JE.list c.encoder <| added )
                    |> (\l ->
                            if List.isEmpty l then
                                Nothing

                            else
                                Just <| JE.object l
                       )
        , patch =
            \old ->
                JD.map3
                    (\removed diffs added -> { removed = removed, diffs = diffs, added = added })
                    (JD.maybe <| JD.field "r" JD.int)
                    (JD.maybe <| JD.field "d" (JD.list JD.value))
                    (JD.maybe <| JD.field "a" (JD.list c.decoder))
                    |> JD.andThen
                        (\{ removed, diffs, added } ->
                            let
                                afterRemove =
                                    case removed of
                                        Nothing ->
                                            old

                                        Just r ->
                                            old
                                                |> List.reverse
                                                |> List.drop r
                                                |> List.reverse

                                afterDiff =
                                    case diffs of
                                        Nothing ->
                                            JD.succeed afterRemove

                                        Just d ->
                                            List.map2 (\o d_ -> JD.decodeValue (c.patch o) d_) old d
                                                |> traverse
                                                |> toDecoder

                                afterAdd =
                                    JD.map (\a -> a ++ Maybe.withDefault [] added) afterDiff
                            in
                            afterAdd
                        )
        }


{-| `Codec` between a JSON array and an Elm `Array`.
-}
array : Codec a -> Codec (Array a)
array =
    map Array.fromList Array.toList << list


{-| `Codec` between a JSON object and an Elm `Dict`.
-}
dict : Codec a -> Codec (Dict String a)
dict (Codec c) =
    let
        diff_ old new =
            let
                merged =
                    new
                        |> Dict.toList
                        |> List.filterMap
                            (\( k, v ) ->
                                case Dict.get k old of
                                    Nothing ->
                                        Just ( k, c.encoder v )

                                    Just o ->
                                        Maybe.map (\d -> ( k, d )) <| c.diff o v
                            )

                removed =
                    Dict.diff old new |> Dict.keys |> List.map JE.string
            in
            if List.isEmpty removed then
                if List.isEmpty merged then
                    Nothing

                else
                    Just <| JE.object merged

            else
                Just <| JE.list identity <| JE.object merged :: removed
    in
    Codec
        { encoder = JE.object << Dict.toList << Dict.map (\_ -> c.encoder)
        , decoder = JD.dict c.decoder
        , diff = diff_
        , patch =
            \old ->
                let
                    mergedDecoder : Dict String a -> JD.Decoder (Dict String a)
                    mergedDecoder oldClean =
                        JD.dict JD.value
                            |> JD.andThen
                                (\merged ->
                                    merged
                                        |> Dict.toList
                                        |> List.map
                                            (\( k, v ) ->
                                                Result.map (Tuple.pair k) <|
                                                    JD.decodeValue
                                                        (case Dict.get k oldClean of
                                                            Nothing ->
                                                                c.decoder

                                                            Just o ->
                                                                c.patch o
                                                        )
                                                        v
                                            )
                                        |> traverse
                                        |> Result.map Dict.fromList
                                        |> toDecoder
                                )
                in
                JD.oneOf
                    [ JD.list JD.value
                        |> JD.andThen
                            (\l ->
                                case l of
                                    [] ->
                                        JD.fail "Expected at least one element"

                                    merged :: removed ->
                                        removed
                                            |> List.map (JD.decodeValue JD.string)
                                            |> traverse
                                            |> Result.andThen
                                                (\removedStrings ->
                                                    merged
                                                        |> JD.decodeValue (mergedDecoder <| List.foldr Dict.remove old removedStrings)
                                                )
                                            |> toDecoder
                            )
                    , mergedDecoder old
                    , JD.null old
                    , JD.value
                        |> JD.andThen
                            (\v ->
                                JD.fail <| "expected a list merged::removed or just merged, got " ++ valueToString v
                            )
                    ]
        }


valueToString : Value -> String
valueToString w =
    let
        escapeString s =
            "\"" ++ String.replace "\"" "\\\"" s ++ "\""

        mainDec () =
            JD.oneOf
                [ JD.string |> JD.map escapeString
                , JD.int |> JD.map String.fromInt
                , JD.float |> JD.map String.fromFloat
                , JD.bool
                    |> JD.map
                        (\t ->
                            if t then
                                "true"

                            else
                                "false"
                        )
                , JD.null "null"
                , JD.list (JD.lazy mainDec)
                    |> JD.map (\l -> "[" ++ String.join ", " l ++ "]")
                , JD.dict (JD.lazy mainDec)
                    |> JD.map
                        (\d ->
                            "{"
                                ++ String.join ", "
                                    (Dict.toList d
                                        |> List.map
                                            (\( dk, dv ) -> escapeString dk ++ ": " ++ dv)
                                    )
                                ++ "}"
                        )
                ]
    in
    JD.decodeValue (mainDec ()) w
        |> Result.withDefault "???"


{-| `Codec` between a JSON array and an Elm `Set`.
-}
set : Codec comparable -> Codec (Set comparable)
set =
    map Set.fromList Set.toList << list


{-| `Codec` between a JSON array of length 2 and an Elm `Tuple`.
-}
tuple : Codec a -> Codec b -> Codec ( a, b )
tuple ((Codec c1) as m1) ((Codec c2) as m2) =
    Codec
        { encoder =
            \( v1, v2 ) ->
                JE.list identity
                    [ encoder m1 v1
                    , encoder m2 v2
                    ]
        , decoder =
            JD.map2
                Tuple.pair
                (JD.index 0 <| decoder m1)
                (JD.index 1 <| decoder m2)
        , diff =
            \( o1, o2 ) ( n1, n2 ) ->
                case ( c1.diff o1 n1, c2.diff o2 n2 ) of
                    ( Nothing, Nothing ) ->
                        Nothing

                    ( d1, d2 ) ->
                        Just <|
                            JE.list identity
                                [ Maybe.withDefault JE.null d1
                                , Maybe.withDefault JE.null d2
                                ]
        , patch =
            \( o1, o2 ) ->
                JD.oneOf
                    [ JD.map2 Tuple.pair
                        (JD.index 0 <| c1.patch o1)
                        (JD.index 1 <| c2.patch o2)
                    , JD.succeed ( o1, o2 )
                    ]
        }


{-| `Codec` between a JSON array of length 3 and an Elm triple.
-}
triple : Codec a -> Codec b -> Codec c -> Codec ( a, b, c )
triple ((Codec c1) as m1) ((Codec c2) as m2) ((Codec c3) as m3) =
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
        , diff =
            \( o1, o2, o3 ) ( n1, n2, n3 ) ->
                case ( c1.diff o1 n1, c2.diff o2 n2, c3.diff o3 n3 ) of
                    ( Nothing, Nothing, Nothing ) ->
                        Nothing

                    ( d1, d2, d3 ) ->
                        Just <|
                            JE.list identity
                                [ Maybe.withDefault JE.null d1
                                , Maybe.withDefault JE.null d2
                                , Maybe.withDefault JE.null d3
                                ]
        , patch =
            \( o1, o2, o3 ) ->
                JD.oneOf
                    [ JD.map3 (\a b c -> ( a, b, c ))
                        (JD.index 0 <| c1.patch o1)
                        (JD.index 1 <| c2.patch o2)
                        (JD.index 2 <| c3.patch o3)
                    , JD.succeed ( o1, o2, o3 )
                    ]
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
        , diff : a -> a -> List ( String, Value )
        , decoder : Decoder b
        , patch : a -> Decoder b
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
        , diff = \_ _ -> []
        , patch = \_ -> JD.succeed ctor
        }


{-| Specify the name, getter and `Codec` for a field.

The name is only used as the field name in the resulting JSON, and has no impact on the Elm side.

-}
field : String -> (a -> f) -> Codec f -> ObjectCodec a (f -> b) -> ObjectCodec a b
field name getter ((Codec c) as codec) (ObjectCodec ocodec) =
    ObjectCodec
        { encoder = \v -> ( name, encoder codec <| getter v ) :: ocodec.encoder v
        , decoder = JD.map2 (\f x -> f x) ocodec.decoder (JD.field name (decoder codec))
        , diff =
            \old new ->
                let
                    inner =
                        c.diff (getter old) (getter new)
                in
                case inner of
                    Nothing ->
                        ocodec.diff old new

                    Just d ->
                        ( name, d ) :: ocodec.diff old new
        , patch =
            \old ->
                JD.map2 (\f x -> f x)
                    (ocodec.patch old)
                    (JD.oneOf
                        [ JD.field name (c.patch (getter old))
                        , JD.succeed <| getter old
                        ]
                    )
        }


{-| Specify the name getter and `Codec` for an optional field.

This is particularly useful for evolving your `Codec`s.

If the field is not present in the input then it gets decoded to `Nothing`.
If the optional field's value is `Nothing` then the resulting object will not contain that field.

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
        , diff =
            \old new ->
                let
                    (Codec maybecodec) =
                        maybe codec

                    inner =
                        maybecodec.diff (getter old) (getter new)
                in
                case inner of
                    Nothing ->
                        ocodec.diff old new

                    Just d ->
                        ( name, d ) :: ocodec.diff old new
        , patch =
            \old ->
                let
                    (Codec maybecodec) =
                        maybe codec
                in
                JD.map2 (\f x -> f x)
                    (ocodec.patch old)
                    (JD.oneOf
                        [ JD.field name (maybecodec.patch (getter old))
                        , JD.succeed <| getter old
                        ]
                    )
        }


{-| Specify the name getter and `Codec` for a required field, whose value can be `null`.

If the field is not present in the input then _the decoding fails_.
If the field's value is `Nothing` then the resulting object will contain the field with a `null` value.

This is a shorthand for a field having a codec built using `Codec.maybe`.

-}
nullableField : String -> (a -> Maybe f) -> Codec f -> ObjectCodec a (Maybe f -> b) -> ObjectCodec a b
nullableField name getter codec ocodec =
    field name getter (maybe codec) ocodec


{-| Create a `Codec` from a fully specified `ObjectCodec`.
-}
buildObject : ObjectCodec a a -> Codec a
buildObject (ObjectCodec om) =
    Codec
        { encoder = \v -> JE.object <| om.encoder v
        , decoder = om.decoder
        , diff =
            \old new ->
                case om.diff old new of
                    [] ->
                        Nothing

                    ls ->
                        Just <| JE.object ls
        , patch = om.patch
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
        | Yellow Float
        | Green

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.custom
            (\red yellow green value ->
                case value of
                    Red i s ->
                        red i s

                    Yellow f ->
                        yellow f

                    Green ->
                        green
            )
            |> Codec.variant2 "Red" Red Codec.int Codec.string
            |> Codec.variant1 "Yellow" Yellow Codec.float
            |> Codec.variant0 "Green" Green
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


{-| Define a variant with 0 parameters for a custom type. The first argument is
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
        { encoder = am.match
        , decoder =
            JD.field "tag" JD.string
                |> JD.andThen
                    (\tag ->
                        case Dict.get tag am.decoder of
                            Nothing ->
                                JD.fail <| "tag " ++ tag ++ "did not match"

                            Just dec ->
                                JD.field "args" dec
                    )
        , diff =
            \old new ->
                if old == new then
                    Nothing

                else
                    Just <| am.match new
        , patch =
            \old ->
                JD.oneOf
                    [ JD.null old
                    , JD.field "tag" JD.string
                        |> JD.andThen
                            (\tag ->
                                case Dict.get tag am.decoder of
                                    Nothing ->
                                        JD.fail <| "tag " ++ tag ++ "did not match"

                                    Just dec ->
                                        JD.field "args" dec
                            )
                    ]
        }



-- INCONSISTENT STRUCTURE


{-| Try a set of decoders (in order).
The first argument is used for encoding and decoding, the list of other codecs is used as a fallback while decoding.

This is particularly useful for backwards compatibility. You would pass the current codec as the first argument,
and the old ones (eventually `map`ped) as a fallback list to use while decoding.

-}
oneOf : Codec a -> List (Codec a) -> Codec a
oneOf (Codec main) alts =
    Codec
        { encoder = main.encoder
        , decoder = JD.oneOf <| main.decoder :: List.map decoder alts
        , diff = main.diff
        , patch = \old -> JD.oneOf <| List.map (\d -> d.patch old) <| main :: List.map (\(Codec d) -> d) alts
        }



-- MAPPING


{-| Transform a `Codec`.
-}
map : (a -> b) -> (b -> a) -> Codec a -> Codec b
map go back (Codec codec) =
    Codec
        { decoder = JD.map go codec.decoder
        , encoder = \v -> codec.encoder <| back v
        , diff = \old new -> codec.diff (back old) (back new)
        , patch = \old -> JD.map go <| codec.patch (back old)
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
        , diff = \_ _ -> Nothing
        , patch = \_ -> JD.fail msg
        }


{-| Create codecs that depend on previous results.
-}
andThen : (a -> Codec b) -> (b -> a) -> Codec a -> Codec b
andThen dec enc (Codec c) =
    Codec
        { decoder = c.decoder |> JD.andThen (dec >> decoder)
        , encoder = c.encoder << enc
        , diff = \old new -> c.diff (enc old) (enc new)
        , patch = \old -> old |> enc |> c.patch |> JD.andThen (dec >> decoder)
        }


{-
   {-| Create a `Codec` for a recursive data structure.
   The argument to the function you need to pass is the fully formed `Codec`.
   -}
   recursive : (Codec a -> Codec a) -> Codec a
   recursive f =
       f <| lazy (\_ -> recursive f)
-}


{-| Create a `Codec` that produces null as JSON and always decodes as the same value.
-}
succeed : a -> Codec a
succeed default_ =
    Codec
        { decoder = JD.succeed default_
        , encoder = \_ -> JE.null
        , diff = \_ _ -> Nothing
        , patch = \_ -> JD.succeed default_
        }


{-| Create a `Codec` that produces null as JSON and always decodes as the same value. Obsolete alias of `succeed`, will be removed in a future version.
-}
constant : a -> Codec a
constant =
    succeed



{-
   {-| This is useful for recursive structures that are not easily modeled with `recursive`.
   Have a look at the Json.Decode docs for examples.
   -}



   lazy : (() -> Codec a) -> Codec a
   lazy f =
          Codec
              { decoder = JD.lazy (\_ -> decoder <| f ())
              , encoder = \v -> encoder (f ()) v
              , diff = \_ new -> Just <| encoder (f ()) new
              , patch = \_ -> JD.lazy (\_ -> decoder <| f ())
              }
-}


{-| Create a `Codec` that doesn't transform the JSON value, just brings it to and from Elm as a `Value`.
-}
value : Codec Value
value =
    Codec
        { encoder = identity
        , decoder = JD.value
        , diff = \_ new -> Just new
        , patch = \_ -> JD.value
        }
