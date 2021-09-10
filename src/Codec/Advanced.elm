module Codec.Advanced exposing (CustomObjectCodec, customObject, objectVariant0, objectVariant1, objectVariant2, objectVariant3, objectVariant4, objectVariant5, objectVariant6, objectVariant7, objectVariant8, buildCustomObject)

{-| Codecs that can encode/decode objects of a custom shape. These are similar to the codecs for custom types in the `Codec` module, but give you more control over the shape of the result.

@docs CustomObjectCodec, customObject, objectVariant0, objectVariant1, objectVariant2, objectVariant3, objectVariant4, objectVariant5, objectVariant6, objectVariant7, objectVariant8, buildCustomObject

-}

import Codec exposing (Codec, decoder, encoder)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


{-| A partially built `Codec` for an object with a custom shape.
-}
type CustomObjectCodec match v
    = CustomCodec
        { tagField : String
        , match : match
        , decoder : Dict String (Decoder v)
        }


{-| Starts building a `Codec` for an object with a custom shape.

You need to pass the field name there the variant name will be stored, and a pattern matching function, built like this:

    type Semaphore
        = Red Int String
        | Yellow
        | Green Float

    semaphoreCodec : Codec Semaphore
    semaphoreCodec =
        Codec.customObject "color"
            (\red yellow green value ->
                case value of
                    Red i s ->
                        red i s

                    Yellow ->
                        yellow

                    Green f ->
                        green f
            )
            |> Codec.objectVariant2 "Red" Red ( "width", Codec.int ) ( "label", Codec.string )
            |> Codec.objectVariant0 "Yellow" Yellow
            |> Codec.objectVariant1 "Green" Green ( "value", Codec.float )
            |> Codec.buildCustomObject

This codec would generate and parse values such as `{ "color": "Red", "width": 42, "label": "413" }`, `{ "color": "yellow" }` and `{ "color": "green", "value": 0.42 }`

-}
customObject : String -> match -> CustomObjectCodec match value
customObject tagField match =
    CustomCodec
        { tagField = tagField
        , match = match
        , decoder = Dict.empty
        }


objectVariant :
    String
    -> ((List ( String, Value ) -> Value) -> a)
    -> Decoder v
    -> CustomObjectCodec (a -> b) v
    -> CustomObjectCodec b v
objectVariant name matchPiece decoderPiece (CustomCodec am) =
    let
        enc v =
            JE.object <| ( am.tagField, JE.string name ) :: v
    in
    CustomCodec
        { tagField = am.tagField
        , match = am.match <| matchPiece enc
        , decoder = Dict.insert name decoderPiece am.decoder
        }


{-| Define a variant with 0 parameters for a custom type.
-}
objectVariant0 :
    String
    -> v
    -> CustomObjectCodec (Value -> c) v
    -> CustomObjectCodec c v
objectVariant0 name ctor =
    objectVariant name
        (\c -> c [])
        (JD.succeed ctor)


{-| Define a variant with 1 parameter for a custom type.
-}
objectVariant1 :
    String
    -> (a -> v)
    -> ( String, Codec a )
    -> CustomObjectCodec ((a -> Value) -> c) v
    -> CustomObjectCodec c v
objectVariant1 name ctor ( f1, m1 ) =
    objectVariant name
        (\c v1 ->
            c
                [ ( f1, encoder m1 v1 )
                ]
        )
        (JD.map ctor
            (JD.field f1 <| decoder m1)
        )


{-| Define a variant with 2 parameters for a custom type.
-}
objectVariant2 :
    String
    -> (a -> b -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> CustomObjectCodec ((a -> b -> Value) -> c) v
    -> CustomObjectCodec c v
objectVariant2 name ctor ( f1, m1 ) ( f2, m2 ) =
    objectVariant name
        (\c v1 v2 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                ]
        )
        (JD.map2 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
objectVariant3 :
    String
    -> (a -> b -> c -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> CustomObjectCodec ((a -> b -> c -> Value) -> k) v
    -> CustomObjectCodec k v
objectVariant3 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) =
    objectVariant name
        (\c v1 v2 v3 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                , ( f3, encoder m3 v3 )
                ]
        )
        (JD.map3 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
            (JD.field f3 <| decoder m3)
        )


{-| Define a variant with 4 parameters for a custom type.
-}
objectVariant4 :
    String
    -> (a -> b -> c -> d -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> CustomObjectCodec ((a -> b -> c -> d -> Value) -> k) v
    -> CustomObjectCodec k v
objectVariant4 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) =
    objectVariant name
        (\c v1 v2 v3 v4 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                , ( f3, encoder m3 v3 )
                , ( f4, encoder m4 v4 )
                ]
        )
        (JD.map4 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
            (JD.field f3 <| decoder m3)
            (JD.field f4 <| decoder m4)
        )


{-| Define a variant with 5 parameters for a custom type.
-}
objectVariant5 :
    String
    -> (a -> b -> c -> d -> e -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> ( String, Codec e )
    -> CustomObjectCodec ((a -> b -> c -> d -> e -> Value) -> k) v
    -> CustomObjectCodec k v
objectVariant5 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) =
    objectVariant name
        (\c v1 v2 v3 v4 v5 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                , ( f3, encoder m3 v3 )
                , ( f4, encoder m4 v4 )
                , ( f5, encoder m5 v5 )
                ]
        )
        (JD.map5 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
            (JD.field f3 <| decoder m3)
            (JD.field f4 <| decoder m4)
            (JD.field f5 <| decoder m5)
        )


{-| Define a variant with 6 parameters for a custom type.
-}
objectVariant6 :
    String
    -> (a -> b -> c -> d -> e -> f -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> ( String, Codec e )
    -> ( String, Codec f )
    -> CustomObjectCodec ((a -> b -> c -> d -> e -> f -> Value) -> k) v
    -> CustomObjectCodec k v
objectVariant6 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) =
    objectVariant name
        (\c v1 v2 v3 v4 v5 v6 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                , ( f3, encoder m3 v3 )
                , ( f4, encoder m4 v4 )
                , ( f5, encoder m5 v5 )
                , ( f6, encoder m6 v6 )
                ]
        )
        (JD.map6 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
            (JD.field f3 <| decoder m3)
            (JD.field f4 <| decoder m4)
            (JD.field f5 <| decoder m5)
            (JD.field f6 <| decoder m6)
        )


{-| Define a variant with 7 parameters for a custom type.
-}
objectVariant7 :
    String
    -> (a -> b -> c -> d -> e -> f -> g -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> ( String, Codec e )
    -> ( String, Codec f )
    -> ( String, Codec g )
    -> CustomObjectCodec ((a -> b -> c -> d -> e -> f -> g -> Value) -> k) v
    -> CustomObjectCodec k v
objectVariant7 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) ( f7, m7 ) =
    objectVariant name
        (\c v1 v2 v3 v4 v5 v6 v7 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                , ( f3, encoder m3 v3 )
                , ( f4, encoder m4 v4 )
                , ( f5, encoder m5 v5 )
                , ( f6, encoder m6 v6 )
                , ( f7, encoder m7 v7 )
                ]
        )
        (JD.map7 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
            (JD.field f3 <| decoder m3)
            (JD.field f4 <| decoder m4)
            (JD.field f5 <| decoder m5)
            (JD.field f6 <| decoder m6)
            (JD.field f7 <| decoder m7)
        )


{-| Define a variant with 8 parameters for a custom type.
-}
objectVariant8 :
    String
    -> (a -> b -> c -> d -> e -> f -> g -> h -> v)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> ( String, Codec e )
    -> ( String, Codec f )
    -> ( String, Codec g )
    -> ( String, Codec h )
    -> CustomObjectCodec ((a -> b -> c -> d -> e -> f -> g -> h -> Value) -> k) v
    -> CustomObjectCodec k v
objectVariant8 name ctor ( f1, m1 ) ( f2, m2 ) ( f3, m3 ) ( f4, m4 ) ( f5, m5 ) ( f6, m6 ) ( f7, m7 ) ( f8, m8 ) =
    objectVariant name
        (\c v1 v2 v3 v4 v5 v6 v7 v8 ->
            c
                [ ( f1, encoder m1 v1 )
                , ( f2, encoder m2 v2 )
                , ( f3, encoder m3 v3 )
                , ( f4, encoder m4 v4 )
                , ( f5, encoder m5 v5 )
                , ( f6, encoder m6 v6 )
                , ( f7, encoder m7 v7 )
                , ( f8, encoder m8 v8 )
                ]
        )
        (JD.map8 ctor
            (JD.field f1 <| decoder m1)
            (JD.field f2 <| decoder m2)
            (JD.field f3 <| decoder m3)
            (JD.field f4 <| decoder m4)
            (JD.field f5 <| decoder m5)
            (JD.field f6 <| decoder m6)
            (JD.field f7 <| decoder m7)
            (JD.field f8 <| decoder m8)
        )


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustomObject : CustomObjectCodec (a -> Value) a -> Codec a
buildCustomObject (CustomCodec am) =
    Codec.build am.match
        (JD.field am.tagField JD.string
            |> JD.andThen
                (\tag ->
                    Dict.get tag am.decoder
                        |> Maybe.withDefault
                            (JD.fail <| am.tagField ++ " \"" ++ tag ++ "\" did not match")
                )
        )
