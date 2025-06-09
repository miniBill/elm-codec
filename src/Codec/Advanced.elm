module Codec.Advanced exposing (CustomCodec(..), custom, variant, buildCustom)

{-|

@docs CustomCodec, custom, variant, buildCustom

-}

import Codec exposing (Codec, Decoder, Value)
import Json.Decode as JD



-- CUSTOM


{-| A partially built `Codec` for a custom type.
-}
type CustomCodec match v
    = CustomCodec
        { match : match
        , decoder : List (Decoder v)
        }


{-| Starts building a `Codec` for a custom type.

This version allows you to control how variants are encoded, but will use `oneOf` under the hood, so if two variants have the same shape when encoded, it will always use the first one for decoding.

**In particular, in the example below swapping the `Green` and `Yellow` variants means that `Green` values will be decoded as `Yellow` (because `Codec.succeed` always succeeds).**

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

-}
custom : match -> CustomCodec match value
custom match =
    CustomCodec
        { match = match
        , decoder = []
        }


{-| Define a variant for a custom type.

The first argument is a function from the encoded type to your custom type, the second argument is a codec for the encoded type.

-}
variant :
    (a -> v)
    -> Codec a
    -> CustomCodec ((a -> Value) -> b) v
    -> CustomCodec b v
variant matchPiece decoderPiece (CustomCodec am) =
    CustomCodec
        { match = am.match (Codec.encoder decoderPiece)
        , decoder = JD.map matchPiece (Codec.decoder decoderPiece) :: am.decoder
        }


{-| Build a `Codec` for a fully specified custom type.
-}
buildCustom : CustomCodec (a -> Value) a -> Codec a
buildCustom (CustomCodec am) =
    Codec.build
        am.match
        (JD.oneOf (List.reverse am.decoder))
