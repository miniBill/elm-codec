module Codec.SumType exposing
    ( sumType, buildSumType
    , variant0, variant1, variant2, variant3, variantData
    , SumTypeCodec
    )

{-| A Codec designed to parse and encode a JSON object representing a sum type


# Make a codec

@docs sumType, buildSumType


# Variants

@docs variant0, variant1, variant2, variant3, variantData

-}

import Codec exposing (Codec)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE


type SumTypeCodec match value
    = SumTypeCodec
        { match : match
        , decoders : Dict String (Decoder value)
        }


{-| Starts building a `Codec` for a sum type.

You need to pass a pattern matching function, built like this:

    type Payment
        = Cash
        | CreditCard String
        | Paypal String String

    paypmentCodec : Codec Payment
    paypmentCodec =
        sumType
            (\cash creditCard paypal value ->
                case value of
                    Cash ->
                        cash

                    CreditCard number ->
                        creditCard number

                    Paypal email telephone ->
                        paypal email telephone
            )
            |> variant0 "cash" Cash
            |> variant1 "card" CreditCard ( "number", Codec.string )
            |> variant2 "paypal" Paypal ( "email", Codec.string ) ( "tel", Codec.string )
            |> buildSumType

-}
sumType : match -> SumTypeCodec match value
sumType match =
    SumTypeCodec
        { match = match
        , decoders = Dict.empty
        }


mkTag : String -> ( String, JE.Value )
mkTag s =
    ( "tag", JE.string s )


variant :
    String
    -> a
    -> JD.Decoder v
    -> SumTypeCodec (a -> b) v
    -> SumTypeCodec b v
variant tag enc dec (SumTypeCodec st) =
    SumTypeCodec
        { match = st.match enc
        , decoders = Dict.insert tag dec st.decoders
        }


{-| Define a variant with 0 parameters for a sum type.

The first argument is the "tag" used to pick choose this type.

    type Payment
        = Cash

    codec : Codec Payment
    codec =
        sumType
            (\cash value ->
                case value of
                    Cash ->
                        cash
            )
            |> variant0 "cash" Cash
            |> buildSumType

Then `Cash` variant will be encoded to a JSON object like:

    { "tag": "cash" }

As you may notice, "tag" is the same as the first argument used to define the codec.

**NOTE**: This type has only one variant to make the example simpler, but if you have a case like that, it's better to use `Codec.object`. As the name suggests `sumType` is designed to be used when you have more than one variant.

-}
variant0 :
    String
    -> v
    -> SumTypeCodec (List ( String, JE.Value ) -> b) v
    -> SumTypeCodec b v
variant0 tag val =
    variant tag
        [ mkTag tag ]
        (JD.succeed val)


{-| Define a variant with 1 parameter for a sum type.

The first argument is the "tag" used to pick this type.
The second argument is a tuple representing the name of the expected property in the JSON object and its type.

    type Payment
        = CreditCard String

    codec : Codec Payment
    codec =
        sumType
            (\creditCard value ->
                case value of
                    CreditCard number ->
                        creditCard number
            )
            |> variant1 "card" CreditCard ( "number", Codec.string )
            |> buildSumType

Then `CreditCard "1234"` variant will be encoded to a JSON object like:

    { "tag": "card"
    , "number": "1234"
    }

**NOTE**: This type has only one variant to make the example simpler, but if you have a case like that, it's better to use `Codec.object`. As the name suggests `sumType` is designed to be used when you have more than one variant.

-}
variant1 :
    String
    -> (a -> v)
    -> ( String, Codec a )
    -> SumTypeCodec ((a -> List ( String, JE.Value )) -> b) v
    -> SumTypeCodec b v
variant1 tag make ( field, codec ) =
    variant tag
        (\a -> [ mkTag tag, ( field, Codec.encoder codec a ) ])
        (JD.map make <| JD.field field <| Codec.decoder codec)


{-| Define a variant with 2 parameters for a sum type.

The first argument is the "tag" used to pick this type.
The subsequent arguments are tuples representing the name of the expected property in the JSON object and its type.

    type Payment
        = Paypal Email Telephone

    codec : Codec Payment
    codec =
        sumType
            (\paypal value ->
                case value of
                    Paypal email telephone ->
                        paypal email telephone
            )
            |> variant2 "paypal" Paypal ( "email", emailCodec ) ( "tel", telCodec )
            |> buildSumType

Then `Paypal "my@email" "1234"` variant will be encoded to a JSON object like:

    { "tag": "card"
    , "email": "my@email"
    , "tel": "1234"
    }

**NOTE**: This type has only one variant to make the example simpler, but if you have a case like that, it's better to use `Codec.object`. As the name suggests `sumType` is designed to be used when you have more than one variant.

-}
variant2 :
    String
    -> (a1 -> a2 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> SumTypeCodec ((a1 -> a2 -> List ( String, JE.Value )) -> b) v
    -> SumTypeCodec b v
variant2 tag make ( f1, fc1 ) ( f2, fc2 ) =
    variant tag
        (\a1 a2 ->
            [ mkTag tag
            , ( f1, Codec.encoder fc1 a1 )
            , ( f2, Codec.encoder fc2 a2 )
            ]
        )
        (JD.map2 make
            (JD.field f1 <| Codec.decoder fc1)
            (JD.field f2 <| Codec.decoder fc2)
        )


{-| Define a variant with 3 parameters for a sum type.

Any variant with more than 3 paramters is discouraged, please check [variantData](#variantData) on how to handle those cases.

The first argument is the "tag" used to pick this type.
The subsequent arguments are tuples representing the name of the expected property in the JSON object and its type.

    type Value
        = Date Year Month Day

    codec : Codec Date
    codec =
        sumType
            (\date value ->
                case value of
                    Date year month day ->
                        date year month day
            )
            |> variant3 "date"
                Date
                ( "year", yearCodec )
                ( "month", monthCodec )
                ( "day", dayCodec )
            |> buildSumType

Then `Date 2020 1 1` variant will be encoded to a JSON object like:

    { "tag": "date"
    , "year": 2020
    , "month": 1
    , "day": 1
    }

**NOTE**: This type has only one variant to make the example simpler, but if you have a case like that, it's better to use `Codec.object`. As the name suggests `sumType` is designed to be used when you have more than one variant.

-}
variant3 :
    String
    -> (a1 -> a2 -> a3 -> v)
    -> ( String, Codec a1 )
    -> ( String, Codec a2 )
    -> ( String, Codec a3 )
    -> SumTypeCodec ((a1 -> a2 -> a3 -> List ( String, JE.Value )) -> b) v
    -> SumTypeCodec b v
variant3 tag make ( f1, fc1 ) ( f2, fc2 ) ( f3, fc3 ) =
    variant tag
        (\a1 a2 a3 ->
            [ mkTag tag
            , ( f1, Codec.encoder fc1 a1 )
            , ( f2, Codec.encoder fc2 a2 )
            , ( f3, Codec.encoder fc3 a3 )
            ]
        )
        (JD.map3 make
            (JD.field f1 <| Codec.decoder fc1)
            (JD.field f2 <| Codec.decoder fc2)
            (JD.field f3 <| Codec.decoder fc3)
        )


{-| Define a variant with an object as parameter.

This is the preferred way of encoding variants that requires a lot of parameters.

    type alias YMD =
        { year : Int
        , month : Int
        , day : Int
        }

    type Value
        = Date YMD

    -- dateCodec : Codec YMD
    codec : Codec Value
    codec =
        sumType
            (\date value ->
                case value of
                    Date ymd ->
                        date ymd
            )
            |> variantData "date" Date dateCodec
            |> buildSumType

Then `Date { year = 2020, month = 1, day = 1 }` variant will be encoded to a JSON object like:

       { "tag": "date"
       , "data":
            { "year": 2020
            , "month": 1
            , "day": 1
            }
       }

In this way is possible to use the full flexibility offered by `Codec.object`

**NOTE**: This type has only one variant to make the example simpler, but if you have a case like that, it's better to use `Codec.object`. As the name suggests `sumType` is designed to be used when you have more than one variant.

-}
variantData :
    String
    -> (a -> v)
    -> Codec a
    -> SumTypeCodec ((a -> List ( String, JE.Value )) -> b) v
    -> SumTypeCodec b v
variantData tag make codec =
    variant1 tag make ( "data", codec )


buildSumType : SumTypeCodec (a -> List ( String, JE.Value )) a -> Codec a
buildSumType (SumTypeCodec st) =
    Codec.build
        (\val -> JE.object <| st.match val)
        (JD.field "tag" JD.string
            |> JD.andThen
                (\tag ->
                    case Dict.get tag st.decoders of
                        Nothing ->
                            JD.fail <| "tag " ++ tag ++ "did not match"

                        Just dec ->
                            dec
                )
        )
