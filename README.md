# elm-codecs

[![Build Status](https://travis-ci.org/miniBill/elm-codec.svg?branch=master)](https://travis-ci.org/miniBill/elm-codec)

This package allows you to build pairs of JSON encoders (`a -> Value`) and decoders (`Decoder a`), collectively called a `Codec a`.

It supports all of the basic types, collections, records and even custom types! See an example at the bottom of this document.

## Design Goals

The design goal is to be as type safe as possible while keeping a nice API.
Using this package will greatly reduce the risk of unmatched encoders and decoders.

The packages re-exposes the `Value` and `Decoder` types from `elm/json`, so you don't need to import them too.

## Learning Resources

Ask for help on the [Elm Slack](https://elmlang.herokuapp.com/).

You can also have a look at the `FAQ.md` file.

## Examples
See the `examples` folder for more examples.

### Basic usage ###

```elm
import Codec exposing (Codec, Value)

codec : Codec (List Int)
codec =
    Codec.list Codec.int

encode : List Int -> Value
encode list =
    Codec.encoder codec list

decodeString : String -> Result Codec.Error (List Int)
decodeString s =
    Codec.decodeString codec s
```

### Custom types ###

```elm
type Semaphore
    = Red Int String
    | Yellow Float
    | Green


semaphoreCodec : Codec Semaphore
semaphoreCodec =
    Codec.custom
        (\fred fyellow fgreen value ->
            case value of
                Red i s ->
                    fred i s

                Yellow f ->
                    fyellow f

                Green ->
                    fgreen
        )
        |> Codec.variant2 "Red" Red Codec.int Codec.string
        |> Codec.variant1 "Yellow" Yellow Codec.float
        |> Codec.variant0 "Green" Green
        |> Codec.buildCustom
```