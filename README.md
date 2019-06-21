# elm-codecs

[![Build Status](https://travis-ci.org/miniBill/elm-codec.svg?branch=master)](https://travis-ci.org/miniBill/elm-codec)

This package allows you to build pairs of JSON encoders (`a -> Value`) and decoders (`Decoder a`), collectively called a `Codec a`.

## Design Goals

The design goal is to be as type safe as possible while keeping a nice API.
Using this package will greatly reduce the risk of unmatched encoders and decoders.

The packages re-exposes the `Value` and `Decoder` types from `elm/json`, so you don't need to import them too.

## Examples

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

## Learning Resources

Ask for help on the [Elm Slack](https://elmlang.herokuapp.com/).

You can also have a look at the `FAQ.md` file.
