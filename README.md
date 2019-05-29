# elm-codecs

[![Build Status](https://travis-ci.org/miniBill/elm-codec.svg?branch=master)](https://travis-ci.org/miniBill/elm-codec)

This package allows you to build pairs of JSON encoders (`a -> Value`) and decoders (`Decoder a`), collectively called a `Codec a`.

See an end-to-end example in the `examples/` folder.

## Design Goals

The design goal is to be as type safe as possible while keeping a nice API.
Using this package will greatly reduce the risk of unmatched encoders and decoders.

## Example

```elm
codec : Codec (List Int)
codec =
    Codec.list Codec.int

encode : List Int -> Json.Encode.Value
encode =
    Codec.encoder codec

decoder : Json.Decoder.Decoder (List Int)
decoder =
    Codec.decoder codec
```

## Learning Resources

Ask for help on the [Elm Slack](https://elmlang.herokuapp.com/).

You can also have a look at the `FAQ.md` file.
