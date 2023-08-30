# elm-codecs

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

### Basic usage

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

### Custom types

```elm
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
```
