## How do you use `recursive`?
The trick to understanding the `recursive` codec is: pretend you are already done.
When the function you pass to `recursive` is called the argument is the finished `Codec`.

An example may be worth a thousand words:

```elm
type Peano
    = Peano (Maybe Peano)


peanoCodec : Codec Peano
peanoCodec =
    Codec.recursive
        (\finishedCodec ->
            Codec.maybe finishedCodec
                |> Codec.map Peano (\(Peano p) -> p)
        )
```

## Why does `map` take two opposite functions?
One is used for the encoder, the other for the decoder

## How do I build `Codec`s for custom types?
You start building with `custom` which needs the pattern matcher for your type as an argument.

The pattern matcher is just the most generic `case ... of` possible for your type.

You then chain `variantX` calls for every alternative (in the same order as the pattern matcher).

You end with a call to `buildCustom`.

An example:

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

## What happens to existing `Value`s if I change my `Codec`s?
Old `Value`s will be parsed fine by new `Codec`s if you:
* add new `variant`s to custom types,
* remove (from the end) parameters from `variant`s,
* change any `Codec` to a `succeed` one or
* add optional fields (`maybeField`) to records.

New `Value`s will be parsed fine by old `Codec`s if you:
* remove `variant`s from custom types,
* append parameters to `variant`s ,
* change a `succeed` `Codec` to any other one or
* remove fields from records.
