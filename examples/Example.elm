module Examples exposing (Point, Tree(..), pointCodec, treeCodec)

import Codec exposing (Codec)


type alias Point =
    { x : Int
    , y : Int
    }


pointCodec : Codec Point
pointCodec =
    Codec.object Point
        |> Codec.field "x" .x Codec.int
        |> Codec.field "y" .y Codec.int
        |> Codec.buildObject


type Peano
    = Peano (Maybe Peano)


peanoCodec : Codec Peano
peanoCodec =
    Codec.recursive
        (\finishedCodec ->
            Codec.maybe finishedCodec
                |> Codec.map Peano (\(Peano p) -> p)
        )


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


type Tree a
    = Node (List (Tree a))
    | Leaf a


treeCodec : Codec a -> Codec (Tree a)
treeCodec meta =
    Codec.recursive
        (\rmeta ->
            let
                match fnode fleaf tree =
                    case tree of
                        Node cs ->
                            fnode cs

                        Leaf x ->
                            fleaf x
            in
            Codec.custom match
                |> Codec.variant1 "Node" Node (Codec.list rmeta)
                |> Codec.variant1 "Leaf" Leaf meta
                |> Codec.buildCustom
        )
