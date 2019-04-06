module Task.Extra exposing
    ( map6
    , map7
    , map8
    , map9
    , map10
    , map11
    , map12
    , map13
    , map14
    )

{-|


# Task.Extra

@docs map6
@docs map7
@docs map8
@docs map9
@docs map10
@docs map11
@docs map12
@docs map13
@docs map14

-}

import Task exposing (..)


{-| Put the results of six tasks together.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x result
map6 func taskA taskB taskC taskD taskE taskF =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen (\f -> succeed (func a b c d e f))
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of seven tasks together.
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x result
map7 func taskA taskB taskC taskD taskE taskF taskG =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        succeed (func a b c d e f g)
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of eight tasks together.
-}
map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x result
map8 func taskA taskB taskC taskD taskE taskF taskG taskH =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    succeed (func a b c d e f g h)
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of nine tasks together.
-}
map9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x i
    -> Task x result
map9 func taskA taskB taskC taskD taskE taskF taskG taskH taskI =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    taskI
                                                                                                        |> andThen
                                                                                                            (\i ->
                                                                                                                succeed (func a b c d e f g h i)
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of 10 tasks together.
-}
map10 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x i
    -> Task x j
    -> Task x result
map10 func taskA taskB taskC taskD taskE taskF taskG taskH taskI taskJ =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    taskI
                                                                                                        |> andThen
                                                                                                            (\i ->
                                                                                                                taskJ
                                                                                                                    |> andThen
                                                                                                                        (\j ->
                                                                                                                            succeed (func a b c d e f g h i j)
                                                                                                                        )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of 11 tasks together.
-}
map11 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x i
    -> Task x j
    -> Task x k
    -> Task x result
map11 func taskA taskB taskC taskD taskE taskF taskG taskH taskI taskJ taskK =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    taskI
                                                                                                        |> andThen
                                                                                                            (\i ->
                                                                                                                taskJ
                                                                                                                    |> andThen
                                                                                                                        (\j ->
                                                                                                                            taskK
                                                                                                                                |> andThen
                                                                                                                                    (\k ->
                                                                                                                                        succeed (func a b c d e f g h i j k)
                                                                                                                                    )
                                                                                                                        )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of 12 tasks together.
-}
map12 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x i
    -> Task x j
    -> Task x k
    -> Task x l
    -> Task x result
map12 func taskA taskB taskC taskD taskE taskF taskG taskH taskI taskJ taskK taskL =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    taskI
                                                                                                        |> andThen
                                                                                                            (\i ->
                                                                                                                taskJ
                                                                                                                    |> andThen
                                                                                                                        (\j ->
                                                                                                                            taskK
                                                                                                                                |> andThen
                                                                                                                                    (\k ->
                                                                                                                                        taskL
                                                                                                                                            |> andThen
                                                                                                                                                (\l ->
                                                                                                                                                    succeed (func a b c d e f g h i j k l)
                                                                                                                                                )
                                                                                                                                    )
                                                                                                                        )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of 13 tasks together.
-}
map13 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x i
    -> Task x j
    -> Task x k
    -> Task x l
    -> Task x m
    -> Task x result
map13 func taskA taskB taskC taskD taskE taskF taskG taskH taskI taskJ taskK taskL taskM =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    taskI
                                                                                                        |> andThen
                                                                                                            (\i ->
                                                                                                                taskJ
                                                                                                                    |> andThen
                                                                                                                        (\j ->
                                                                                                                            taskK
                                                                                                                                |> andThen
                                                                                                                                    (\k ->
                                                                                                                                        taskL
                                                                                                                                            |> andThen
                                                                                                                                                (\l ->
                                                                                                                                                    taskM
                                                                                                                                                        |> andThen
                                                                                                                                                            (\m ->
                                                                                                                                                                succeed (func a b c d e f g h i j k l m)
                                                                                                                                                            )
                                                                                                                                                )
                                                                                                                                    )
                                                                                                                        )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )


{-| Put the results of 14 tasks together.
-}
map14 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> result)
    -> Task x a
    -> Task x b
    -> Task x c
    -> Task x d
    -> Task x e
    -> Task x f
    -> Task x g
    -> Task x h
    -> Task x i
    -> Task x j
    -> Task x k
    -> Task x l
    -> Task x m
    -> Task x n
    -> Task x result
map14 func taskA taskB taskC taskD taskE taskF taskG taskH taskI taskJ taskK taskL taskM taskN =
    taskA
        |> andThen
            (\a ->
                taskB
                    |> andThen
                        (\b ->
                            taskC
                                |> andThen
                                    (\c ->
                                        taskD
                                            |> andThen
                                                (\d ->
                                                    taskE
                                                        |> andThen
                                                            (\e ->
                                                                taskF
                                                                    |> andThen
                                                                        (\f ->
                                                                            taskG
                                                                                |> andThen
                                                                                    (\g ->
                                                                                        taskH
                                                                                            |> andThen
                                                                                                (\h ->
                                                                                                    taskI
                                                                                                        |> andThen
                                                                                                            (\i ->
                                                                                                                taskJ
                                                                                                                    |> andThen
                                                                                                                        (\j ->
                                                                                                                            taskK
                                                                                                                                |> andThen
                                                                                                                                    (\k ->
                                                                                                                                        taskL
                                                                                                                                            |> andThen
                                                                                                                                                (\l ->
                                                                                                                                                    taskM
                                                                                                                                                        |> andThen
                                                                                                                                                            (\m ->
                                                                                                                                                                taskN
                                                                                                                                                                    |> andThen
                                                                                                                                                                        (\n ->
                                                                                                                                                                            succeed (func a b c d e f g h i j k l m n)
                                                                                                                                                                        )
                                                                                                                                                            )
                                                                                                                                                )
                                                                                                                                    )
                                                                                                                        )
                                                                                                            )
                                                                                                )
                                                                                    )
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )
