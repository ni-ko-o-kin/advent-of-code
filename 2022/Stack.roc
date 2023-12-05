interface Stack
    exposes [
        Stack,
        empty,
        fromList,
        push,
        pop,
        peek,
        isEmpty,
        size
    ]
    imports []

Stack a : [ Stack (List a) ]
StackError : [ StackWasEmpty ]

empty : Stack a
empty =
    Stack []

fromList : List a -> Stack a
fromList = \xs ->
    go : Stack a, a -> Stack a
    go = \acc, cur ->
        push acc cur

    List.walk xs empty go

push : Stack a, a -> Stack a
push = \Stack xs, x ->
    Stack (List.append xs x )

pop : Stack a ->  ({ item: Result a StackError, stack: Stack a })
pop = \Stack xs ->
    item =
        when List.takeLast xs 1 is
            [] -> Err StackWasEmpty
            [x, ..] -> Ok x

    { item: item
    , stack: fromList (List.dropLast xs)
    }

peek : Stack a ->  ({ item: Result a StackError, stack: Stack a })
peek = \stack ->
    { item } = pop stack

    { item: item
    , stack: stack
    }

size : Stack a -> Nat
size = \Stack xs ->
    List.len xs

isEmpty : Stack * -> Bool
isEmpty = \Stack xs ->
    List.isEmpty xs
