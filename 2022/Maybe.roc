interface Maybe
    exposes [
        Maybe,
        map,
        map2,
        andThen,
        fromResult,
        keepJusts,
        filterMap,
        withDefault
    ]
    imports []

Maybe a : [Just a, Nothing]

map : Maybe a, (a -> b) -> Maybe b
map = \maybe, fn ->
    when maybe is
        Just x ->
            Just (fn x)
        Nothing ->
            Nothing

map2 : Maybe a, Maybe b, (a, b -> c) -> Maybe c
map2 = \maybeA, maybeB, fn ->
    when maybeA is
        Just x ->
            when maybeB is
                Just y ->
                    Just (fn x y)
                Nothing ->
                    Nothing
        Nothing ->
            Nothing

andThen : Maybe a, (a -> Maybe b) -> Maybe b
andThen = \maybe, fn ->
    when maybe is
        Just x ->
            fn x
        Nothing ->
            Nothing

fromResult : Result ok err -> Maybe ok
fromResult = \result ->
    when result is
        Ok ok ->
            Just ok
        Err _ ->
            Nothing

keepJusts : List (Maybe a) -> List a
keepJusts = \xs ->
    go = \acc, cur ->
        when cur is
            Just x ->
                List.append acc x
            Nothing ->
                acc
    List.walk xs [] go

withDefault : Maybe a, a -> a
withDefault = \maybe, defaultValue ->
    when maybe is
        Just x ->
            x
        Nothing ->
            defaultValue

filterMap : List a, (a -> Maybe b) -> List b
filterMap = \xs, fn ->
    xs
    |> List.map fn
    |> keepJusts
