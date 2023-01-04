app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Maybe.{ Maybe }, Bool.{ Bool, true, false }]
    provides [main] to pf

Range : { from: U8, to: U8 }
Pair : [Pair Range Range]

main =
    toNoneEmptyLines : Str -> List Str
    toNoneEmptyLines = \str ->
        str
        |> Str.split "\n"
        |> List.map Str.trim
        |> List.dropIf Str.isEmpty

    task =
        input <- await (File.readUtf8 (Path.fromStr "./04-input"))
        input
        |> toNoneEmptyLines
        |> decode
        |> solve
        |> Num.toStr
        |> Stdout.line

    Task.mapFail task (\_ -> crash "oh no")

decode : List Str -> List Pair
decode = \lines ->
    go : Str -> Maybe Pair
    go = \line ->
        line
        |> Str.split ","
        |> List.map (\x -> Str.split x "-")
        |> List.map \rangeStrings ->
            when rangeStrings is
                [fromStr, toStr] ->
                    Maybe.map2
                        (Str.toU8 fromStr |> Maybe.fromResult)
                        (Str.toU8 toStr |> Maybe.fromResult)
                        (\from, to -> {from: from, to: to})
                _ ->
                    Nothing
        |> Maybe.keepJusts
        |> \ranges ->
            when ranges is
                [r1, r2] -> Just (Pair r1 r2)
                _ -> Nothing

    lines
    |> List.map go
    |> Maybe.keepJusts

solve : List Pair -> U16
solve = \pairs ->
    isFullyContained : Pair -> Bool
    isFullyContained = \Pair r1 r2 ->
        (r1.from >= r2.from && r1.to <= r2.to) || (r2.from >= r1.from && r2.to <= r1.to)

    pairs
    |> List.map isFullyContained
    |> List.keepIf (\x -> x == true)
    |> List.len
    |> Num.toU16
