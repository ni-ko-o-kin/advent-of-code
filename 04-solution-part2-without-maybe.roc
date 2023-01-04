app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Bool.{ Bool }, Result]
    provides [main] to pf

Range : [Range { from: U8, to: U8 }, InvalidRange]
Pair : [Pair Range Range, InvalidPair]

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
    go : Str -> Pair
    go = \line ->
        line
        |> Str.split ","
        |> List.map (\x -> Str.split x "-")
        |> List.map \rangeStrings ->
            when rangeStrings is
                [fromStr, toStr] ->
                    when [Str.toU8 fromStr, Str.toU8 toStr] is
                        [Ok from, Ok to] ->
                            Range {from: from, to: to}
                        _ ->
                            InvalidRange
                _ ->
                    InvalidRange
        |> \ranges ->
            when ranges is
                [r1, r2] -> Pair r1 r2
                _ -> InvalidPair

    lines
    |> List.map go

solve : List Pair -> U16
solve = \pairs ->
    isOverlapping : Pair -> Bool
    isOverlapping = \pair ->
        when pair is
            Pair (Range r1) (Range r2) ->
                Bool.not ((r1.to < r2.from || r1.from > r2.to) || (r2.to < r1.from || r2.from > r1.to))

            Pair InvalidRange _ ->
                Bool.false

            Pair _ InvalidRange ->
                Bool.false

            InvalidPair ->
                Bool.false

    pairs
    |> List.keepIf isOverlapping
    |> List.len
    |> Num.toU16
