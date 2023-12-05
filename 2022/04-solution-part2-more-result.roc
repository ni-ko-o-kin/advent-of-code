app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Bool.{ Bool }]
    provides [main] to pf

Range : { from: U8, to: U8 }
Pair : [Pair Range Range]
ParserError : [InvalidRange, InvalidPair, InvalidNumStr]

main =
    toNoneEmptyLines : Str -> List Str
    toNoneEmptyLines = \str ->
        str
        |> Str.split "\n"
        |> List.map Str.trim
        |> List.dropIf Str.isEmpty

    task =
        input <- "./04-input" |> Path.fromStr |> File.readUtf8 |> await
        pairs <- input |> toNoneEmptyLines |> parse |> Task.fromResult |> await

        solution = solve pairs

        Stdout.line (Num.toStr solution)

    Task.mapFail task (\_ -> crash "oh no")

parse : List Str -> Result (List Pair) ParserError
parse = \lines ->
    parseRange : Str -> Result (List Range) [InvalidRange, InvalidNumStr]
    parseRange = \line ->
        line
            |> Str.split ","
            |> List.map (\x -> Str.split x "-")
            |> List.mapTry \rangeStrings ->
                when rangeStrings is
                    [fromStr, toStr] ->
                        from <- Result.try (Str.toU8 fromStr)
                        to <- Result.try (Str.toU8 toStr)
                        Ok { from: from, to: to }
                    _ ->
                        Err InvalidRange

    parsePair : List Range -> Result Pair [InvalidPair]
    parsePair = \ranges ->
        when ranges is
            [r1, r2] -> Ok (Pair r1 r2)
            _ -> Err InvalidPair

    rangesList <- lines
        |> List.mapTry parseRange
        |> Result.try

    List.mapTry rangesList parsePair


solve : List Pair -> U16
solve = \pairs ->
    isOverlapping : Pair -> Bool
    isOverlapping = \Pair r1 r2 ->
        r1Overlapping = Bool.not (r1.to < r2.from || r1.from > r2.to)
        r2Overlapping = Bool.not (r2.to < r1.from || r2.from > r1.to)
        r1Overlapping && r2Overlapping

    pairs
    |> List.keepIf isOverlapping
    |> List.len
    |> Num.toU16
