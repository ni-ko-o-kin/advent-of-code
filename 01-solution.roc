app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path]
    provides [main] to pf

main =
    solve = \input ->
        toSum = \block ->
            block
                |> Str.split "\n"
                |> List.map (\str -> Result.withDefault (Str.toI32 str) 0)
                |> List.sum

        input
            |> Str.split "\n\n"
            |> List.map toSum
            |> List.sortDesc
            |> List.takeFirst 3
            |> List.sum
            |> Num.toStr

    task =
        input <- await (File.readUtf8 (Path.fromStr "./01-input"))
        Stdout.line (solve input)

    Task.mapFail task (\_ -> crash "oh no")
