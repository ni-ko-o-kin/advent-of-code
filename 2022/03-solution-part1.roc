app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Maybe.{ Maybe }]
    provides [main] to pf

# a-z 1-26
# A-Z 27-52

Priority : U16
Compartments : { first: List Str, second: List Str }

main =
    task =
        input <- await (File.readUtf8 (Path.fromStr "./03-input"))
        input
            |> solve
            |> Num.toStr
            |> Stdout.line

    Task.mapFail task (\_ -> crash "oh no")


solve : Str -> Priority
solve = \input ->
    extractCompartments : Str -> Compartments
    extractCompartments = \str ->
        str
        |> Str.graphemes
        |> \xs -> List.split xs (List.len xs // 2)
        |> \{before, others} -> { first: before, second: others }

    getSameChar : Compartments -> Maybe Str
    getSameChar = \{first, second}->
        Set.intersection
            (Set.fromList first)
            (Set.fromList second)
        |> Set.toList
        |> List.first
        |> Maybe.fromResult

    calcPriority : Str -> Maybe Priority
    calcPriority = \str ->
        Str.toUtf8 str
        |> List.first
        |> Maybe.fromResult
        |> Maybe.map Num.toU16
        |> Maybe.map (\x ->
            if x >= 97 && x <= 122 then
                x - 96
            else
                x - 38
        )

    input
         |> Str.split "\n"
         |> List.map Str.trim
         |> List.dropIf Str.isEmpty
         |> List.map extractCompartments
         |> List.map getSameChar
         |> Maybe.keepJusts
         |> List.map calcPriority
         |> Maybe.keepJusts
         |> List.sum
