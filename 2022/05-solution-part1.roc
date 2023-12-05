app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Stack.{ Stack }]
    provides [main] to pf

StackIndex : U8
Instruction : { origin: StackIndex, destination: StackIndex }

Crate : Str
CratesStack : Dict StackIndex (Stack Crate)

Problem : { instructions: List Instruction, stack: CratesStack }
ParserError : [ InvalidInput, InvalidStack, InvalidCrate, InvalidInstructions ]

pdbg = \x, str ->
    dbg str
    dbg x
    x

main =
    task =
        input <- "./05-input" |> Path.fromStr |> File.readUtf8 |> await
        problem <- input |> parse |> Task.fromResult |> await

        Stdout.line (Num.toStr (solve problem))

    Task.mapFail task \err ->
        when err is
            FileReadErr _ _ -> crash "FileReadErr"
            InvalidInput -> crash "InvalidInput"
            InvalidCrate -> crash "InvalidCrate"
            InvalidStack -> crash "InvalidStack"
            InvalidInstructions -> crash "InvalidInstructions"
            InvalidNumStr -> crash "InvalidNumStr"
            _ -> crash "oh no"

parse : Str -> Result Problem ParserError
parse = \input ->
    toNoneEmptyLines : List Str -> List Str
    toNoneEmptyLines = \lines ->
        lines
        |> List.map Str.trim
        |> List.dropIf Str.isEmpty

    parseStack : List Str -> Result CratesStack [InvalidStack, InvalidNumStr]
    parseStack = \lines ->
        cratesStack <- lines
            |> List.takeLast 1
            |> \indicesStr ->
                when indicesStr is
                    [x] -> Ok (x |> Str.split " " |> toNoneEmptyLines)
                    _ -> Err InvalidStack
            |> Result.try (\xs -> List.mapTry xs Str.toU8)
            |> Result.map \stackIndices ->
                List.walk stackIndices Dict.empty \acc, stackIndex ->
                    Dict.insert acc Stack.empty stackIndex
            |> Result.try

        dbg cratesStack

        Ok (Dict.empty)

        # cratesLines = lines
        #     |> List.dropLast
        #     |> List.map Str.graphemes

        # cratesLines
        #     |> List.walk cratesStack \cStack, crateLine ->
        #         cratesStack |> Dict.walk cStack \acc, key, value ->
        #             when List.get crateLine (key * 4 + 1) is
        #                 Ok crate ->
        #                     Dict.insert acc key (Stack.push value crate)
        #                 Err _ ->
        #                     acc
        #     |> Ok

    parseInstructions : List Str -> Result (List Instruction) [InvalidInstructions]
    parseInstructions = \_ -> Ok []

    input
        |> Str.split "\n"
        |> List.splitFirst ""
        |> Result.try \{before, after} ->
            instructions <- parseInstructions (toNoneEmptyLines after) |> Result.try
            stack <- parseStack (toNoneEmptyLines before) |> Result.try

            Ok { instructions: instructions
               , stack: stack
               }

    # s : CratesStack
    # s = Dict.empty

    # # Ok { instructions: [], stack: s }
    # Err InvalidInput


solve : Problem -> U16
solve = \_ ->
    []
    |> List.len
    |> Num.toU16
