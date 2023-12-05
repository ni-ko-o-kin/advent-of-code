app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path]
    provides [main] to pf

# A X 1 for rock
# B Y 2 for paper
# C Z 3 for scissors

# 0 for lose
# 3 for draw
# 6 for win

Points : I16
Sign : [Rock, Paper, Scissors]
Signs : { firstSign: Sign, secondSign: Sign }

main =
    task =
        input <- await (File.readUtf8 (Path.fromStr "./02-input"))
        Stdout.line (solve input)

    Task.mapFail task (\_ -> crash "oh no")

solve = \input ->
    decode : Str -> Signs
    decode = \str ->
        toSign : Str -> Sign
        toSign = \char ->
            when char is
                "A" | "X" -> Rock
                "B" | "Y" -> Paper
                "C" | "Z" -> Scissors
                _ -> Rock
            signs = str |> Str.split " "
        { firstSign: signs  |> List.first |> Result.withDefault "A" |> toSign
        , secondSign: signs |> List.last  |> Result.withDefault "X" |> toSign
        }

    # signToStr : Sign -> Str
    # signToStr = \sign ->
    #     when sign is
    #         Rock -> "Rock"
    #         Paper -> "Paper"
    #         Scissors -> "Scissors"

    toScore : Signs -> Points
    toScore = \{firstSign, secondSign} ->
        # dbg "---"
        # dbg (signToStr firstSign)
        # dbg (signToStr secondSign)
        signPoints : Points
        signPoints =
            when secondSign is
                Rock -> 1
                Paper -> 2
                Scissors -> 3

        outcomePoints : Points
        outcomePoints =
            when [firstSign, secondSign] is
                [Rock, Rock] | [Paper, Paper] | [Scissors, Scissors] ->
                    3

                [Rock, Paper] | [Paper, Scissors] | [Scissors, Rock] ->
                    6

                [Paper, Rock] | [Rock, Scissors] | [Scissors, Paper] ->
                    0

                _ ->
                    0

        signPoints + outcomePoints

    input
        |> Str.split "\n"
        |> List.map Str.trim
        |> List.dropIf Str.isEmpty
        |> List.map decode
        |> List.map toScore
        |> List.sum
        |> Num.toStr


expect solve "A X" == "4"
expect solve "A Y" == "8"
expect solve "A Z" == "3"
