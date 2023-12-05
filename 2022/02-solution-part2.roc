app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Maybe.{Maybe}]
    provides [main] to pf

# A 1 for rock
# B 2 for paper
# C 3 for scissors

# X 0 lose
# Y 3 draw
# Z 6 win

Points : I16
Sign : [Rock, Paper, Scissors]
Outcome : [Lose, Draw, Win]
Instruction : [Instruction Sign Outcome]

main =
    task =
        input <- await (File.readUtf8 (Path.fromStr "./02-input"))
        input
            |> solve
            |> Num.toStr
            |> Stdout.line

    Task.mapFail task (\_ -> crash "oh no")

decode : Str -> Maybe Instruction
decode = \str ->
    toSign : Str -> Maybe Sign
    toSign = \char ->
        when char is
            "A" -> Just Rock
            "B" -> Just Paper
            "C" -> Just Scissors
            _ -> Nothing

    toOutcome : Str -> Maybe Outcome
    toOutcome = \char ->
        when char is
            "X" -> Just Lose
            "Y" -> Just Draw
            "Z" -> Just Win
            _ -> Nothing

    splitted = str |> Str.split " "

    Maybe.map2
        (splitted  |> List.first |> Maybe.fromResult |> Maybe.andThen toSign)
        (splitted |> List.last  |> Maybe.fromResult |> Maybe.andThen toOutcome)
        Instruction

calcScore : Instruction -> Points
calcScore = \Instruction sign outcome ->
    signToAchiveOutcome : Sign
    signToAchiveOutcome =
        when outcome is
            Draw -> sign
            Lose ->
                when sign is
                    Rock -> Scissors
                    Paper -> Rock
                    Scissors -> Paper
            Win ->
                when sign is
                    Rock -> Paper
                    Paper -> Scissors
                    Scissors -> Rock

    signPoints : Points
    signPoints =
        when signToAchiveOutcome is
            Rock -> 1
            Paper -> 2
            Scissors -> 3

    outcomePoints : Points
    outcomePoints =
        when outcome is
            Lose -> 0
            Draw -> 3
            Win -> 6

    signPoints + outcomePoints

solve : Str -> Points
solve = \input ->
    input
        |> Str.split "\n"
        |> List.map Str.trim
        |> List.dropIf Str.isEmpty
        |> List.map decode
        |> Maybe.keepJusts
        |> List.map calcScore
        |> List.sum
