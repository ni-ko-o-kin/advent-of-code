app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [pf.Stdout, pf.Task.{ Task }, "01-input" as input : Str]
    provides [main] to pf

main : Task {} *
main =
    input
    |> solve
    |> Num.toStr
    |> Stdout.line

solve : Str -> Nat
solve = \str ->
    str
    |> Str.split "\n"
    |> List.map \line ->
        line
        |> Str.graphemes
        |> List.keepIf isDigit
        |> \xs ->
            when xs is
                [x] -> [x, x]
                [x, .., y] -> [x, y]
                _ -> []
        |> Str.joinWith ""
    |> List.keepOks Str.toNat
    |> List.sum

isDigit : Str -> Bool
isDigit = \str ->
    when str is
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> Bool.true
        _ -> Bool.false
