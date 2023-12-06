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
        |> List.keepOks Str.toNat
        |> \xs ->
            when xs is
                [x] -> 10 * x + x
                [x, .., y] -> 10 * x + y
                _ -> 0
    |> List.sum
