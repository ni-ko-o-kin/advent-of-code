app "advent-of-code"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "03-input" as input : Str,
    ]
    provides [main] to pf

main =
    input
    |> parse
    |> solve
    |> Num.toStr
    |> Stdout.line

Validity : [Valid, Invalid, NotCheckedYet]
Index : { col: Nat, row: Nat }
Field : [Symbol Index, Digit Index Nat Validity]

parse : Str -> Set Field
parse = \lines ->
    lines
    |> Str.split "\n"
    |> List.walkWithIndex (Set.empty {}) \acc, line, rowNumber ->
        line
        |> Str.graphemes
        |> List.walkWithIndex acc \innerAcc, char, colNumber ->
            if Str.contains "1234567890" char then
                when Str.toNat char is
                    Ok digit ->
                        Set.insert innerAcc (Digit { row: rowNumber, col: colNumber } digit NotCheckedYet)
                    Err _ ->
                        innerAcc
            else if Str.contains "@+-#!§$%&/()=?*~" char then
                Set.insert innerAcc (Symbol { row: rowNumber, col: colNumber })
            else
                innerAcc

solve : Set Field -> Nat
solve = \fields ->
    fields
    |> \_ -> 1
