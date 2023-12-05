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
    checkAndAddIndex : Index, [Up, Down, Stay], [Left, Right, Stay] -> Result Index [Overflow]
    checkAndAddIndex = \index, rowDirection, colDirection ->
        if rowDirection == Up && index.row == 0 then
            Err Overflow
        else if colDirection == Left && index.col == 0 then
            Err Overflow
        else
            rowToCheck = when rowDirection is
                Up -> index.row - 1
                Down -> index.row + 1
                Stay -> index.row
            colToCheck = when colDirection is
                Left -> index.col - 1
                Right -> index.col + 1
                Stay -> index.col

            Ok {row: rowToCheck , col: colToCheck}

    fields
    |> Set.map \field ->
        when field is
            Digit digitIndex value _ ->
                [ checkAndAddIndex digitIndex Up Left
                , checkAndAddIndex digitIndex Up Stay
                , checkAndAddIndex digitIndex Up Right
                , checkAndAddIndex digitIndex Stay Left
                , checkAndAddIndex digitIndex Stay Stay
                , checkAndAddIndex digitIndex Stay Right
                , checkAndAddIndex digitIndex Down Left
                , checkAndAddIndex digitIndex Down Stay
                , checkAndAddIndex digitIndex Down Right
                ]
                |> List.keepOks \x -> x
                |> List.any
                    \symbolIndex -> Set.contains fields (Symbol {row: symbolIndex.row, col: symbolIndex.col})
                |> \anySymbolsAdjacent ->
                    if anySymbolsAdjacent then
                        Digit { row: digitIndex.row, col: digitIndex.col } value Valid
                    else
                        Digit { row: digitIndex.row, col: digitIndex.col } value Invalid
            _ ->
                field
    |> \_ -> 1
