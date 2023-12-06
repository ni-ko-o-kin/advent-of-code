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
Width : Nat
Value: Nat
Field : [Symbol, Digit Value Width Validity]

parse : Str -> Dict Index Field
parse = \lines ->
    lines
    |> Str.split "\n"
    |> List.walkWithIndex (Dict.empty {}) \acc, line, rowNumber ->
        line
        |> Str.graphemes
        |> List.walkWithIndex acc \innerAcc, char, colNumber ->
            currentIndex = { row: rowNumber, col: colNumber }

            if Str.contains "1234567890" char then
                when Str.toNat char is
                    Ok digit ->
                        newDigit = (Digit digit 1 NotCheckedYet)

                        if colNumber > 0 then
                            previousIndex = { row: rowNumber, col: colNumber - 1 }
                            previous = Dict.get innerAcc previousIndex

                            when previous is
                                Ok (Digit previousValue previousWidth _) ->
                                    innerAcc
                                    |> Dict.insert currentIndex (Digit (previousValue * 10 + digit) (previousWidth + 1) NotCheckedYet)
                                    |> Dict.remove previousIndex

                                _ ->
                                    Dict.insert innerAcc currentIndex newDigit
                        else
                            Dict.insert innerAcc currentIndex newDigit
                    Err _ ->
                        innerAcc
            else if Str.contains "@+-#!§$%&/()=?*~" char then
                Dict.insert innerAcc currentIndex Symbol
            else
                innerAcc

solve : Dict Index Field -> Nat
solve = \fields ->
    fields
    |> Dict.map \rightDigitIndex, field ->
        when field is
            Digit value width _ ->
                # aaaaa
                # bxxrc valueWidth=3(x+x+r,1+1+1) r=rightDigitIndex
                # ddddd

                cols =
                    { start: At 0, end: Length (width + 2) }
                    |> List.range
                    |> List.keepOks \i -> Num.subChecked (rightDigitIndex.col + 1) i

                a = Result.map (Num.subChecked rightDigitIndex.row 1) \r -> List.map cols \colForA -> {row: r, col: colForA}
                b = Result.map (Num.subChecked rightDigitIndex.col width) \colForB -> List.single {row: rightDigitIndex.row, col: colForB}
                c = Ok (List.single {row: rightDigitIndex.row, col: rightDigitIndex.col + 1})
                d = Ok (List.map cols \colForD -> {row: rightDigitIndex.row + 1, col: colForD})

                indicesAroundValue =
                    [a,b,c,d]
                    |> List.keepOks \x -> x
                    |> List.join

                anySymbolsAdjacent = List.any indicesAroundValue \index ->
                    when Dict.get fields index is
                        Ok Symbol -> Bool.true
                        _ -> Bool.false

                if anySymbolsAdjacent then
                    Digit value width Valid
                else
                    Digit value width Invalid
            _ ->
                field
    |> Dict.walk 0 \acc, _, cur ->
        when cur is
            Digit value _ Valid ->
                acc + value
            _ ->
                acc
