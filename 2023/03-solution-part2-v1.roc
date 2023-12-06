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
Field : [Star, Digit Value Width Validity]

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
            else if "*" == char then
                Dict.insert innerAcc currentIndex Star
            else
                innerAcc

solve : Dict Index Field -> Nat
solve = \fields ->
    fields
    |> Dict.walk 0 \acc, starIndex, field ->
        when field is
            Star ->
                # first line
                # ??r.... .??r... ..??r.. ...??r. ....??r
                # ...*... ...*... ...*... ...*... ...*...

                # middle line
                # ??r*... ...*r.. ...*?r. ...*??r

                # last line
                # ...*... ...*... ...*... ...*... ...*...
                # ??r.... .??r... ..??r.. ...??r. ....??r

                cols = List.range { start: At 0, end: Length 5 }

                # TODO only allow ??r in first line with len 3 and ?r only with len 2 aso

                firstLine =
                    List.keepOks cols \i ->
                        r <- Result.try (Num.subChecked starIndex.row 1)
                        c <- Result.try (Num.subChecked (starIndex.col + 3) i)

                        when (Dict.get fields {row: r, col: c}) is
                            Ok (Digit value _ _) -> Ok value
                            _ -> Err NotADigit

                lastLine =
                    List.keepOks cols \i ->
                        c <- Result.try (Num.subChecked (starIndex.col + 3) i)

                        when (Dict.get fields {row: starIndex.row + 1, col: c}) is
                            Ok (Digit value _ _) -> Ok value
                            _ -> Err NotADigit

                middleLine =
                    middleCols = List.keepOks
                        [ Num.subChecked starIndex.col 1
                        , Ok (starIndex.col + 1)
                        , Ok (starIndex.col + 2)
                        , Ok (starIndex.col + 3)
                        ]
                        \x -> x

                    List.keepOks middleCols \middleCol ->
                        when (Dict.get fields {row: starIndex.row + 1, col: middleCol}) is
                            Ok (Digit value _ _) -> Ok value
                            _ -> Err NotADigit

                values = [firstLine, middleLine, lastLine] |> List.join  |> Set.fromList |> Set.toList
                dbg "---"
                dbg values
                dbg (List.len values == 2)
                if List.len values == 2 then
                    value1 = values |> List.first |> Result.withDefault 1
                    value2 = values |> List.last |> Result.withDefault 1
                    acc + (value1 * value2)
                else
                    acc

            _ ->
                acc
