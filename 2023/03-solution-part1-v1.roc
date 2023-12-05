app "advent-of-code"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" }
    imports [
        pf.Stdout,
        "03-input" as input : Str,
    ]
    provides [main] to pf

# nice and readable solution but inefficient

main =
    input
    |> parse
    |> solve
    |> Num.toStr
    |> Stdout.line

Validity : [Valid, Invalid, NotCheckedYet]
Index : { col: Nat, row: Nat }
Field : [Symbol Index, Digit (List Index) Nat Validity]

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
                        newDigit = (Digit [currentIndex] digit NotCheckedYet)

                        if colNumber > 0 then
                            previousIndex = { row: rowNumber, col: colNumber - 1 }
                            previous = Dict.get innerAcc previousIndex

                            when previous is
                                Ok (Digit previousIndices previousValue _) ->
                                    innerAcc
                                    |> Dict.insert currentIndex (Digit (List.append previousIndices currentIndex) (previousValue * 10 + digit) NotCheckedYet)
                                    |> Dict.remove previousIndex

                                _ ->
                                    Dict.insert innerAcc currentIndex newDigit
                        else
                            Dict.insert innerAcc currentIndex newDigit
                    Err _ ->
                        innerAcc
            else if Str.contains "@+-#!§$%&/()=?*~" char then
                Dict.insert innerAcc currentIndex (Symbol currentIndex)
            else
                innerAcc

solve : Dict Index Field -> Nat
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
    |> Dict.map \_, field ->
        when field is
            Digit digitIndices value _ ->
                go = \digitIndex ->
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
                    |> List.any \possibleSymbolIndex ->
                        possibleSymbolField = Dict.get fields possibleSymbolIndex
                        when possibleSymbolField is
                            Ok (Symbol _) -> Bool.true
                            _ -> Bool.false


                anySymbolsAdjacent = List.any digitIndices go

                if anySymbolsAdjacent then
                    Digit digitIndices value Valid
                else
                    Digit digitIndices value Invalid
            _ ->
                field
    |> Dict.walk 0 \acc, _, cur ->
        when cur is
            Digit _ value Valid ->
                acc + value
            _ ->
                acc
