app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        "04-input" as input : Str,
        parser.Core.{ Parser, const, skip, keep, many, sepBy },
        parser.String.{ parseStr, digits, string },
    ]
    provides [main] to pf

main =
    input
    |> parse
    |> solve
    |> Num.toStr
    |> Stdout.line

Card : { winningNumbers: Set Nat, myNumbers: Set Nat }

parse : Str -> List Card
parse = \str ->
    str
    |> Str.split "\n"
    |> List.keepIf \line -> Bool.not (Str.isEmpty line)
    |> List.keepOks parseLine

parseLine : Str -> Result Card [ParsingFailure Str, ParsingIncomplete Str]
parseLine = \line ->
    lineParser : Parser _ Card
    lineParser =
        const \xs -> \ys -> { winningNumbers: Set.fromList xs, myNumbers: Set.fromList ys }
        |> skip (string "Card")
        |> skip (many (string " "))
        |> skip digits
        |> skip (string ":")
        |> skip (many (string " "))
        |> keep (digits |> sepBy (many (string " ")))
        |> skip (many (string " "))
        |> skip (string "|")
        |> skip (many (string " "))
        |> keep (digits |> sepBy (many (string " ")))

    parseStr lineParser line

solve : List Card -> Nat
solve = \cards ->
    initialCounter =
        cards
        |> List.mapWithIndex \_, idx -> (idx, 1)
        |> Dict.fromList

    List.walkWithIndex cards initialCounter \counter, card, currentCardIdx ->
        currentCardCount = Dict.get counter currentCardIdx |> Result.withDefault 0
        distancesToCurrentCard =
            Set.intersection card.winningNumbers card.myNumbers
            |> Set.len
            |> \matches ->  List.range { start: At 1, end: Length matches  }

        List.walk distancesToCurrentCard counter \acc, distance ->
            Dict.update acc (currentCardIdx + distance) \possibleOtherCount ->
                when possibleOtherCount is
                    Missing ->
                        Missing
                    Present otherCount ->
                        Present (otherCount + currentCardCount)

    |> Dict.walk 0 \acc, _, count -> acc + count

example =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    """

expect
    parsed = parse example
    solved = solve parsed
    solved == 30
