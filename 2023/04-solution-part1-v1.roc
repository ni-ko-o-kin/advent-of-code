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

Game : { id: Nat, winningNumbers: Set Nat, myNumbers: Set Nat }
Games : List Game

parse : Str -> Games
parse = \str ->
    str
    |> Str.split "\n"
    |> List.keepIf \line -> Bool.not (Str.isEmpty line)
    |> List.keepOks parseLine

parseLine : Str -> Result Game [ParsingFailure Str, ParsingIncomplete Str]
parseLine = \line ->
    lineParser : Parser _ Game
    lineParser =
        const \id -> \xs -> \ys -> { id: id, winningNumbers: Set.fromList xs, myNumbers: Set.fromList ys }
        |> skip (string "Card")
        |> skip (many (string " "))
        |> keep digits
        |> skip (string ":")
        |> skip (many (string " "))
        |> keep (digits |> sepBy (many (string " ")))
        |> skip (many (string " "))
        |> skip (string "|")
        |> skip (many (string " "))
        |> keep (digits |> sepBy (many (string " ")))

    parseStr lineParser line

solve : Games -> Nat
solve = \games ->
    games
    |> List.map \game -> Set.len (Set.intersection game.winningNumbers game.myNumbers)
    |> List.map \matches -> if matches == 0 then 0 else Num.powInt 2 (matches - 1)
    |> List.sum

expect
    game = { id: 123, winningNumbers: Set.fromList [1,2,3], myNumbers: Set.fromList [1,2,3] }
    parsed = parse "Card 123: 1 2 3 | 1 2 3"
    parsed == [game]

expect
    game = { id: 2, winningNumbers: Set.fromList [2,3], myNumbers: Set.fromList [1,2,3,4] }
    parsed = parse "Card 2: 2 3 | 1 2 3 4"
    parsed == [game]

expect
    game = { id: 2, winningNumbers: Set.fromList [22,3], myNumbers: Set.fromList [1,23] }
    parsed = parse "Card 2: 22 3 | 1 23"
    parsed == [game]

expect
    game = { id: 123, winningNumbers: Set.fromList [1,2,3], myNumbers: Set.fromList [1,2,3] }
    solved = solve [game]
    solved == 4

expect
    game = { id: 2, winningNumbers: Set.fromList [2,3], myNumbers: Set.fromList [1,2,3,4] }
    solved = solve [game]
    solved == 2

expect
    game = { id: 2, winningNumbers: Set.fromList [21,3], myNumbers: Set.fromList [1,21,4] }
    solved = solve [game]
    solved == 1

expect
    parsed = parse "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    solved = solve parsed
    solved == 2

examplePlusOneCard =
    """
    Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
    Card 7:  1  8  3 56 19 |  1 77 10 23 35 67 36 11
    """

expect
    parsed = parse examplePlusOneCard
    solved = solve parsed
    solved == 14
