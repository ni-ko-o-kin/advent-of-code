app "advent-of-code"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.3.0/-e3ebWWmlFPfe9fYrr2z1urfslzygbtQQsl69iH1qzQ.tar.br",
    }
    imports [
        pf.Stdout,
        "05-input" as input : Str,
        parser.Core.{ Parser, const, skip, keep, many, sepBy  },
        parser.String.{ parseStr, digits, string },
    ]
    provides [main] to pf

main =
    input
    |> parse
    |> solve
    |> Num.toStr
    |> Stdout.line

Game : { seeds: List Nat , mapping: List (List Entry)}
Range : { start: Nat, end: Nat }
Entry : { src: Range, dest: Range }

parse : Str -> Game
parse = \str ->
    parser : Parser _ Game
    parser =
        parseCategory = \mapStr  ->
            \pipeline ->
                pipeline
                |> skip (many (string "\n"))
                |> skip (string (Str.concat mapStr " map:\n"))
                |> keep
                    (many
                        ((const \dest -> \src -> \range -> {dest: {start: dest, end: dest + range}, src: {start: src, end: src + range}})
                        |> keep digits
                        |> skip (string " ")
                        |> keep digits
                        |> skip (string " ")
                        |> keep digits
                        |> skip (string "\n")))

        const \seeds -> \m1 -> \m2 -> \m3 -> \m4 -> \m5 -> \m6 -> \m7 ->
            { seeds, mapping: [m1, m2, m3, m4, m5, m6, m7] }

        |> skip (string "seeds: ")
        |> keep (digits |> sepBy (string " "))

        |> (parseCategory "seed-to-soil" )
        |> (parseCategory "soil-to-fertilizer" )
        |> (parseCategory "fertilizer-to-water" )
        |> (parseCategory "water-to-light" )
        |> (parseCategory "light-to-temperature" )
        |> (parseCategory "temperature-to-humidity" )
        |> (parseCategory "humidity-to-location" )

    parseStr parser str
    |> Result.withDefault {seeds: [], mapping: []}

solve : Game -> Nat
solve = \{seeds, mapping} ->
    go = \number, entries ->
        entries
        |> List.findFirst \{src} -> number >= src.start && number < src.end
        |> Result.map \{src, dest} -> dest.start + (number - src.start)
        |> Result.withDefault number

    seeds
    |> List.map \seed -> List.walk mapping seed go
    |> List.sortAsc
    |> List.first
    |> Result.withDefault 0

expect
    example =
        """
        seeds: 79 14 55 13

        seed-to-soil map:
        50 98 2
        52 50 48

        soil-to-fertilizer map:
        0 15 37
        37 52 2
        39 0 15

        fertilizer-to-water map:
        49 53 8
        0 11 42
        42 0 7
        57 7 4

        water-to-light map:
        88 18 7
        18 25 70

        light-to-temperature map:
        45 77 23
        81 45 19
        68 64 13

        temperature-to-humidity map:
        0 69 1
        1 0 69

        humidity-to-location map:
        60 56 37
        56 93 4

        """

    35 == (example |> parse |> solve)
