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

Category : [ Seed , Soil , Fertilizer , Water , Light , Temperature , Humidity , Location]
Entry : { category: Category, number: Nat }

Game :
    { seeds: List Nat
    , mapping: Dict Entry Entry
    }

parse : Str -> Game
parse = \str ->
    parser : Parser _ Game
    parser =
        parseCategory = \mapStr, srcCategory, destCategory ->
            \pipeline ->
                pipeline
                |> skip (many (string "\n"))
                |> skip (string (Str.concat mapStr " map:\n"))
                |> keep
                    (many
                        ((const \dest -> \src -> \range -> {dest: {category: destCategory, number: dest}, src: {category: srcCategory, number: src}, range})
                        |> keep digits
                        |> skip (string " ")
                        |> keep digits
                        |> skip (string " ")
                        |> keep digits
                        |> skip (string "\n")))

        const \seeds -> \m1 -> \m2 -> \m3 -> \m4 -> \m5 -> \m6 -> \m7 ->
            basicEntries = List.join [m1, m2, m3, m4, m5, m6, m7]
            allEntries = List.walk basicEntries (Dict.empty {}) \acc, spec ->
                     List.walk (List.range {start: At 0, end: Length spec.range}) acc \innerAcc, idx ->
                        Dict.insert innerAcc
                            ({category: spec.src.category, number: (spec.src.number + idx)})
                            ({category: spec.dest.category, number: (spec.dest.number + idx)})
            { seeds, mapping: allEntries }
        |> skip (string "seeds: ")
        |> keep (digits |> sepBy (string " "))

        |> (parseCategory "seed-to-soil" Seed Soil)
        |> (parseCategory "soil-to-fertilizer" Soil Fertilizer)
        |> (parseCategory "fertilizer-to-water" Fertilizer Water)
        |> (parseCategory "water-to-light" Water Light)
        |> (parseCategory "light-to-temperature" Light Temperature)
        |> (parseCategory "temperature-to-humidity" Temperature Humidity)
        |> (parseCategory "humidity-to-location" Humidity Location)

    parseStr parser str
    |> Result.withDefault {seeds: [], mapping: Dict.empty {}}

solve : Game -> Nat
solve = \{seeds, mapping} ->
    findLocation = \seed ->
        go = \srcEntry ->
            when Dict.get mapping srcEntry is
                Ok destEntry ->
                    when destEntry.category is
                        Location -> destEntry.number
                        _ -> go destEntry
                _ ->
                    when srcEntry.category is
                        Seed ->        go {category: Soil, number: srcEntry.number}
                        Soil ->        go {category: Fertilizer, number: srcEntry.number}
                        Fertilizer ->  go {category: Water, number: srcEntry.number}
                        Water ->       go {category: Light, number: srcEntry.number}
                        Light ->       go {category: Temperature, number: srcEntry.number}
                        Temperature -> go {category: Humidity, number: srcEntry.number}
                        Humidity ->    go {category: Location, number: srcEntry.number}
                        Location ->    srcEntry.number

        go {category: Seed, number: seed}

    seeds
    |> List.map findLocation
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
