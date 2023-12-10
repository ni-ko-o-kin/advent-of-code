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

Game : { seeds: List Seed , mapping: List (List Entry)}
Seed : { start: Nat, range: Nat }
Entry : { src: Nat, dest: Nat, range: Nat }

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
                        ((const \dest -> \src -> \range -> {dest, src, range})
                        |> keep digits
                        |> skip (string " ")
                        |> keep digits
                        |> skip (string " ")
                        |> keep digits
                        |> skip (string "\n")))

        const \seeds -> \m1 -> \m2 -> \m3 -> \m4 -> \m5 -> \m6 -> \m7 ->
            seedsFromPairs =
                List.chunksOf seeds 2
                |> List.keepOks \xs ->
                    when xs is
                        [x, y] -> Ok {start: x, range: y}
                        _ -> Err InvalidPair
            { seeds: seedsFromPairs, mapping: [m1, m2, m3, m4, m5, m6, m7] }

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
    findSeed : Nat -> Result Nat [NotFound]
    findSeed = \initialDestination ->
        isInSeeds : Nat -> Bool
        isInSeeds = \seed ->
            seeds
            |> List.findFirst \{start, range} -> seed >= start && seed < start + range
            |> Result.isOk

        go : Result Nat [NotFound], List (List Entry) -> Result Nat [NotFound]
        go = \destinationResult, reverseMapping ->
            when destinationResult is
                Ok destination ->
                    when reverseMapping is
                        [] ->
                            if isInSeeds destination then
                                Ok destination
                            else
                                Err NotFound
                        [entries, .. as rest] ->
                            entries
                            |> List.findFirst \{dest, range} -> destination >= dest && destination < dest + range
                            |> Result.map \{src, dest} -> src + (destination - dest)
                            |> go rest
                            # .........................
                            # .....ssss_sss............
                            # ...........dddd_ddd......
                Err _ ->
                    destinationResult
        go (Ok initialDestination) (List.reverse mapping)

    findClosestLocation : Result Nat [NotFound], Entry -> Result Nat [NotFound]
    findClosestLocation = \acc, entry ->
        when acc is
            Ok _ ->
                acc
            Err _ ->
                List.range { start: At entry.dest, end: Length entry.range }
                |> List.walk acc \innerAcc, currentDestination ->
                    when acc is
                        Ok _ ->
                            innerAcc
                        Err _ ->
                            findSeed currentDestination
                            |> Result.mapErr \_ -> NotFound

    mapping
    |> List.last
    |> Result.withDefault []
    |> List.sortWith \a,b -> if a.dest < b.dest then LT else GT
    |> List.walk (Err NotFound) findClosestLocation
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

    46 == (example |> parse |> solve)
