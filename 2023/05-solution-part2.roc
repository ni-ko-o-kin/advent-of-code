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

Game : { seeds: List Range , mapping: List (List Entry)}
Range : { start: Nat, end: Nat }
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
                        [x, y] -> Ok {start: x, end: x + (y - 1)}
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
    findMappings : List Entry, Range -> (List Range, List Range)
    findMappings = \entries, range ->
        # find mappings for one range
        List.walk entries ([], []) \(remaining, new), entry ->
            if List.isEmpty remaining then
                ([], new)
            else
                if entry.src > range.end then
                    (remaining, new)
                else if entry.src + entry.range < range.start then
                    (remaining, new)
                else if entry.src >= range.start && entry.src + entry.range <= range.end then
                    beforeEntryRange =
                        if (entry.src - 1) - range.start > 0 then
                            Ok { start: range.start, end: entry.src - 1 }
                        else
                            Err ZeroLengthRange
                    afterEntryRange =
                        if range.end - (entry.src + entry.range + 1) > 0 then
                            Ok { start: entry.src + entry.range + 1, end: range.end }
                        else
                            Err ZeroLengthRange

                    ( List.concat remaining (List.keepOks [beforeEntryRange, afterEntryRange] \x -> x)

                    # TODO handle mapping of range inside entry
                    , List.concat new []
                    )
                else if 1 then # handle entry left ouside of range and inside of range with right side
                else if 1 then # same but for right side
                else if 1 then # handle entry is bigger than range on both sides
                else
                    (remaining, new)

    List.walk mapping seeds \ranges, m ->
        go : List Range, List Range -> List Range
        go = \remainingRanges, newRanges ->
            when remainingRanges is
                [] ->
                    newRanges
                [range, .. as rest] ->
                    (moreRemainingRanges, moreNewRanges) = findMappings m range
                    # TODO map moreRemainingRanges directly bc no mappings could be found

                    go (List.concat rest moreRemainingRanges) (List.concat newRanges moreNewRanges)

        go ranges []
    |> List.map .start
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

    46 == (example |> parse |> solve)
