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
                        ((const \dest -> \src -> \range -> {dest: {start: dest, end: dest + (range - 1)}, src: {start: src, end: src + (range - 1)}})
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
    dbg seeds
    findMappings : List Entry, Range -> List Range
    findMappings = \entries, originalRange ->
        # find mappings for one range
        dbg entries
        dbg originalRange
        (finalRemaining, finalNew) = List.walk entries ([originalRange], []) \(r, n), entry ->
            go = \remaining, notInEntry, new ->
                when remaining is
                    [] ->
                        dbg "no remaining"
                        (notInEntry, new)

                    [range, .. as restOfRemaining] ->
                        dbg range
                        dbg restOfRemaining
                        # not in range
                        if entry.src.start > range.end || entry.src.end < range.start then
                            go restOfRemaining (List.append notInEntry range) new
                        else
                            # full or partially in range
                            p1 = Num.min entry.src.start range.start
                            p2 = Num.max entry.src.start range.start
                            p3 = Num.min entry.src.end range.end
                            p4 = Num.max entry.src.end range.end

                            before = if p2 > range.start then Ok { start: p1, end: p2 } else Err NotInRange
                            after = if p3 < range.end then Ok { start: p3, end: p4 } else Err NotInRange

                            shift =
                                if (entry.src.start >= entry.dest.start) then
                                    RightShift (entry.src.start - entry.dest.start)
                                else
                                    LeftShift (entry.dest.start - entry.src.start)

                            go
                                restOfRemaining
                                (notInEntry |> List.appendIfOk before |> List.appendIfOk after)
                                (new |> List.append
                                    { start: when shift is
                                        RightShift x -> p2 - x
                                        LeftShift x -> p2 + x
                                    , end: when shift is
                                        RightShift x -> p3 - x
                                        LeftShift x -> p4 + x
                                    })
            go r [] n

        # map ranges that have no mapping directly
        List.concat finalRemaining finalNew

    List.walk mapping seeds \ranges, m ->
        dbg ranges
        List.joinMap ranges \range ->
            dbg range
            findMappings m range

    |> List.map .start
    |> List.sortAsc
    |> \xs ->
        dbg xs
        xs
    |> List.first
    |> Result.withDefault 0

# expect
#     example =
#         """
#         seeds: 79 14 55 13

#         seed-to-soil map:
#         50 98 2
#         52 50 48

#         soil-to-fertilizer map:
#         0 15 37
#         37 52 2
#         39 0 15

#         fertilizer-to-water map:
#         49 53 8
#         0 11 42
#         42 0 7
#         57 7 4

#         water-to-light map:
#         88 18 7
#         18 25 70

#         light-to-temperature map:
#         45 77 23
#         81 45 19
#         68 64 13

#         temperature-to-humidity map:
#         0 69 1
#         1 0 69

#         humidity-to-location map:
#         60 56 37
#         56 93 4

#         """

#     solution = example |> parse |> solve
#     solution == 46

expect
    example =
        """
        seeds: 1 1 0 1

        seed-to-soil map:
        1 0 1
        0 1 1

        soil-to-fertilizer map:
        3 0 2

        fertilizer-to-water map:
        1 1 1

        water-to-light map:
        1 1 1

        light-to-temperature map:
        1 1 1

        temperature-to-humidity map:
        1 1 1

        humidity-to-location map:
        1 1 1

        """

    solution = example |> parse |> solve
    solution == 1
