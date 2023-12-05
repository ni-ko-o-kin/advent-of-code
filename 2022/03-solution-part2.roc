app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout, pf.Task.{ await }, pf.File, pf.Path, Maybe.{ Maybe }]
    provides [main] to pf

# a-z 1-26
# A-Z 27-52

Priority : U16
Rucksack : List Str
Group : [Group Rucksack Rucksack Rucksack]

main =
    toNoneEmptyLines : Str -> List Str
    toNoneEmptyLines = \str ->
        str
        |> Str.split "\n"
        |> List.map Str.trim
        |> List.dropIf Str.isEmpty

    task =
        input <- await (File.readUtf8 (Path.fromStr "./03-input"))
        input
            |> toNoneEmptyLines
            |> solve
            |> Num.toStr
            |> Stdout.line

    Task.mapFail task (\_ -> crash "oh no")

solve : List Str -> Priority
solve = \lines ->
    groups : List Group
    groups =
        go = \acc, elem ->
            newRucksack = Str.graphemes elem

            when acc is
                [] ->
                    [[newRucksack]]

                [.., [_, _, _]] ->
                    List.append acc [newRucksack]

                [.., unfinshedGroup] ->
                    List.set acc (List.len acc - 1) (List.append unfinshedGroup newRucksack)

        makeGroup : List Rucksack -> Maybe Group
        makeGroup = \rucksacks ->
            when rucksacks is
                [r1, r2, r3] -> Just (Group r1 r2 r3)
                _ -> Nothing

        lines
        |> List.walk [] go
        |> Maybe.filterMap makeGroup


    getOverlappingChar : Group -> Maybe Str
    getOverlappingChar = \Group r1 r2 r3 ->
        Set.intersection
            (Set.fromList r1)
            (Set.intersection
                (Set.fromList r2)
                (Set.fromList r3))
        |> Set.toList
        |> List.first
        |> Maybe.fromResult

    calcPriority : Str -> Maybe Priority
    calcPriority = \str ->
        str
        |> Str.toUtf8
        |> List.first
        |> Maybe.fromResult
        |> Maybe.map Num.toU16
        |> Maybe.andThen \x ->
            if x >= 97 && x <= 122 then
                Just (x - 96)
            else if x >= 65 && x <= 90 then
                Just (x - 38)
            else
                Nothing

    groups
         |> Maybe.filterMap getOverlappingChar
         |> Maybe.filterMap calcPriority
         |> List.sum
