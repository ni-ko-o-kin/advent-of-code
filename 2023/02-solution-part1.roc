app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br"
    }
    imports [pf.Stdout, pf.Task.{ Task }, "02-input" as input : Str]
    provides [main] to pf

Draw : [Draw Nat Nat Nat]

main : Task {} *
main =
    input
    |> solve
    |> Num.toStr
    |> Stdout.line

solve : Str -> Nat
solve = \str ->
    str
    |> Str.split "\n"
    |> List.keepOks \line ->
        gameId <- Result.try (extractGameId line)
        draws <- Result.try (extractDraws line)

        Ok {gameId, draws}
    |> List.keepIf \{draws} ->
        List.all draws \Draw red green blue ->
            red <= 12 && green <= 13 && blue <= 14
    |> List.map .gameId
    |> List.sum

initDraw : Str, Nat  -> Draw
initDraw = \colorStr, count  ->
    when colorStr is
        "red" -> Draw (count) 0 0
        "green" -> Draw 0 (count) 0
        "blue" -> Draw 0 0 (count)
        _ -> Draw 0 0 0

mergeDraw : Draw, Draw -> Draw
mergeDraw = \Draw red1 green1 blue1, Draw red2 green2 blue2 ->
    Draw (red1 + red2) (green1 + green2) (blue1 + blue2)

extractGameId : Str -> Result Nat [InvalidGameId]
extractGameId = \line ->
    Str.splitFirst line ":"
    |> Result.try  \{before} -> Str.splitFirst before " "
    |> Result.try  \{after} -> Str.toNat after
    |> Result.mapErr  \_ -> InvalidGameId

extractDraws : Str -> Result (List Draw) [InvalidDraws, InvalidDraw]
extractDraws = \line ->
    line
        |> Str.split ":"
        |> List.dropFirst 1
        |> List.first
        |> Result.withDefault ""
        |> Str.split ";"
        |> List.map \drawsStr ->
            drawsStr
            |> Str.split  ", "
            |> List.keepOks \countAndColorStr ->
                countAndColorStr
                |> Str.trim
                |> Str.split " "
                |> \x ->
                    when x is
                        [countStr, colorStr] ->
                            count <- Result.map (Str.toNat countStr)
                            initDraw colorStr count
                        _ -> Err InvalidDraw

            |> List.walk (initDraw "" 0) (\cur, acc -> mergeDraw acc cur)
        |> \xs ->
            if List.isEmpty xs then
                Err InvalidDraws
            else
                Ok xs
