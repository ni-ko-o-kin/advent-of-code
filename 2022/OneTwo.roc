app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

main =
    [One, Two, One, Two]
    |> solve
    |> List.map tagToStr
    |> Str.joinWith ", "
    |> Stdout.line

tagToStr : [One, Two] -> Str
tagToStr = \tag ->
    when tag is
        One -> "One"
        Two -> "Two"

solve : List [One, Two] -> List [One]
solve = \tags ->
    List.map tags \tag ->
        when tag is
            One -> One
            Two -> Two
