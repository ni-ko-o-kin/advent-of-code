app "hello"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.1.3/5SXwdW7rH8QAOnD71IkHcFxCmBEPtFSLAIkclPEgjHQ.tar.br" }
    imports [pf.Stdout]
    provides [main] to pf

Foo : [Foo]
Bar : [Bar Foo]

main =
    Stdout.line (fn x)

fn : Bar -> Str
fn = \_ -> ""

x : Bar
x = Bar SomeOtherTag
