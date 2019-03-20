namespace Precompiler

module Precompiler =
    open MParser.MParser

    type CompilerContext = {
        a: int
    }

    let prec (context: CompilerContext) (args: MataTree) =
        printfn "Hello %s" ""
