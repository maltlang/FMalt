namespace Precompiler

module Precompiler =
    open MParser.MParser

    type CompilerContext = {
        a: int
    }

    let pureEval (context: CompilerContext) (args: MataTree) =
        0

    let prec (context: CompilerContext) (args: MataTree) =
        0
