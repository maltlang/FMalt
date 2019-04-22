namespace CodeGen

open Precompiler

type ModuleImages = {
    a: int
}

module CodeGen =
    let CodeGenerator (e: Precompiler.Expr): ModuleImages =
        {a=0}
