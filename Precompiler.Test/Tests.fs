namespace Precompiler.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MParser
open Precompiler.Precompiler

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.testPrec1 () =
        let (m, _) = (Some ("(\ (a b) (\ () 1))" |> LParserC.LParserC.createStrStream) |> MParser.MaltParser).Value
        let a = prec m
        printfn "%A" a
        //Assert.IsTrue(a = );
