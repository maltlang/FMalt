namespace Precompiler.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MParser
open Precompiler.Precompiler
open System
open System

[<TestClass>]
type TestClass () =
    [<TestMethod>]
    member this.getTypeLabel1 () =
        let (m, _) = (Some ("(int uint)" |> LParserC.LParserC.createStrStream) |> MParser.MaltParser).Value
        Assert.IsTrue(Equ (m |> getTypeLabel) Any);
