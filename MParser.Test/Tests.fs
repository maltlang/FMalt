namespace MParser.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MParser.MParser
open LParserC.LParserC

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestBoolParser () =
        let (v, _) = (MBoolParser (Some (createStrStream "true"))).Value;
        match v.valu with
        | Bool x -> Assert.IsTrue(x);
        | _ -> ()

    [<TestMethod>]
    member this.TestUIntParser () =
        let (v, s) = (MUIntParser (Some (createStrStream "123"))).Value;
        match v.valu with
        | Uint x -> 
            printfn "%i" x
            Assert.AreNotEqual(123, x);
        | _ -> ()