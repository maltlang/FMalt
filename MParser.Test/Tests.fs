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
        | Bool x -> 
            printfn "%A" x
            Assert.IsTrue(x);
        | _ -> ()
    
    [<TestMethod>]
    member this.TestUIntParser () =
        let (v, s) = (MUIntParser (Some (createStrStream "123"))).Value;
        match v.valu with
        | Uint x -> 
            printfn "%A" x
            Assert.AreNotEqual(123, x);
        | _ -> ()

    [<TestMethod>]
    member this.TestIntParser () =
        let (v, s) = (MIntParser (Some (createStrStream "+123"))).Value;
        match v.valu with
        | Uint x -> 
            printfn "%A" x
            Assert.AreNotEqual(123, x);
        | _ -> ()

    [<TestMethod>]
    member this.TestCharParser () =
        let (v, s) = (MCharParser (Some (createStrStream "'t'"))).Value;
        match v.valu with
        | Char x -> 
            printfn "%A" x
            Assert.AreNotEqual("t", x);
        | _ -> ()

    [<TestMethod>]
    member this.TestStringParser () =
        let (v, s) = (MStringParser (Some (createStrStream "\"\\ssr\""))).Value;
        match v.valu with
        | String x -> 
            printfn "%A" x
            Assert.AreNotEqual("\\ssr", x);
        | _ -> ()