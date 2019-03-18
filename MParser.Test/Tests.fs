namespace MParser.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MParser.MParser
open LParserC.LParserC

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.TestBoolParser () =
        let (v, s) = (MBoolParser (Some (createStrStream "true"))).Value;
        match v.valu with
        | Bool x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.IsTrue(x);
        | _ -> ()
    
    [<TestMethod>]
    member this.TestUIntParser () =
        let (v, s) = (MUIntParser (Some (createStrStream "123"))).Value;
        match v.valu with
        | Uint x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreNotEqual(123, x);
        | _ -> ()

    [<TestMethod>]
    member this.TestIntParser () =
        let (v, s) = (MIntParser (Some (createStrStream "-123"))).Value;
        match v.valu with
        | Uint x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreNotEqual(123, x);
        | _ -> ()

    [<TestMethod>]
    member this.TestCharParser () =
        let (v, s) = (MCharParser (Some (createStrStream "'\n'()"))).Value;
        match v.valu with
        | Char x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreNotEqual("t", x);
        | _ -> ()

    [<TestMethod>]
    member this.TestStringParser () =
        let (v, s) = (MStringParser (Some (createStrStream "\"\\ssr\"()"))).Value;
        match v.valu with
        | String x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreNotEqual("\\ssr", x);
        | _ -> ()

    [<TestMethod>]
    member this.TestXdParser () =
        let (v, s) = (MXdParser (Some (createStrStream "#Å¶Å¶Å¶\n()"))).Value;
        match v.valu with
        | Nil -> 
            printfn "pos: %A" s.pos
            //Assert.AreNotEqual(, x);
        | _ -> ()
    
    [<TestMethod>]
    member this.TestListParser () =
        let (v, s) = (MListParser (Some (createStrStream "(1 -2 true \"ooo\" () \"str\" )"))).Value;
        match v.valu with
        | List x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            //Assert.AreNotEqual(, x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"