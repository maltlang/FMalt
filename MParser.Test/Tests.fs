namespace MParser.Test

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open MParser.MParser
open LParserC.LParserC
open System

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
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"
    
    [<TestMethod>]
    member this.TestUIntParser () =
        let (v, s) = (MUIntParser (Some (createStrStream "123"))).Value;
        match v.valu with
        | Uint x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreEqual(uint64 123, x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"

    [<TestMethod>]
    member this.TestIntParser () =
        let (v, s) = (MIntParser (Some (createStrStream "-123"))).Value;
        match v.valu with
        | Int x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreEqual(int64 -123, x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"

    [<TestMethod>]
    member this.TestCharParser () =
        let (v, s) = (MCharParser (Some (createStrStream "'\n'()"))).Value;
        match v.valu with
        | Char x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreEqual('\n', x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"

    [<TestMethod>]
    member this.TestStringParser () =
        let (v, s) = (MStringParser (Some (createStrStream "\"\\ssr\"()"))).Value;
        match v.valu with
        | String x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            Assert.AreEqual("\"\\ssr\"", x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"

    [<TestMethod>]
    member this.TestSymbolParser () =
        let (v, s) = (MSymbolParser (Some (createStrStream "Ss90t ()"))).Value;
        match v.valu with
        | Symbol x -> 
            printfn "pos: %A" s.pos
            printfn "%A" x
            printfn "%A" s
            Assert.AreEqual("Ss90t", x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"

    [<TestMethod>]
    member this.TestXdParser () =
        let (v, s) = (MXdParser (Some (createStrStream "#Å¶Å¶Å¶\n()"))).Value;
        match v.valu with
        | Nil -> 
            printfn "pos: %A" s.pos
            //Assert.AreNotEqual(, x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"
    
    [<TestMethod>]
    member this.TestListParser () =
        let (v, s) = (MListParser (Some (createStrStream "(1 -2 true \"ooo\" -1.2 () str)"))).Value;
        match v.valu with
        | List x ->
            printfn "pos: %A" s.pos
            printfn "%A" x
            //Assert.AreNotEqual(, x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"
    
    [<TestMethod>]
    member this.TestMaltParser () =
        let (v, s) = (MaltParser (Some (createStrStream (Console.ReadLine ())))).Value;
        match v.valu with
        | List x ->
            printfn "pos: %A" s.pos
            printfn "%A" x
            //Assert.AreNotEqual(, x);
        | _ -> failwith "Éµ±Æ£¬Õ¨ÁË°É£¬¹þ¹þ¹þ¹þ¹þ¹þ¹þ"