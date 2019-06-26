// Learn more about F# at http://fsharp.org

open System
open LParserC.LParserC
open MParser.MParser
open Precompiler.Precompiler
open CodeGen.CodeGen

[<EntryPoint>]
let main argv =
    while true do
        let m = CodeGen2WordCode {
            path="";
            global_lambda="";
            lambdas = ref [| "" |]}
        Console.Write(">>> ")
        let input = Console.ReadLine()
        if input.Length = 0
        then ()
        else
            let mutable ms = createStrStream (input)
            match Some (ms) |> MaltParser with
            | Some (e, n) ->
                printf "%s" (e |> Prec |> m)
                ms <- n
            | None -> printf "error"
    0