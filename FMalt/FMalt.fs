// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text
open System.Threading.Tasks
open LParserC.LParserC
open MParser.MParser
open Precompiler.Precompiler
open CodeGen.CodeGen

let inline lambdabox str = "(\\\n" + str + ")\n"

let modulebox m =
    "(srcpath" + m.path + ")\n" +
    if (! m.lambdas).Length = 0
    then ""
    else Array.reduce (+) (Array.map (lambdabox) (! m.lambdas))
    + lambdabox (! m.global_lambda)

let comp1 filepath =
    let file = File.OpenText(filepath) 
    let m = {
        path            = filepath;
        global_lambda   = ref ""
        lambdas         = ref [| |]}
    let mf = CodeGen2WordCode m

    use fh = new StreamReader(filepath)
    let input = fh.ReadToEnd()
    let mutable ms = createStrStream input

    while ms.size < ms.valu.Length-1 do
        match Some (ms) |> MaltParser with
        | Some (e, n) ->
            m.global_lambda := (!m.global_lambda) + (e |> Prec |> mf)
            ms <- n
        | None -> failwithf "compiler error in %s Ln:%i Cos: %i" filepath ms.pos.line ms.pos.col
    use ofile = new StreamWriter(filepath + ".mwc")
    ofile.Write(modulebox m)

[<EntryPoint>]
let main argv =
    argv
    |> Array.map (fun x -> new Task(fun _ -> comp1 x))
    |> Array.map (fun x -> x.Start (); x)
    |> Array.map (fun x -> x.Wait ())
    |> ignore
    0