module Program =
    open System
    open MParser.Test
    open LParserC.LParserC
    open MParser.MParser

    let [<EntryPoint>] debug _ =
        let t = TestClass ()
        //t.TestListParser ()
        while true do
            printf ">>> "
            let mutable is = Console.ReadLine () + "\n" |> createStrStream
            let mutable i = true
            while i do
                try
                    let (v, s) = (MaltParser (emptysParser (Some is))).Value;
                    is <- s
                    printfn "size: %A" s.size
                    printfn "pos: %A" s.pos
                    printfn "val: %A" v
                    let r = Some s |> emptysParser
                    if r.Value.size = r.Value.valu.Length
                    then i <- false
                with
                | :? ApplicationException ->
                    printfn "error."
                    i <- false
        0
