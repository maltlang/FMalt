module Program =
    open MParser.Test

    let [<EntryPoint>] debug _ =
        let t = TestClass ()
        t.TestListParser ()
        //t.TestListParser ()
        0
