module Program =
    open MParser.Test

    let [<EntryPoint>] debug _ =
        let t = TestClass ()
        t.TestCharParser ()
        t.TestStringParser ()
        t.TestXdParser ()
        0
