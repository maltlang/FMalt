namespace LParserC

module LParserC =

    // StrStream and operation functions

    [<Struct>]
    type StrStream = {
        valu:   string;
        size:   int;
        line:   int;
        col:    int;
    }

    let inline createStrStream (s: string) = {
            valu    = s;
            size    = 0;
            line    = 1;
            col     = 1
        }

    let inline getHead (self: StrStream) = self.valu.Chars self.size

    exception OutOfRange

    let inline getNext (self: StrStream) =
        if self.size = self.valu.Length
        then raise OutOfRange
        else
            if getHead self = '\n'
            then {
                size    = self.size + 1;
                valu    = self.valu;
                line    = self.line + 1;
                col     = 1}
            else {
                size    = self.size + 1;
                valu    = self.valu;
                line    = self.line;
                col     = self.col + 1}

    let inline slice (self: StrStream) = (getHead self, getNext self)

    let inline highSlice s1 s2 =
        assert (s1.size < s2.size)
        let rec recSlice s1 s2 =
            if s1.size = s2.size
            then ""
            else string (getHead s1) + recSlice (getNext s1) s2
        recSlice s1 s2

    // ParserC and ToolFunctions

    type Parserc = StrStream option -> StrStream option

    // boxSay(foxSay), function = Packing
    let inline boxSay f v =
        match v with
        | Some s    -> f s
        | None      -> None

    let inline (+) f1 f2 = f1 >> f2

    let inline (*) f1 f2 = 
        function
        | Some x ->
            match f1 (Some x) with
            | Some t    -> Some t
            | None      -> f2 (Some x)
        | None -> None

    let rpt (f: Parserc) =
        let rec rf (v: StrStream option) =
            try
                match f v with
                | Some x    -> rf (Some x)
                | None      -> v
            with
                | :? OutOfRange -> v
        rf

    // Expand

    let charParser v = 
        boxSay (
            fun s ->
                let (head, tail) = slice s
                if head = v then Some tail else None)

    let charNotParser (v: char) = 
        boxSay (
            fun s ->
                let (head, tail) = slice s
                if not (head = v) then Some tail else None)

    let rec stringParserR v s =
        if v.size = v.valu.Length
        then Some s
        else    let ((h1, t1), (h2, t2)) = (slice v, slice s)
                if h1 = h2
                then stringParserR t1 t2
                else None

    let inline stringParser v =
        boxSay (fun s -> stringParserR (createStrStream v) s)

    let emptyParser = charParser ' ' * charParser '\n' * charParser '\t' * charParser '\r'

    let inline parseSegm s =
        charParser '(' *
        charParser ')' *
        charParser '[' *
        charParser ']' *
        charParser '<' *
        charParser '>' *
        charParser ',' *
        charParser '.' *
        emptyParser


    let numberParserR s =
        let (head, tail) = slice s
        if head >= '0' && head <= '9'
        then Some tail
        else None

    let numberParser = boxSay numberParserR

    let inline numbersParser s = (rpt numberParser) s

    let anyChar =
        boxSay ( fun s ->
            let (_, t) = slice s
            Some t)

    let inline ConChar s =
        s |> (charParser '\\' + anyChar) * anyChar

    // Export

    let inline parseUint s =
        s
        |> numberParser
        |> numbersParser

    let inline parseInt s =
        s
        |> (charParser '+' * charParser '-')
        |> parseUint

    let inline parseFloat s =
        s |>
        (parseInt * parseUint) + charParser '.' + parseUint

    let inline parseRational s =
        s |>
        (parseInt * parseUint) |> charParser '/' |> (parseInt * parseUint)

    let inline parseChar s =
        s 
        |> charParser '\''
        |> ConChar
        |> charParser '\''

    let inline parseString s =
        s 
        |> charParser '\"'
        |> rpt (charParser '\"' * ConChar)
        
// please use automatic curry
// demo: (charParser 'a') s
// demo: (boxSay parserFunc) s
