namespace LParserC

module LParserC =
    open System

    // StrStream and operation functions

    type Pos = {
        line:   int;
        col:    int;
    }

    [<Struct>]
    type StrStream = {
        valu:   string;
        size:   int;
        pos: Pos
    }

    let inline createStrStream (s: string) = {
            valu    = s;
            size    = 0;
            pos = {
                col = 1;
                line= 1;
            }
        }

    let inline getHead (self: StrStream) = self.valu.Chars self.size

    type OutOfRange(pos: Pos) = inherit ApplicationException()

    let inline slice (self: StrStream) = 
        if self.size = self.valu.Length
        then raise (OutOfRange self.pos)
        else (getHead self, 
                if getHead self = '\n'
                then {
                    size    = self.size + 1;
                    valu    = self.valu;
                    pos     = {
                        line    = self.pos.line + 1;
                        col     = 1
                    }}
                else {
                    size    = self.size + 1;
                    valu    = self.valu;
                    pos     = {
                        line    = self.pos.line;
                        col     = self.pos.col + 1
                    }})

    let inline highSlice s1 s2 =
        assert (s1.size < s2.size)
        let rec recSlice s1 s2 =
            if s1.size = s2.size
            then ""
            else
                let (f, l) = slice s1
                string f + recSlice l s2
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

    let emptysParser = rpt emptyParser

    let parseSegm =
        charParser '(' *
        charParser ')' *
        charParser '[' *
        charParser ']' *
        charParser '<' *
        charParser '>' *
        charParser ',' *
        charParser '.' *
        charParser '&' *
        charParser '*' *
        emptyParser


    let numberParserR s =
        let (head, tail) = slice s
        if head >= '0' && head <= '9'
        then Some tail
        else None

    let numberParser = boxSay numberParserR

    let numbersParser = rpt numberParser

    let anyChar =
        boxSay ( fun s ->
            let (_, t) = slice s
            Some t)

    let ConChar = (charParser '\\' + anyChar) * anyChar

    // Export

    let appendl f =
        function
        | Some x -> 
            try
                match f (Some x) with
                | Some y -> Some x
                | _ -> None
            with
            | :? OutOfRange -> Some x
        | _ -> None

    let parseBool = stringParser "false" * stringParser "true"

    let parseUint = numberParser + numbersParser

    let parseInt = (charParser '+' * charParser '-') + parseUint

    let parseFloat = (parseInt * parseUint) + charParser '.' + parseUint

    let parseRational = (parseInt * parseUint) + charParser '/' + (parseInt * parseUint)

    let parseChar = charParser '\'' + ConChar + charParser '\''

    let parseString = charParser '\"' + rpt (charParser '\"' * ConChar)
        
// please use automatic curry
// demo: (charParser 'a') s
// demo: (boxSay parserFunc) s
