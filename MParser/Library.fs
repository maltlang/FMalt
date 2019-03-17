namespace MParser

module MParser =
    open System
    open LParserC.LParserC
    open System

    type RValue =
        | Nil
        | Bool      of bool
        | Char      of char
        | Uint      of uint64
        | Int       of int64
        | String    of string
        | Symbol    of string
        | Tuple     of MataTree array
        | List      of MataTree list

    and MataTree = {
        valu:   RValue
        line:   int
        col:    int
    }

    type MParser = StrStream option -> (MataTree * StrStream) option

    (*
    let inline And (f1: MParser) (f2: MParser) 
        fun x ->
            f1 x
    *)

    let rpt (f: MParser) =
        let rec rf (v: StrStream option) =
            try
                match f v with
                | Some (_, x) -> rf (Some x)
                | None -> v
            with
                | :? OutOfRange -> v
        rf
        
    let inline ParserAnyAtom f1 f2 f3 =
        fun s ->
        match s with
        | Some s ->
            match f1 (Some s) with
            | Some x -> Some ({
                    valu= f2 ((highSlice s x) |> f3)
                    line= s.line
                    col= s.col
                }
                ,x)
            | None -> None
        | None -> None
        
    let MBoolParser   x = x |> ParserAnyAtom parseBool   Bool    Convert.ToBoolean
    let MCharParser   x = x |> ParserAnyAtom parseChar   Char    Convert.ToChar
    let MUIntParser   x = x |> ParserAnyAtom parseUint   Uint    Convert.ToUInt64
    let MIntParser    x = x |> ParserAnyAtom parseInt    Int     Convert.ToInt64
    let MStringParser x = x |> ParserAnyAtom parseString String  Convert.ToString

    
