namespace MParser

module MParser =
    open System
    open System.Text
    open LParserC.LParserC

    type RValue =
        | Bool      of bool
        | Uint      of uint64
        | Int       of int64
        | String    of string
        | Symbol    of string
        | Tuple     of MataTree array
        | List      of MataTree list

    and MataTree = {
        valu:   RValue;
        line:   int;
        col:    int;
    }

    type MParser = StrStream option -> (MataTree * StrStream) option

    (*
    let inline And (f1: MParser) (f2: MParser) 
        fun x ->
            f1 x
    *)

    let rpt (f: Parserc) =
        let rec rf (v: StrStream option) =
            try
                match f v with
                | Some x -> rf (Some x)
                | None -> v
            with
                | :? OutOfRange -> v
        rf
        
    