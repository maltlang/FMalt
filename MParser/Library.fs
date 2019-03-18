namespace MParser

module MParser =
    open System
    open System.Collections
    open LParserC.LParserC

    type RValue =
        | Nil
        | Bool      of bool
        | Char      of char
        | Uint      of uint64
        | Int       of int64
        | Float     of float
        | String    of string
        | Symbol    of string
        | Tuple     of MataTree array
        | List      of MataTree list

    and MataTree = {
        valu:   RValue
        pos:    Pos
    }

    type MParser = StrStream option -> (MataTree * StrStream) option
        
    let inline ParserAnyAtom (f1: Parserc) f2 f3 =
        function
        | Some s ->
            match emptysParser (Some s) |> f1 |> (appendl parseSegm) with
            | Some x -> Some ({
                        valu    = f2 ((highSlice s x) |> f3)
                        pos     = {
                            line= s.pos.line;
                            col = s.pos.col
                        }}, (emptysParser (Some x)).Value)
            | _ -> None
        | _ -> None

    type InvalidEscapeChar(pos: Pos) = inherit ApplicationException()

    let ToCharThisIsShit (pos: Pos) (s: string) =
        if s.Chars 1 = '\\'
        then
            match s.Chars 2 with
            | '\\' -> '\\'
            | '\'' -> '\''
            | '\"' -> '\"'
            | 'n' -> '\n'
            | 't' -> '\t'
            | 'r' -> '\r'
            | 'f' -> '\f'
            | 'b' -> '\b'
            | _ -> raise (InvalidEscapeChar pos)
        else s.Chars 1

    // parser

    let parseSymbol s = s |> highrpt parseSegm anyChar

    let MCharParser =
        function
        | Some x -> Some x |> ParserAnyAtom parseChar   Char    (ToCharThisIsShit x.pos)
        | _ -> None

    let MBoolParser   x = x |> ParserAnyAtom parseBool      Bool    Convert.ToBoolean
    let MUIntParser   x = x |> ParserAnyAtom parseUint      Uint    Convert.ToUInt64 
    let MIntParser    x = x |> ParserAnyAtom parseInt       Int     Convert.ToInt64  
    let MFloatParser  x = x |> ParserAnyAtom parseFloat     Float   Convert.ToDouble
    let MStringParser x = x |> ParserAnyAtom parseString    String  Convert.ToString
    let MSymbolParser x = x |> ParserAnyAtom parseSymbol    Symbol  (fun s -> String.Intern (Convert.ToString (s)))

    let rec MExprParser s =
        s |>
        MXdParser *
        MQuoteParser *
        MEvalParser *
        MListParser *
        MAtomParser

    and MXdParser x =
        match x |> charParser '#' + highrpt (charParser '\n') anyChar with
        | Some y -> Some ({
            pos = x.Value.pos;
            valu= Nil}
            , y)
        | _ -> None

    and MQuoteParser s = 
        match s |> (charParser '&') with
        | Some t ->
            match Some t |> MExprParser with
            | Some (v, t) -> Some ({
                pos=s.Value.pos;
                valu= List [{pos= s.Value.pos; valu= Symbol (String.Intern ("quote"))}; v]
            }, t)
            | _ -> None
        | _ -> None

    and MEvalParser s = 
        match s |> (charParser '*') with
        | Some t ->
            match Some t |> MExprParser with
            | Some (v, t) -> Some ({
                pos=s.Value.pos;
                valu= List [{pos= s.Value.pos; valu= Symbol (String.Intern ("eval"))}; v]
            }, t)
            | _ -> None
        | _ -> None

    and MListParser s =
        let rec rf s =
            match s |> charParser ')' with
            | Some x -> Some ([], x)
            | _ ->
                match MEvalParser s with
                | Some (v, t) ->
                    match rf s with
                    | Some (v2, t) -> Some ((List.append [v] v2), t)
                    | _ -> None
                | _ -> None
        match s |> (charParser '(') |> rf with
        | Some (v, t) -> Some ({
            pos= s.Value.pos;
            valu= List v}
            , t)
        | _ -> None 

    and MAtomParser =
        MBoolParser *
        MCharParser *
        MStringParser *
        MUIntParser *
        MIntParser *
        MFloatParser *
        MSymbolParser

    // MaltRawParser
    let inline MaltParser s = s |> MExprParser