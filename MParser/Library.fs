namespace MParser

module MParser =
    open System
    open LParserC.LParserC
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
        pos: Pos
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

    let MXdParser x =
        match x |> charParser '#' + highrpt (charNotParser '\n') anyChar with
        | Some y -> Some ({
            pos = x.Value.pos;
            valu= Nil}
            , y)
        | _ -> None

    let MCharParser =
        function
        | Some x -> Some x |> ParserAnyAtom parseChar   Char    (ToCharThisIsShit x.pos)
        | _ -> None

    let MBoolParser   x = x |> ParserAnyAtom parseBool   Bool    Convert.ToBoolean
    let MUIntParser   x = x |> ParserAnyAtom parseUint   Uint    Convert.ToUInt64 
    let MIntParser    x = x |> ParserAnyAtom parseInt    Int     Convert.ToInt64  
    let MFloatParser  x = x |> ParserAnyAtom parseFloat  Float   Convert.ToDouble
    let MStringParser x = x |> ParserAnyAtom parseString String  Convert.ToString

    let MAtomParser =
        MBoolParser *
        MCharParser *
        MStringParser *
        MUIntParser *
        MIntParser *
        MFloatParser

    
