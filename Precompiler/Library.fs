namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC
    //open Map

    type InvalidTypeLabel(pos: Pos) = inherit System.ApplicationException()
    type ParamerIsNotSymbol(pos: Pos) = inherit System.ApplicationException()   
    type InvalidTopLevelExpr(pos: Pos) = inherit System.ApplicationException()

    type ExprValue =
    | Nil
    | Const of RValue
    | Symbol of string
    | Set of (string * Expr)
    | Let of (string * Expr * Expr list)
    | Use of (string * Expr * Expr list)
    | Lambda of (string list * Expr list)
    | MethodCall of (Expr * string * Expr list)
    
    and Expr = (ExprValue * Pos)

    let rec prec (args: MataTree): Expr =
        match args.valu with
        | RValue.Nil -> (Nil, args.pos)
        | RValue.Bool (_)
        | RValue.Char (_)
        | RValue.Int (_)
        | RValue.Uint (_)
        | RValue.Float (_)
        | RValue.String (_) -> (Const args.valu, args.pos)
        | RValue.Symbol (x) -> (Symbol x, args.pos)
        | RValue.List ([{valu=RValue.Symbol("set")}; {valu=RValue.Symbol(name)}; e]) ->
            (Set (name, prec e), args.pos)
        | RValue.List ([{valu=RValue.Symbol("set")}; {valu=RValue.Symbol(name)}; e]) ->
            (Set (name, prec e), args.pos)
        | _ -> raise (InvalidTopLevelExpr args.pos)