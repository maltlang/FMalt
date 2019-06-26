namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC
    //open Map

    type InvalidTypeLabel(pos: Pos)     = inherit System.ApplicationException()
    type ParamerIsNotSymbol(pos: Pos)   = inherit System.ApplicationException()   
    type InvalidTopLevelExpr(pos: Pos)  = inherit System.ApplicationException()

    type Expr = (ExprValue * Pos)

    and ExprValue =
    | Nil
    | Const of RValue
    | Symbol of string
    | Set of (string * Expr)
    | Let of (string * Expr)
    | Use of (string * Expr * Expr list)
    | Lambda of (string list * Expr list)
    | MethodCall of (Expr * string * Expr list)

    let getSymbol (args: MataTree) =
        match args.valu with
        | RValue.Symbol (s) -> s
        | _ -> raise (ParamerIsNotSymbol args.pos)

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
        | RValue.List ([{valu=RValue.Symbol("quote")}; {valu=RValue.Symbol(name)}]) ->
            (Const (RValue.Symbol (name)), args.pos)
        | RValue.List ([{valu=RValue.Symbol("set")}; {valu=RValue.Symbol(name)}; e]) ->
            (Set (name, prec e), args.pos)
        | RValue.List ([{valu=RValue.Symbol("let")};
                        {valu=RValue.Symbol(name)};
                        e]) ->
            (Let (name, prec e), args.pos)
        | RValue.List ({valu=RValue.Symbol("use")} ::
                        {valu=RValue.Symbol(name)} ::
                        e :: ex) ->
            (Use (name, prec e, ((List.map prec) ex)), args.pos)
        | RValue.List ({valu=RValue.Symbol("\\")} :: {valu=RValue.List (l)} :: e) ->
            (Lambda (((List.map getSymbol) l), ((List.map prec) e)), args.pos)
        | RValue.List (e :: {valu=RValue.Symbol (name)} :: ex) ->
            (MethodCall ((prec e, name, ((List.map prec) ex))), args.pos)
        | _ -> raise (InvalidTopLevelExpr args.pos)
    
    let inline Prec x = prec x