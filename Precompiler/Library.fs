namespace Precompiler

module Precompiler =
    open MParser.MParser
    open LParserC.LParserC

    type Body =
    | Atom
    | Let of (RValue * BodyNode * BodyNode list)
    | Use of (FunctionConst * string * BodyNode * BodyNode list)
    | Cond of (BodyNode * BodyNode) list
    //| Match of (Body)
    | Quote of RValue
    | Lambda of FunctionConst
    | DyLoad of string
    | FunCall
    //| MacroCall

    and BodyNode = (Pos * Body)

    and FunctionConst = (string * MataTree * BodyNode)

    type MacroConst = FunctionConst

    (*
    type MacroCall = {
        callname: string
        parameter: MataTree[]
    }
    *)

    type typeLabal =
    | Any
    | Nil
    | Int
    | UInt
    | Float
    | Bool
    | Char
    | List of typeLabal
    | Tuple of typeLabal list
    //| Struct of string
    | Function of functionTypeLabal

    and functionTypeLabal = (typeLabal list * typeLabal)

    type TopAst =
    | Nil
    | Import of string list
    | Load of string list
    | Export of string list
    | Defun of FunctionConst
    | DefMacro of MacroConst
    | TypeLabel of functionTypeLabal
    | MacroCall of (string * MataTree)

    type RootNode = {
        pos: LParserC.LParserC.Pos
        ast: TopAst
    }

    type CompilerContext = {
        a: int
    }

    (*
    // macro eval
    let pureEval (context: CompilerContext) (args: MataTree) =
        0
    *)

    type ParamerIsNotSymbol(pos: Pos) = inherit System.ApplicationException()   
    type InvalidTopLevelExpr(pos: Pos) = inherit System.ApplicationException()

    let getSymbol (args: MataTree) =
        match args.valu with
        | Symbol (s) -> s
        | _ -> raise (ParamerIsNotSymbol args.pos)

    let prec (context: CompilerContext) (args: MataTree): RootNode =
        match args.valu with
        | RValue.List ({valu=Symbol("import")}::t) -> {
            pos= args.pos;
            ast= Import ((List.map getSymbol) t)}
        | RValue.List ({valu=Symbol("load")}::t) -> {
            pos= args.pos;
            ast= Load ((List.map getSymbol) t)}
        | RValue.List ({valu=Symbol("export")}::t) -> {
            pos= args.pos;
            ast= Export ((List.map getSymbol) t)}
        // TODO:...
        | RValue.List ({valu=Symbol(macroName)}::t) -> {
            pos= args.pos;
            ast= MacroCall (macroName, {pos= args.pos; valu= RValue.List t})}
        | RValue.Nil -> {
            pos= args.pos;
            ast = Nil}
        | _ -> raise (InvalidTopLevelExpr args.pos)